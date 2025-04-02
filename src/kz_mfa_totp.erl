%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc Kazoo multi factor authentication using HOTP (RFC 4226)
%%%      compatible with google authenticator mobile app
%%% @author Hesaam Farhang
%%% @author Alan R. Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_mfa_totp).

-export([authenticate/2, totp/2]).

-include("../../core/kazoo_auth/src/kazoo_auth.hrl").

-define(PERIOD, 30).
-define(OWNER_LIST, <<"qrcodes/listing_by_owner">>).


%%------------------------------------------------------------------------------
%% @doc Takes the configuration and authenticate the `Claims'.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(kz_term:proplist(), kz_json:object()) -> mfa_result().
authenticate(Claims, _JObj) ->
    case props:get_ne_binary_value(<<"mfa_resp">>, Claims)
    of
        'undefined' -> maybe_mfa_request(Claims);
        Mfa_resp -> check_mfa_resp(Claims, Mfa_resp)
    end.

maybe_mfa_request(Claims) ->
    {ok, JObjs} = get_qrcode_info(Claims),
    maybe_mfa_request(Claims, JObjs).

% No QRCode exits for Account/User
maybe_mfa_request(_Claims, []) ->
    {'ok', 'authenticated'};
% One QRCode exists for Account/User
maybe_mfa_request(_Claims, [JObj]) ->
    case kz_json:get_boolean_value([<<"value">>,<<"enabled">>], JObj) of
        'true' ->
            Resp = [{<<"provider_name">>, <<"totp">>}
                    ,{<<"settings">>
                    ,kz_json:from_list([])
                    }
                   ],
            {'error', 401, kz_json:from_list(Resp)};
        'false' ->
            {'ok', 'authenticated'}
    end;
%Multiple QRCodes exist for Account/User, not handled
maybe_mfa_request(_Claims, _JObjs) ->
    Resp = [{<<"provider_name">>, <<"totp">>}
        ,{<<"settings">>
        ,kz_json:from_list([])
        }
       ],
    {'error', 401, kz_json:from_list(Resp)}.

            
check_mfa_resp(Claims, Mfa_resp) ->
    {ok, JObjs} = get_qrcode_info(Claims),
    Passcodes = totp(JObjs),
    case lists:any(fun(Passcode) -> Passcode == Mfa_resp end, Passcodes) of
        'true' -> {'ok', 'authenticated'};
        'false' -> {'error', 401, kz_json:from_list([{<<"error">>, <<"invalid mfa passcode">>}])}
    end.

totp([JObj]) ->
        Secret = kz_json:get_binary_value([<<"value">>,<<"secret">>], JObj),
        Key = crypto:hash(sha, Secret),
        totp(Key, ?PERIOD).

-spec totp(kz_term:binary(), integer()) -> list().
totp(Key, Period) ->
        T = unow() div Period,
        [hotp(Key, T - 1), hotp(Key, T), hotp(Key, T + 1)].
%% RFC-4226 "HOTP: An HMAC-Based One-Time Password Algorithm"
%% @ref <http://tools.ietf.org/html/rfc4226>
hotp(Key, Count) when is_binary(Key), is_integer(Count) ->
        HS = crypto:hmac(sha, Key, <<Count:64>>),
        <<_:19/binary, _:4, Offset:4>> = HS,
        <<_:Offset/binary, _:1, P:31, _/binary>> = HS,
        HOTP = integer_to_list(P rem 1000000),
        Pad = lists:duplicate(6 - length(HOTP), $0),
        list_to_binary([Pad, HOTP]).


-define(UNIX_TIME_ZERO, 62167219200).

unow() ->
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?UNIX_TIME_ZERO.

get_qrcode_info(Claims) ->
        UserId = props:get_value(<<"owner_id">>, Claims),
        AccountId = props:get_value(<<"account_id">>, Claims),
        AccountDb = kz_util:format_account_db(AccountId),
        ViewOptions = [{'key', UserId}],
        kz_datamgr:get_results(AccountDb, ?OWNER_LIST, ViewOptions).