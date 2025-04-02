%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%% @author Alan R. Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(totpot_handlers).

-export([
         handle_config_change/2
        ]).

-include("totpot.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").


-spec handle_config_change(kz_json:object(), kz_term:proplist()) -> any().
handle_config_change(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    handle_qrcode_change(kz_json:get_value(<<"Account-ID">>, JObj)
                           ,kz_json:get_value(<<"User-ID">>, JObj)
                           ,kz_json:get_value(<<"Event-Name">>, JObj)
                           ).

handle_qrcode_change(AccountId, UserId, ?DOC_CREATED) ->
    lager:debug("maybe creating a qrcode for ~s: ~s", [AccountId, UserId]);
handle_qrcode_change(AccountId, UserId, ?DOC_DELETED) ->
    lager:debug("maybe deleting a qrcode for ~s: ~s", [AccountId, UserId]).
