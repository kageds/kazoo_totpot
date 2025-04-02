%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_qrcodes).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/1, content_types_provided/2
        ,validate/1, validate/2
        ,validate_resource/1, validate_resource/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include_lib("crossbar/src/crossbar.hrl").

-define(CB_LIST, <<"qrcodes/crossbar_listing">>).
-define(OWNER_LIST, <<"qrcodes/listing_by_owner">>).
-define(USER_LIST, <<"users/list_by_id">>).
-define(PERIOD, 30).


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.qrcodes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.qrcodes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.qrcodes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.qrcodes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.validate_resource.qrcodes">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.execute.put.qrcodes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.get.qrcodes">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.qrcodes">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.qrcodes">>, ?MODULE, 'delete'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_Thing) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /totpot => []
%%    /totpot/foo => [<<"foo">>]
%%    /totpot/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be using to respond (matched against
%% client's accept header).
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _) ->
    Context.
%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /totpot might load a list of qrcode objects
%% /totpot/123 might load the qrcode object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, _QrCode) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_totpot(Context, cb_context:req_verb(Context)).

-spec validate_totpot(cb_context:context(), http_method()) -> cb_context:context().
validate_totpot(Context, ?HTTP_GET) ->
    load_qrcode_summary(Context);
validate_totpot(Context, ?HTTP_PUT) ->
    create(Context).


-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate_qrcode(Context, PathToken, cb_context:req_verb(Context)).

validate_qrcode(Context, QrCodeId, ?HTTP_PATCH) ->
    validate_patch(Context, QrCodeId);
validate_qrcode(Context, QrCodeId, ?HTTP_DELETE) ->
    load_qrcode(QrCodeId, Context).

validate_patch(Context, QrCodeId) ->
    crossbar_doc:patch_and_validate(QrCodeId, Context, fun validate_qrcode/2).

-spec validate_qrcode(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_qrcode(QrCodeId, Context) ->
    Routines = [
                fun check_qrcode_schema/2
               ],
    lists:foldl(fun(F, C) -> F(QrCodeId, C) end
               ,Context
               ,Routines
               ).
-spec check_qrcode_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_qrcode_schema(QrCodeId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(QrCodeId, C) end,
    cb_context:validate_request_data(<<"qrcodes">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    cb_context:set_resp_data(Context1, kz_json:set_value(<<"result">>, <<"QRCode successfully created">>, kz_json:new())).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    Nouns = cb_context:req_nouns(Context),
    [Owner_id] = props:get_value(<<"users">>, Nouns),
    JObj = kz_json:set_value(<<"owner_id">>, Owner_id, kz_json:new()),
    JObj1 = kz_json:set_value(<<"secret">>, kz_binary:rand_hex(16), JObj),
    JObj2 = kz_json:set_value(<<"enabled">>, 'false', JObj1),
    JObj3 = create_qrcode_image(Context, JObj2),
    cb_context:validate_request_data(<<"qrcodes">>, cb_context:set_req_data(Context, JObj3), OnSuccess).

create_qrcode_image(Context, JObj) ->
    ViewOptions = [{'key', kz_json:get_binary_value(<<"owner_id">>, JObj)}],
    {ok, [UserJObj]} = kz_datamgr:get_results(cb_context:account_db(Context), ?USER_LIST, ViewOptions),
    Passcode = kz_json:get_binary_value(<<"secret">>, JObj),
    UserName = kz_json:get_binary_value([<<"value">>,<<"username">>], UserJObj),
    AccountName = kzd_accounts:fetch_name(cb_context:account_id(Context)),
    Domain = <<UserName/binary, "@", AccountName/binary>>,
    PasscodeHash = crypto:hash(sha, Passcode),
    PasscodeBase32 = tpot_base32:encode(PasscodeHash),
    Period = list_to_binary(integer_to_list(?PERIOD)),
    Token = <<"otpauth://totp/", Domain/binary, "?issuer=Kazoo-UI&period=", Period/binary, "&secret=", PasscodeBase32/binary>>,
    QRCode = tpot_qrcode:encode(Token),
    Image = tpot_png:encode(QRCode),
    Filename = "/tmp/qrcode.png",
    ok = file:write_file(Filename, Image),
    kz_json:set_value(<<"image">>, base64:encode(Image), JObj).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec load_qrcode_summary(cb_context:context()) ->
          cb_context:context().
load_qrcode_summary(Context) ->
    load_qrcode_summary(Context, cb_context:req_nouns(Context)).

-spec load_qrcode_summary(cb_context:context(), req_nouns()) ->
          cb_context:context().
load_qrcode_summary(Context, [{<<"qrcodes">>, []}
                             ,{<<"users">>, [UserId]}
                              |_]
                   ) ->
    load_users_qrcode_summary(Context, UserId);
load_qrcode_summary(Context, _ReqNouns) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

-spec load_users_qrcode_summary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_users_qrcode_summary(Context, UserId) ->
    ViewOptions = [{'key', UserId}],
    crossbar_doc:load_view(?OWNER_LIST, ViewOptions, Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Load a device document from the database.
%% @end
%%------------------------------------------------------------------------------
-spec load_qrcode(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_qrcode(QRCodeId, Context) ->
    crossbar_doc:load(QRCodeId, Context,  ?TYPE_CHECK_OPTION(<<"qrcode">>)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"qrcode">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"qrcode">>)).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [remove_secret(JObj)|Acc].

remove_secret(JObj) ->
    J = kz_json:get_value(<<"value">>, JObj),
    kz_json:delete_key(<<"secret">>, J).