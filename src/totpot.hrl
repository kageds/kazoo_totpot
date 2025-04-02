-ifndef(TOTPOT_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"totpot">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(CACHE_NAME, 'totpot_cache').

-define(CONFIG_CAT, ?APP_NAME).

-define(PRINT(Str), ?PRINT(Str, [])).
-define(PRINT(Fmt, Args), begin
                              lager:info(Fmt, Args),
                              io:format(Fmt++"\n", Args)
                          end).

-define(TOTPOT_HRL, 'true').
-endif.
