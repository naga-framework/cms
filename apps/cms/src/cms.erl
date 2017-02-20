-module(cms).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-compile(export_all).

-include_lib("kvs/include/config.hrl").
-include("cms.hrl").

main(A)    -> mad_repl:sh(A).
start(_,_) -> supervisor:start_link({local,cms }, cms,[]).
stop(_)    -> ok.
init([])   -> kvs:join(),
              naga:start([cms]), 
              naga:watch([cms,gentelella]),
              sup().
sup()      -> { ok, { { one_for_one, 5, 100 }, [] } }.

log_modules() -> [n2o_client,
                  n2o_nitrogen,
                  n2o_stream,
                  wf_convert,
                  cms_index,
                  cms_error,
                  cms,
                  cms_db].
