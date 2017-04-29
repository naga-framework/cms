-module(cms_filter_theme).
-export([before_filter/2]).

-include_lib("n2o/include/wf.hrl").

before_filter(_, #{ '_application':= A } = Ctx) -> 
 case wf:config(A, theme) of [] -> {ok, Ctx}; T -> {ok, Ctx#{ '_theme' => T }} end.