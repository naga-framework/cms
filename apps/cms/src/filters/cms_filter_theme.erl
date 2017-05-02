-module(cms_filter_theme).
-export([before_filter/2]).

-include_lib("n2o/include/wf.hrl").

before_filter(_, #{ '_application':= A, 
                    '_controller' := Ctrl,
                    '_action'     := Act } = Ctx) -> 
 case wf:config(A, theme) of 
   [] -> {ok, Ctx}; 
   T -> {ok, Ctx#{ '_theme' => T,
                   css => css(T:css(Ctrl,Act)),
                   js  => js(T:js(Ctrl,Act)) }} end.

%% INTERNAL 
css(Vendors) -> 
  lists:foldr(fun({X,L},Acc) -> [X:vendors(css,L)|Acc];
                 ({X},Acc)   -> [X:vendors(css)|Acc]
              end, [], Vendors).

js(Vendors) -> 
  lists:foldr(fun({X,L},Acc) -> [X:vendors(js,L)|Acc];
                 ({X},Acc)   -> [X:vendors(js)|Acc]
              end, [], Vendors).