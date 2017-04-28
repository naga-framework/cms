-module(cms_lib).

-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

redirect(Sec,Redirect) ->
  wf:wire(wf:f("setTimeout(function(){window.location='~s';}, ~B);",
          [Redirect,1000*Sec])). 

refresh() -> wf:wire("window.location=window.location;"). 

css(Vendors) -> 
  lists:foldr(fun({X,L},Acc) -> [X:vendors(css,L)|Acc];
                 ({X},Acc)   -> [X:vendors(css)|Acc]
              end, [], Vendors).

js(Vendors) -> 
  lists:foldr(fun({X,L},Acc) -> [X:vendors(js,L)|Acc];
                 ({X},Acc)   -> [X:vendors(js)|Acc]
              end, [], Vendors).

bindings(VendorsCSS, VendorsJS) ->               
  CSS = css(VendorsCSS),
  JS  = js(VendorsJS),
   [
    {page,[{css,CSS},
           {title,"Blog Demo"},
           {js,JS}]}
   ].