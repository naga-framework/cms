-module(cms_lib).

-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

redirect(Sec,Redirect) ->
  wf:wire(wf:f("setTimeout(function(){window.location='~s';}, ~B);",
          [Redirect,1000*Sec])). 

refresh() -> wf:wire("window.location=window.location;").
