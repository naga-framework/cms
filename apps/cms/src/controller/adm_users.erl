-module(adm_users).
-export([index/3, 
         profile/3,
         event/1
        ]).
-default_action(index).
-actions([index,profile]).

-include_lib("n2o/include/wf.hrl").
-include_lib("naga/include/naga.hrl").
-include_lib("cms/include/cms.hrl").

%--------------------------------------------------------------------------------
% INDEX
%--------------------------------------------------------------------------------
index(<<"GET">>, _, _)   -> 
  {501, <<"not implemented">>, []}.

%--------------------------------------------------------------------------------
% PROFILE
%--------------------------------------------------------------------------------
profile(<<"GET">>, _, #{identity:=Identity} = Ctx)   -> 

  io:format("Identity ~p~n",[Identity]),
  VendorsCSS = [bootstrap3,fontawesome,
                nprogress,icheck,prettify,select2,switchery,starrr,
                pnotify,gentelella],
  VendorsJS = [jquery,bootstrap3,fastclick,progressbar,
                nprogress,raphael,morris,icheck,moment,
                daterangepicker,wysiwyg,hotkeys,prettify,
                tagsinput,switchery,select2,parsley,autosize,
                autocomplete,pnotify,starrr,gentelella],
  Bindings = cms_lib:bindings(Identity,VendorsCSS,VendorsJS),

  {ok, Bindings}.
      

%--------------------------------------------------------------------------------
% ACCESS
%--------------------------------------------------------------------------------      
access(#{is_admin:=true},User) -> true;
access(#{is_admin:=false, user:=U},User) -> 
  Id1 = U:get(id),
  Id2 = proplists:get_value(id,User), 
  Id1 =:= Id2 andalso Id1 /= undefined andalso Id2 /= undefined.

%--------------------------------------------------------------------------------
% EVENT HANDLING
%--------------------------------------------------------------------------------      
event({update,userInfo,User}) -> 
  Identity = wf:user(),
  case access(Identity,User) of
    true ->  Id = proplists:get_value(id,User),
             Firstname = wf:q(firstname),
             Lastname = wf:q(lastname), 
             {ok,U} = xuser:get(Id),
             U1 = U:set(firstname,Firstname),
             U2 = U1:set(lastname,Lastname),
             case U2:save() of
              {ok,_} -> cms_lib:notify(success,"Udpate profile", "done.");
              _ -> cms_lib:notify(success,"Udpate profile", "done.")
             end;
    false -> cms_lib:notify(error,"Udpate profile", "not authorized.")
  end;


event({change,password,User}) -> 
  io:format("USER ~p",[User]),
  Password = wf:q(password),
  Confirm = wf:q(confirm),
  #{user := U} = wf:user(), 
  cms_lib:notify(error,"Change password", "not yet implemented.");


event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
