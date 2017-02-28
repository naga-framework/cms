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
-include_lib("kvs/include/feed.hrl").
%--------------------------------------------------------------------------------
% INDEX
%--------------------------------------------------------------------------------
index(<<"GET">>, _, #{identity:=Identity} = Ctx) -> 
  io:format("Identity ~p~n",[Identity]),
  VendorsCSS = [bootstrap3,fontawesome,
                nprogress,icheck,datatables,
                pnotify,gentelella],              
  VendorsJS = [jquery,bootstrap3,fastclick,nprogress,icheck,
               datatables,jszip,pdfmake,pnotify,starrr,gentelella],
  Bindings = cms_lib:bindings(Identity,VendorsCSS,VendorsJS),

  Users = kvs:entries(kvs:get(feed,xuser), xuser, undefined),

  {ok, Bindings ++ [{users, Users}]}.
%--------------------------------------------------------------------------------
% PROFILE
%--------------------------------------------------------------------------------
profile(<<"GET">>, _, #{identity:=Identity} = Ctx) -> 
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
      

seek(Type, FeedId, {Page,PageSize}) when Page > 0, PageSize > 0 ->
  case kvs:get(feed, FeedId) of 
    {ok, Feed} -> seek(Type, FeedId, Feed, PageSize, (Page-1) * PageSize, Page* PageSize);
    _ -> [] end.

seek(Type, _, #feed{count=Total} = Feed, PageSize, Start, ___) when Start >= Total -> [];
seek(Type, _, #feed{count=Total} = Feed, PageSize, 0, End) ->
     kvs:entries(Feed, Type, PageSize);
seek(Type, _, #feed{count=Total} = Feed, PageSize, Start, End) ->
     %wf:info(?MODULE, "PageSize ~p, Start ~p, End ~p~n",[PageSize, Start, End]),
     L = kvs:entries(Feed, Type, Start+1),
     %wf:info(?MODULE, "L ~p~n",[L]),
     First = lists:nth(1,L),
     %wf:info(?MODULE, "First ~p~n",[First]),
     kvs:entries(Feed#feed{top=element(2,First)}, Type, PageSize).

%--------------------------------------------------------------------------------
% ACCESS
%--------------------------------------------------------------------------------      
%%TODO when session expire lock the user, ask him to login 
access(undefined,User)         -> false;
access(#{is_admin:=true},User) -> true;
access(#{is_admin:=false, user:=U},User) -> 
  Id1 = U:get(id),
  Id2 = proplists:get_value(id,User), 
  Id1 =:= Id2 andalso Id1 /= undefined andalso Id2 /= undefined.

update_identity(#{user:=U}=Identity,New) ->
  case U:get(id) == New:get(id) of false -> ok;
    true -> wf:user(Identity#{user=>New}) end.

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
             case {Firstname,Lastname} of
                {<<>>,<<>>} -> cms_lib:notify(error,"error udpate profile", "empty fields"); 
                _ ->  U1 = U:set(firstname,Firstname),
                      U2 = U1:set(lastname,Lastname),
                      case catch U2:save() of
                        {ok,New} -> update_identity(Identity,New), 
                          cms_lib:notify(success,"Udpate profile", "profile updated.");
                        Err -> cms_lib:notify(error,"Error udpate profile", wf:to_list(Err) )
                      end
             end;
    false -> cms_lib:notify(error,"Udpate profile", "not authorized.")
  end;


event({change,password,User}) ->
  Identity = wf:user(),
  case access(Identity,User) of
    true ->  io:format("USER ~p",[User]),
             Password = wf:q(password),
             Confirm = wf:q(confirm),
             case Password =:= Confirm of
              true -> Id = proplists:get_value(id,User),
                      {ok,U} = xuser:get(Id),
                      case catch U:update_password(Password) of
                        {ok,New} -> update_identity(Identity,New),  
                          cms_lib:notify(success,"Change password", "password updated.");
                        Err -> cms_lib:notify(error,"Error change password", wf:to_list(Err))
                      end;
              false-> cms_lib:notify(error,"Change password", "doesn't match.")
             end;
             
    false -> cms_lib:notify(error,"Udpate password", "not authorized.")
  end;

event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
