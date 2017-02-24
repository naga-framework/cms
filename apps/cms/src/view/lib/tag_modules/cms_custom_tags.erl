-module(cms_custom_tags).
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("naga/include/naga.hrl").
-include_lib("cms/include/cms.hrl").
-include_lib("nitro/include/nitro.hrl").

% put custom tags in here, e.g.
%
reverse(Variables, Options) ->
    %io:format("Variables: ~p, Options: ~p~n", [Variables, Options] ),
    lists:reverse(binary_to_list(proplists:get_value(string, Variables))).
%
% {% reverse string="hello" %} => "olleh"
%
% Variables are the passed-in vars in your template

%------------------------------------------------------------------------------
% Contact Info Form
%------------------------------------------------------------------------------
wsContactInfo(Vars,Opts) ->
  User = proplists:get_value(user,Vars),
  wf:render(form_contact_info(User)).
form_contact_info(User) ->
  Username  = proplists:get_value(username,User,[]),
  Email     = proplists:get_value(email,User,[]),
  Firstname = proplists:get_value(firstname,User,[]),
  Lastname  = proplists:get_value(lastname,User,[]),
  #form{class=["form-horizontal form-label-left"], body=[
    #panel{class=["form-group"],body=[
      #label{class=["control-label col-md-3 col-sm-3 col-xs-12"],body=["Username"]},
      #panel{class=["col-md-9 col-sm-9 col-xs-12"], body=[
        #input{id=username, type=text, class=["form-control"], 
               value=[Username], data_fields=[{readonly,true}]}
      ]}
    ]},

    #panel{class=["form-group"],body=[
      #label{class=["control-label col-md-3 col-sm-3 col-xs-12"],body=["Firstname"]},
      #panel{class=["col-md-9 col-sm-9 col-xs-12"], body=[
        #input{id=firstname, type=text, class=["form-control"], value=[Firstname]}
      ]}
    ]},

    #panel{class=["form-group"],body=[
      #label{class=["control-label col-md-3 col-sm-3 col-xs-12"],body=["Lastname"]},
      #panel{class=["col-md-9 col-sm-9 col-xs-12"], body=[
        #input{id=lastname, type=text, class=["form-control"], value=[Lastname]}
      ]}
    ]},

    #panel{class=["form-group"],body=[
      #label{class=["control-label col-md-3 col-sm-3 col-xs-12"],body=["Email"]},
      #panel{class=["col-md-9 col-sm-9 col-xs-12"], body=[
        #input{id=email, type=text, class=["form-control"], value=[Email], data_fields=[{readonly,true}]}
      ]}
    ]},

    #panel{class=[ln_solid]},
    #panel{class=["form-group"],body=[
      #panel{class=["col-md-9 col-sm-9 col-xs-12 col-md-offset-3"], body=[
        #button{class=["btn btn-success submit"], 
               postback={update,userInfo,User}, source=[firstname,lastname],
               body=["update"]
               }
      ]}
    ]}    
  ]}.
  
%------------------------------------------------------------------------------
% Change Password Form
%------------------------------------------------------------------------------
%%TODO: client validation before sending to backend
wsChangePassword(Vars,Opts) ->
  User = proplists:get_value(user,Vars),
  wf:render(form_password(User)).
form_password(User) ->
  #form{class=["form-horizontal form-label-left"], body=[
    #panel{class=["form-group"],body=[
      #label{class=["control-label col-md-3 col-sm-3 col-xs-12"],body=["Password"]},
      #panel{class=["col-md-9 col-sm-9 col-xs-12"], body=[
        #input{id=password, type=password, class=["form-control"]}
      ]}
    ]},
    #panel{class=["form-group"],body=[
      #label{class=["control-label col-md-3 col-sm-3 col-xs-12"],body=["Confirm"]},
      #panel{class=["col-md-9 col-sm-9 col-xs-12"], body=[
        #input{id=confirm, type=password, class=["form-control"]}
      ]}
    ]},
    #panel{class=[ln_solid]},
    #panel{class=["form-group"],body=[
      #panel{class=["col-md-9 col-sm-9 col-xs-12 col-md-offset-3"], body=[
        #button{class=["btn btn-success submit"], 
               postback={password_change,User}, source=[password,confirm],
               body=["update"]
               }
      ]}
    ]}    
  ]}.
%------------------------------------------------------------------------------
% TOPNAV
%------------------------------------------------------------------------------
my_access(Vars,Opts) ->
  Access = proplists:get_value(access,Vars,[]),
  io:format("ACCESS ~p~n",[Access]),
  wf:render(
    [#h4{body=["Roles"]},
     #ul{class=["list-unstyled user_data"],body=[
      is_admin(Access),
      is_author(Access),
      is_modo(Access)
     ]}
    ]).
      
is_admin(P) -> case proplists:get_value(id_admin,P,false) of false ->[];
    true -> #li{body=[
                #button{type=button,
                        class=["btn btn-round btn-primary btn-xs"], 
                        body=["Admin"]}]} end.
is_author(P) -> case proplists:get_value(is_author,P,false) of false ->[];
    true -> #li{body=[
                #button{type=button,
                        class=["btn btn-round btn-primary btn-xs"], 
                        body=["Author"]}]} end.

is_modo(P) -> case proplists:get_value(is_moderator,P,false) of false ->[];
    true -> #li{body=[
                #button{type=button,
                        class=["btn btn-round btn-primary btn-xs"], 
                        body=["Moderator"]}]} end.


my_avatar(Vars, Opts) ->
  Avatar = proplists:get_value(avatar, Vars), 
  io:format("Avatar ~p~n",[Avatar]),
  wf:render(
    case Avatar of
      undefined ->
        #panel{id= <<"crop-avatar">>, body=[
          #link{postback={add,avatar},body=[
            #image{class=["img-responsive avatar-view"], 
                 src="/static/assets/images/image.png",
                 alt="Avatar",title="Change the avatar" }
          ]}
        ]};
      Avatar ->
        #panel{id= <<"crop-avatar">>, body=[
          #link{postback={update,avatar},body=[
            #image{class=["img-responsive avatar-view"], 
                 src="/static/assets/images/image.png",
                 alt="Avatar",title="Change the avatar" }
          ]}
        ]}
    end
  ).

%------------------------------------------------------------------------------
% TOPNAV
%------------------------------------------------------------------------------
my_top_nav(Vars, Opts) ->
 wf:render(
  #panel{class=[top_nav], body=[
    #panel{class=[nav_menu], body=[
      #nav{body=[
        toogle(),
        #ul{class=["nav navbar-nav navbar-right"], body=[
          #li{class=[], body=[
            avatar(),
            #ul{class=["dropdown-menu dropdown-usermenu pull-right"], body=[
              profile(),
              logout()
            ]},
            notifications()
          ]}
        ]}
      ]}
    ]}
  ]}).

toogle() ->
  #panel{class=["nav toggle"], body=[
    #link{id = <<"menu_toggle">>, body=[
      #i{class=["fa fa-bars"]}
    ]}
  ]}.
avatar() ->
  #link{href="javascript:;", class=["user-profile dropdown-toggle"], 
        data_fields=[{'data-toggle', dropdown},{'aria-expanded',false}],
        body=[
    #image{src="/static/gentelella/images/img.jpg"},
    "John Doe",
    #span{class=["fa fa-angle-down"]}
  ]}.
logout() ->
  #li{body=[
    #link{href="/admin/logout", body=[
      #i{class=["fa fa-sign-out pull-right"]},
      "Log Out"
    ]}
  ]}.
profile() ->
  #li{body=[
    #link{href="/admin/profile", body=["Profile"]}
  ]}.
help()->
  #li{body=[
    #link{href="/admin/help", body=["Help"]}
  ]}.

settings() ->
  #li{body=[
    #link{href="/admin/settings", body=[
      #span{class=["badge bg-red pull-right"],body=["50%"]},
      #span{body=["Settings"]}
    ]}
  ]}.

notifications() ->
  #li{class=[dropdown],data_fields=[{role,presentation}],body=[
    #link{href="javascript:;",class=["dropdown-toggle info-number"],
          data_fields=[{'data-toggle',dropdown},{'aria-expanded',false}], body=[
      #i{class=["fa fa-envelope-o"]}
      %,#span{class=["badge bg-green"], body=["6"]}
    ]},
    #ul{id= <<"menu1">>, class=["dropdown-menu list-unstyled msg_list"], 
        data_fields=[{role,menu}],
        body=[
     % msg(),
     % msg(),
     % msg(),
     % #li{body=[
     %  #panel{class=["text-center"], body=[
     %    #link{body=[
     %      #strong{body=[
     %        "See All Alerts"
     %      ]},
     %      #i{class=["fa fa-angle-right"]}
     %    ]}
     %  ]}
     % ]}

    ]}
  ]}.

msg() ->
 #li{body=[
  #link{body=[
    #span{class=[image], body=[
      #image{src="/static/gentelella/images/img.jpg",alt="Profile Image"}
    ]},
    #span{class=[time], body=["3 mins ago"]},
    #span{class=[message],body=[
      "Film festivals used to be do-or-die moments for movie makers. They were where..."
    ]}
  ]}
 ]}.

%------------------------------------------------------------------------------
% SIDEBAR MENU
%------------------------------------------------------------------------------
my_sidebar_menu(_Vars,_Opts) ->
 wf:render(
  #panel{id= <<"sidebar-menu">>, class=["main_menu_side hidden-print main_menu"], body=[
    section_general()
    %,section("PLUGINS")
  ]}).  

section_general()->
  #panel{class=[menu_section],body=[
    #h3{body=["GENERAL"]},
    #ul{class=["nav side-menu"], body=[
      dashboard(),          
      #li{body=[
        articles(),
        #ul{class=["nav child_menu"], body=[
          all_articles(),
          add_article()
        ]}        
      ]},
      comments(),
      users()
      %profile(#i{class=["fa fa-user"]})
    ]}
  ]}.
dashboard() ->
  #li{body=[
    #link{ href="/admin",body=[
      #i{class=["fa fa-dashboard"]},
      "Dashboard"
    ]}
  ]}.
articles() ->
  #link{body=[
    #i{class=["fa fa-list"]},
    "Articles"
  ]}.
all_articles() ->
  #li{body=[
    #link{href="/admin/articles",body=["All Articles"]}
  ]}.
add_article() ->
  #li{body=[
    #link{href="/admin/article/add",body=["Add New"]}
  ]}.
comments() ->
  #li{body=[
    #link{ href="/admin/comments",body=[
      #i{class=["fa fa-comments"]},
      "Comments"
    ]}
  ]}.

users() ->
  #li{body=[
    #link{ href="/admin/users",body=[
      #i{class=["fa fa-users"]},
      "Users"
    ]}
  ]}.

profile(Icon) ->
  #li{body=[
    #link{ href="/admin/profile",body=[
      Icon,
      "Profile"
    ]}
  ]}.

section(Title)->
  #panel{class=[menu_section],body=[
    #h3{body=[Title]},
    #ul{class=["nav side-menu"], body=[
      
    ]}
  ]}.