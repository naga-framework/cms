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
      profile(#i{class=["fa fa-user"]})
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