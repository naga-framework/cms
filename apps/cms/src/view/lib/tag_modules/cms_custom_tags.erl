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
      #li{body=[
        #link{ href="/admin",body=[
          #i{class=["fa fa-dashboard"]},
          "Dashboard"
        ]}
      ]},          
      #li{body=[
        #link{body=[
          #i{class=["fa fa-list"]},
          "Articles"
        ]},
        #ul{class=["nav child_menu"], body=[
          #li{body=[
            #link{href="/admin/articles",body=["All Articles"]}
          ]},
          #li{body=[
            #link{href="/admin/article/add",body=["Add New"]}
          ]}
        ]}        
      ]},
      #li{body=[
        #link{ href="/admin/comments",body=[
          #i{class=["fa fa-comments"]},
          "Comments"
        ]}
      ]},
      #li{body=[
        #link{ href="/admin/profile",body=[
          #i{class=["fa fa-user"]},
          "Profile"
        ]}
      ]}    
    ]}
  ]}.

section(Title)->
  #panel{class=[menu_section],body=[
    #h3{body=[Title]},
    #ul{class=["nav side-menu"], body=[
      
    ]}
  ]}.