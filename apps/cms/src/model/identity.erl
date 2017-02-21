-module(identity).
-compile(export_all).


new(User) ->
  #{
   is_author => cms:is_author({user,User}), 
   is_admin  => cms:is_admin({user,User}),  
   is_blocked=> cms:is_blocked({user,User}),
   user      => cms_db:to_maps(User)
  }.