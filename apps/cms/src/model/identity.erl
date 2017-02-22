-module(identity).
-compile(export_all).

new(User) when is_tuple(User) ->
  Email = User:get(email),
  #{
   is_author => cms:is_author(Email),
is_moderator => cms:is_moderator(Email),
   is_admin  => cms:is_admin(Email),  
   is_blocked=> cms:is_blocked(Email),
   user      => User
  }.