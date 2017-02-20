-module(cms_custom_filters).
-compile(export_all).

% put custom filters in here, e.g.
%
my_reverse(Value) ->
     list_to_binary(lists:reverse(binary_to_list(Value))).
%
% "foo"|my_reverse   => "oof"