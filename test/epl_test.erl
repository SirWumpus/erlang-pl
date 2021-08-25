-module(epl_test).
-include_lib("eunit/include/eunit.hrl").

-define(PLIST1, [{a, 1}]).
-define(PLIST2, [{a, 1}, {b, [{c, [{d, 2},{e,3}]}]}]).
-define(PLIST3, [{a, 1}, {l, ["foo","bar","bat"]}]).

get_path_test_() ->
	[
	?_assertMatch(undefined, epl:get_path([], [woot])),
	?_assertMatch(undefined, epl:get_path(?PLIST1, [b])),
	?_assertMatch(undefined, epl:get_path(?PLIST2, [b,bogus,e])),
	?_assertMatch(undefined, epl:get_path(?PLIST2, [b,c,bogus])),
	?_assertMatch(undefined, epl:get_path(?PLIST3, [l,bogus])),
	?_assertMatch(undefined, epl:get_path(?PLIST3, [l,bar])),
% hmm
%	?_assertMatch(undefined, epl:get_path(?PLIST2, [b,c,d,x,y,z])),
	?_assertMatch("woot", epl:get_path(?PLIST1, [b], "woot")),
	?_assertMatch(1, epl:get_path(?PLIST1, [a])),
	?_assertMatch(1, epl:get_path(?PLIST2, [a])),
	?_assertMatch(3, epl:get_path(?PLIST2, [b,c,e])),
	?_assertMatch(["foo","bar","bat"], epl:get_path(?PLIST3, [l]))
	].

set_path_test_() ->
	[
	?_assertMatch(?PLIST1, epl:set_path([], [a], 1)),
	?_assertMatch([{a, 123}], epl:set_path(?PLIST1, [a], 123)),
	?_assertMatch([{z, "woot"}, {a, 1}], epl:set_path(?PLIST1, [z], "woot")),
	?_assertMatch([{b, [{c, [{e,"woop"},{d, 2}]}]}, {a, 1}], epl:set_path(?PLIST2, [b,c,e], "woop"))
	].

set_paths_test_() ->
	[
	?_assertMatch([{z,[{y,[{x,321}]}]},{b,[{c,[{z,123},{d,2},{e,3}]}]},{a,1}], epl:set_paths(?PLIST2, [ {[b,c,z],123}, {[z,y,x],321} ]))
	].

is_plist_test_() ->
	[
	?_assertMatch(false, epl:is_plist(atom)),
	?_assertMatch(false, epl:is_plist(123)),
	?_assertMatch(false, epl:is_plist("woot")),
	?_assertMatch(false, epl:is_plist(<<"woot">>)),
	?_assertMatch(true, epl:is_plist([])),
	?_assertMatch(false, epl:is_plist([123])),
	?_assertMatch(false, epl:is_plist([{key1, value1}, 123])),
	?_assertMatch(true, epl:is_plist([{key1, value1}, key2, {key3, value3}])),
	?_assertMatch(true, epl:is_plist([{key1, value1}, key2, {key3, [123, woot]}]))
	].
