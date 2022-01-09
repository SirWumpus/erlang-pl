-module(epl_test).
-include_lib("eunit/include/eunit.hrl").

-define(PLIST1, [{a, 1}]).
-define(PLIST2, [{a, 1}, {b, [{c, [{d, 2},{e,3}]}]}]).
-define(PLIST3, [{a, 1}, {l, ["foo","bar","bat"]}]).
-define(PLIST4, [{a, 1}, {s, "foo"}]).
-define(PLIST5, [{a, 0}, {l, ["foo",?PLIST2,"bat"]}]).

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
	?_assertMatch(?PLIST1, epl:get_path(?PLIST1, [])),
	?_assertMatch("woot", epl:get_path(?PLIST1, [b], "woot")),
	?_assertMatch(1, epl:get_path(?PLIST1, [a])),
	?_assertMatch(1, epl:get_path(?PLIST2, [a])),
	?_assertMatch(3, epl:get_path(?PLIST2, [b,c,e])),
	?_assertMatch(["foo","bar","bat"], epl:get_path(?PLIST3, [l])),
	?_assertMatch("bar", epl:get_path(?PLIST3, [l, 2])),
	?_assertMatch(3, epl:get_path(?PLIST5, [l, 2, b, c, e]))
	].

set_path_test_() ->
	[
	?_assertMatch(?PLIST1, epl:set_path([], [a], 1)),
	?_assertMatch([{a, [{b, [{c, 3}]}]}], epl:set_path([], [a, b, c], 3)),
	?_assertMatch([{a, 123}], epl:set_path(?PLIST1, [a], 123)),
	?_assertMatch([{z, "woot"}, {a, 1}], epl:set_path(?PLIST1, [z], "woot")),
	?_assertMatch([{b, [{c, [{e,"woop"},{d, 2}]}]}, {a, 1}], epl:set_path(?PLIST2, [b,c,e], "woop")),
	?_assertMatch([{l, ["foo", 999, "bat"]}, {a, 1}], epl:set_path(?PLIST3, [l, 2], 999)),
	?_assertMatch([{l, ["foo", [{b, [{c, [{e, 999}, {d,2}]}]}, {a, 1}], "bat"]}, {a, 0}], epl:set_path(?PLIST5, [l, 2, b, c, e], 999))
	].

set_paths_test_() ->
	[
	?_assertMatch([{z,[{y,[{x,321}]}]},{b,[{c,[{z,123},{d,2},{e,3}]}]},{a,1}], epl:set_paths(?PLIST2, [ {[b,c,z],123}, {[z,y,x],321} ]))
	].

is_plist_tuple_test_() ->
	[
	?_assertMatch(true, epl:is_plist_tuple(ok)),
	?_assertMatch(true, epl:is_plist_tuple({a, "woot"})),
	?_assertMatch(true, epl:is_plist_tuple({a, {b, "woot"}})),
	?_assertMatch(false, epl:is_plist_tuple(123)),
	?_assertMatch(false, epl:is_plist_tuple([])),
	?_assertMatch(false, epl:is_plist_tuple({})),
	?_assertMatch(false, epl:is_plist_tuple(#{})),
	?_assertMatch(false, epl:is_plist_tuple("abc")),
	?_assertMatch(false, epl:is_plist_tuple([a, b, c])),
	?_assertMatch(false, epl:is_plist_tuple({a, "woot", abc}))
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

to_map_test_() ->
	[
	?_assertMatch(#{}, epl:to_map([])),
	?_assertMatch(#{a := 1}, epl:to_map(?PLIST1)),
	?_assertMatch(#{a := 1, b := #{c := #{d := 2, e := 3}}}, epl:to_map(?PLIST2)),
	?_assertMatch(#{a := 1, l := ["foo","bar","bat"]}, epl:to_map(?PLIST3)),
	?_assertMatch(#{a := 1, s := "foo"}, epl:to_map(?PLIST4))
	].

from_map_test_() ->
	[
	?_assertMatch([], epl:from_map(#{})),
	?_assertMatch([{d,[3,4]},{b,[{c,2}]},{a,1}], epl:from_map(#{a => 1, b => #{c => 2}, d => [3, 4]}))
	].
