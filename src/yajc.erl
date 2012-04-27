-module(yajc).

-export([to_erlang/1, to_json/1]).

to_erlang(J) ->
    {ok,T,_} = json_lex:string(J),
    {ok, P} = json_parse:parse(T),
    P.

to_json(J) -> to_json_(J,[]).

to_json_({N,V}) when is_integer(V) -> "\"" ++ atom_to_list(N) ++ "\":" ++ integer_to_list(V);
to_json_({N,V}) when is_float(V)   -> "\"" ++ atom_to_list(N) ++ "\":" ++ float_to_list(V);
to_json_({N,V}) when is_binary(V)  -> "\"" ++ atom_to_list(N) ++ "\":\"" ++ binary_to_list(V) ++ "\"";
to_json_({N,V})                    -> "\"" ++ atom_to_list(N) ++ "\":" ++ to_json_(V, []);
to_json_(V) when is_binary(V)      -> "\"" ++ binary_to_list(V) ++ "\"";
to_json_(V) when is_integer(V)     -> integer_to_list(V);
to_json_(V) when is_float(V)       -> float_to_list(V);
to_json_(V)                        -> to_json_(V, []).

to_json_([E|_] = O, J) when is_tuple(E)     -> to_json_(O, J, object);
to_json_(A, J)                              -> to_json_(A, J, array).

to_json_([], J, array)  -> "[" ++ J ++ "]";
to_json_([], J, object) -> "{" ++ J ++ "}";
to_json_([E|R], [], T)  -> to_json_(R, to_json_(E), T);
to_json_([E|R], J, T)   -> to_json_(R, J ++ "," ++ to_json_(E), T).

%
%% TESTs
%

-include_lib("eunit/include/eunit.hrl").

-define(TEST_JSON,[
        {"{\"a\":\"b\"}", [{a,<<"b">>}]}
        ,{"[10,1.05000000000000000000e+001,\"bikram\"]", [10,1.05000000000000000000e+001,<<"bikram">>]}
        ,{"[10,1.05000000000000000000e+001,\"bikram\",{\"a\":10}]", [10,1.05000000000000000000e+001,<<"bikram">>,[{a,10}]]}
        ,{"{\"a\":{\"b\":1,\"c\":10,\"e\":1.05000000000000000000e+001,\"f\":\"bikram\"}}", [{a,[{b,1},{c,10},{e,1.05000000000000000000e+001},{f,<<"bikram">>}]}]}
        ,{"{\"a\":
            {\"b\":
                [{\"c\":10}
                 ,{\"e\":1.05000000000000000000e+001}
                 ,{\"f\":\"bikram\"}]
            }
           }", [{a,[{b,[[{c,10}],[{e,1.05000000000000000000e+001}],[{f,<<"bikram">>}]]}]}]}
    ]).

parser_test() -> run_parse(?TEST_JSON).

encode_decode_test() -> run_ed(?TEST_JSON).

run_parse([]) -> ok;
run_parse([{J,E}|Jsons]) ->
    ?assertMatch(E, to_erlang(J)),
    run_parse(Jsons).

run_ed([]) -> ok;
run_ed([{J,_}|Jsons]) ->
    J1 = re:replace(J, "[ \r\n]+", "", [{return, list}, global]),
    ?assertMatch(J1, to_json(to_erlang(J))),
    run_ed(Jsons).
