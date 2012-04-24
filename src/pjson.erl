-module(yajc).

-export([j2e/1, e2j/1]).

j2e(J) ->
    {ok,T,_} = json_lex:string(J),
    {ok, P} = json_parse:parse(T),
    P.

e2j(J) -> e2j_(J,[]).

e2j_({N,V}) when is_integer(V) -> "\"" ++ atom_to_list(N) ++ "\":" ++ integer_to_list(V);
e2j_({N,V}) when is_float(V)   -> "\"" ++ atom_to_list(N) ++ "\":" ++ float_to_list(V);
e2j_({N,V}) when is_binary(V)  -> "\"" ++ atom_to_list(N) ++ "\":\"" ++ binary_to_list(V) ++ "\"";
e2j_({N,V})                    -> "\"" ++ atom_to_list(N) ++ "\":" ++ e2j_(V, []);
e2j_(V) when is_binary(V)      -> "\"" ++ binary_to_list(V) ++ "\"";
e2j_(V) when is_integer(V)     -> integer_to_list(V);
e2j_(V) when is_float(V)       -> float_to_list(V);
e2j_(V)                        -> e2j_(V, []).

e2j_([E|_] = O, J) when is_tuple(E)     -> e2j_(O, J, object);
e2j_(A, J)                              -> e2j_(A, J, array).

e2j_([], J, array)  -> "[" ++ J ++ "]";
e2j_([], J, object) -> "{" ++ J ++ "}";
e2j_([E|R], [], T)  -> e2j_(R, e2j_(E), T);
e2j_([E|R], J, T)   -> e2j_(R, J ++ "," ++ e2j_(E), T).

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

% Lexer test
lexer_number_test() ->
    ?assertMatch({ok, [{'NUMBER', _, 10}], _}, json_lex:string("10")),
    ?assertMatch({ok, [{'NUMBER', _, -10}], _}, json_lex:string("-10")),
    ?assertMatch({ok, [{'NUMBER', _, 10.1}], _}, json_lex:string("10.1")),
    ?assertMatch({ok, [{'NUMBER', _, -10.1}], _}, json_lex:string("-10.1")),
    ?assertMatch({ok, [{'NUMBER', _, 1010.0}], _}, json_lex:string("10.1e2")),
    ?assertMatch({ok, [{'NUMBER', _, -1010.0}], _}, json_lex:string("-10.1e2")),
    ?assertMatch({ok, [{'NUMBER', _, 1.01e11}], _}, json_lex:string("10.1e10")),
    ?assertMatch({ok, [{'NUMBER', _, -1.01e11}], _}, json_lex:string("-10.1e10")),
    ?assertMatch({ok, [{'NUMBER', _, 1.01e11}], _}, json_lex:string("+10.1e10")).

lexer_string_test() ->
    ?assertMatch({ok, [{'STRING', _, <<"abc">>}], _}, json_lex:string("\"abc\"")),
    ?assertMatch({error,{_,_,{illegal,_}},_}, json_lex:string("\"abc \\u1234 \"")).

lexer_obj_test() ->
    ?assertMatch({ok, [{'{',_},{'STRING',_,<<"a">>},{':',_},{'STRING',_,<<"b">>},{'}',_}], _}, json_lex:string("{\"a\":\"b\"}")).

parser_test() -> run_parse(?TEST_JSON).

encode_decode_test() -> run_ed(?TEST_JSON).

run_parse([]) -> ok;
run_parse([{J,E}|Jsons]) ->
    ?assertMatch(E, j2e(J)),
    run_parse(Jsons).

run_ed([]) -> ok;
run_ed([{J,_}|Jsons]) ->
    J1 = re:replace(J, "[ \r\n]+", "", [{return, list}, global]),
    ?assertMatch(J1, e2j(j2e(J))),
    run_ed(Jsons).
