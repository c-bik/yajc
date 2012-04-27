Definitions.

D = [0-9]
S = (\+|\-)?
H = [a-zA-Z0-9]
Spl = (\\((u{H}{4})|([\"trf\bn\/])))
Rules.

{S}{D}+                         : {token,{'NUMBER',TokenLine,list_to_integer(TokenChars)}}.
{S}{D}+\.{D}+((E|e){S}{D}+)?    : {token,{'NUMBER',TokenLine,list_to_float(TokenChars)}}.
"(([^\\\"])|{Spl})*"            : {token,{'STRING',TokenLine,strip_quotes(TokenChars)}}.
[\{\}\[\]\,\:]                  : {token, {list_to_atom(TokenChars), TokenLine}}.
('true'|'false'|'null')         : {token, {list_to_atom(TokenChars), TokenLine}}.
([\s\t\r\n]+)                   : skip_token.

Erlang code.

strip_quotes(StrChars) -> list_to_binary(string:substr(StrChars, 2, string:len(StrChars) - 2)).

% Lexer test
-include_lib("eunit/include/eunit.hrl").

lexer_number_test() ->
    ?assertMatch({ok, [{'NUMBER', _, 10}], _}, string("10")),
    ?assertMatch({ok, [{'NUMBER', _, -10}], _}, string("-10")),
    ?assertMatch({ok, [{'NUMBER', _, 10.1}], _}, string("10.1")),
    ?assertMatch({ok, [{'NUMBER', _, -10.1}], _}, string("-10.1")),
    ?assertMatch({ok, [{'NUMBER', _, 1010.0}], _}, string("10.1e2")),
    ?assertMatch({ok, [{'NUMBER', _, -1010.0}], _}, string("-10.1e2")),
    ?assertMatch({ok, [{'NUMBER', _, 1.01e11}], _}, string("10.1e10")),
    ?assertMatch({ok, [{'NUMBER', _, -1.01e11}], _}, string("-10.1e10")),
    ?assertMatch({ok, [{'NUMBER', _, 1.01e11}], _}, string("+10.1e10")).

lexer_string_test() ->
    ?assertMatch({ok, [{'STRING', _, <<"abc">>}], _}, string("\"abc\"")),
    ?assertMatch({error,{_,_,{illegal,_}},_}, string("\"abc \\u1234 \"")).

lexer_obj_test() ->
    ?assertMatch({ok, [{'{',_},{'STRING',_,<<"a">>},{':',_},{'STRING',_,<<"b">>},{'}',_}], _}, string("{\"a\":\"b\"}")).
