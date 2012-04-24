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

list_to_num(NumStr) ->
    PointIdx = string:chr(NumStr, $.),
    if PointIdx > 0 -> list_to_float(NumStr);
    true -> list_to_integer(NumStr)
    end.
