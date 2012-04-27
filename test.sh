#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -eval 'eunit:test(json_lex, [verbose]),eunit:test(yajc, [verbose])'
