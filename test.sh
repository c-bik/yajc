#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -eval 'eunit:test(yajc, [verbose])'
