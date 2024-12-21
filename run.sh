clear
rebar3 compile
cd _build/default/lib/processes/ebin
erl -noshell -s main start -freq 0.5 -w 4 -methods linear lagrange -s init stop
