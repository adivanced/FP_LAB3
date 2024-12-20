clear
rebar3 compile
cd _build/default/lib/lab3_gen_servers/ebin
erl -noshell -s lab3_gen_servers_app start -freq 0.5 -w 2 -methods linear lagrange -s init stop
