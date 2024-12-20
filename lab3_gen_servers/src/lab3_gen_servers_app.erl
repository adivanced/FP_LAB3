-module(lab3_gen_servers_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).
-export([
    parse_initial_values/0, get_arg_value/2, get_multi_arg/2, strings_to_atoms/1, parse_number/1
]).

start() ->
    {Freq, Window, Methods} = parse_initial_values(),
    io:fwrite("User set freq: ~p, windows: ~p, Interpolation methods: ~p~n", [
        Freq, Window, Methods
    ]),
    start(normal, {Freq, Window, Methods}).

start(_StartType, StartArgs) ->
    lab3_gen_servers_sup:start_link(StartArgs).

stop(_State) ->
    ok.

%% internal functions
parse_initial_values() ->
    Freq = get_arg_value(freq, 1),
    Window = get_arg_value(w, 4),
    Methods = strings_to_atoms(get_multi_arg(methods, [])),
    {Freq, Window, Methods}.

get_arg_value(Key, Default) ->
    case init:get_argument(Key) of
        {ok, [[Value]]} -> parse_number(Value);
        {ok, []} -> Default;
        _ -> Default
    end.

get_multi_arg(Key, Default) ->
    case init:get_argument(Key) of
        {ok, Values} -> lists:append(Values);
        _ -> Default
    end.

strings_to_atoms(StrList) ->
    lists:map(fun erlang:list_to_atom/1, StrList).

parse_number(Num) ->
    case string:to_float(Num) of
        {error, _} ->
            case string:to_integer(Num) of
                {error, _} -> io:fwrite("Argument parse error\n");
                Integer -> element(1, Integer)
            end;
        Float ->
            element(1, Float)
    end.
