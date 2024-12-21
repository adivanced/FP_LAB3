-module(main).
-author("adivanced").


-export([start/0, parse_initial_values/0, strings_to_atoms/1, spawn_processes/1]).

start() ->
  process_flag(trap_exit, true),
  {Freq, Window, Methods} = main:parse_initial_values(),
  io:fwrite("User set freq: ~p, windows: ~p, Interpolation methods: ~p~n", [Freq, Window, Methods]),
  {InterpolationPid, InputPid, OutputPid} = main:spawn_processes([{Freq, Window, Methods}]),
  supervisor_loop([
    {interpolation, InterpolationPid}, {io_input, InputPid}, {io_output, OutputPid}
  ]).

supervisor_loop(ChildPids) ->
  receive
    {'EXIT', From, Reason} ->
      case Reason of
        ok ->
          ok;
        _ ->
          io:fwrite(
            "Child proc: ~p Stopped. Reason: ~p~n",
            [From, Reason]
          )
      end;
    _Other ->
      supervisor_loop(ChildPids)
  end.


parse_initial_values() ->
  Freq = get_arg_value(freq, 1),
  Window = get_arg_value(w, 3),
  Methods = strings_to_atoms(get_multi_arg(methods, [])),
  {Freq, Window, Methods}.

spawn_processes(Configurations) ->
  InterpolationPid = spawn_link(interpolations, start, Configurations),
  InputPid = spawn_link(io_module, start_input, []),
  OutputPid = spawn_link(io_module, start_output, []),
  {InterpolationPid, InputPid, OutputPid}.

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
        {error, _} -> io:fwrite("Parse error\n");
        Int -> element(1, Int)
      end;
    Float ->
      element(1, Float)
  end.