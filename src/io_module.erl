-module(io_module).
-author("adivanced").

%% API
-export([start_input/0, start_output/0]).

start_input() ->
  input_loop().

input_loop() ->
  receive
  after 100 ->
    io:fwrite("\nInput coords (X Y): "),
    case parse_points(string:trim(io:get_line(""))) of
      {ok, [X, Y]} ->
        interpolationpid ! {interpolate, [X, Y]},
        input_loop();
      {error, _} ->
        io:fwrite("Incorrect input. Format: X Y\n"),
        input_loop()
    end
  end.

parse_points(Line) ->
  Tokens = string:tokens(Line, " "),
  Parsed = [parse_number(Num) || Num <- Tokens],
  Filtered = lists:filter(fun is_number/1, Parsed),
  case Filtered of
    [X, Y] -> {ok, [X, Y]};
    _ -> {error, invalid_input}
  end.

parse_number(Value) ->
  case string:to_float(Value) of
    {error, _} ->
      case string:to_integer(Value) of
        {error, _} ->
          invalid;
        Int -> element(1, Int)
      end;
    Float ->
      element(1, Float)
  end.

start_output() ->
  register(opid, self()),
  output_loop().

output_loop() ->
  receive
    {ok, Method, Result} when Method =:= linear; Method =:= lagrange ->
      io:fwrite("\nResults of method interpolation (~s):\n", [erlang:atom_to_list(Method)]),
      print_result(Result),
      output_loop();
    {error, Method, Msg} ->
      io:fwrite("\e[31mError in method ~s: ~s\e[0m\n",
        [erlang:atom_to_list(Method), Msg]),
      output_loop()
  end.

print_result([GeneratedDots, InterpolatedValues]) ->
  io:fwrite("Dots generated:\n"),
  lists:foreach(
    fun(Dot) -> io:fwrite("~8.3f ", [float(Dot)]) end,
    GeneratedDots
  ),
  io:fwrite("\nInterpolated values:\n"),
  lists:foreach(
    fun(Value) -> io:fwrite("~8.3f ", [float(round_to(Value, 3))]) end,
    InterpolatedValues
  ),
  io:fwrite("\n").

round_to(Number, Digits) ->
  Factor = math:pow(10, Digits),
  erlang:round(Number * Factor) / Factor.
