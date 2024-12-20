-module(input_server).
-behaviour(gen_server).
-author("adivanced").

%% API
-export([start_link/0, stop/0]).
%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

% Starts the input server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([]) ->
  input_loop().


input_loop() ->
  timer:sleep(100),
  io:fwrite("\nInput coords (X Y): "),
  case parse_points(string:trim(io:get_line(""))) of
    {ok, [X, Y]} ->
      gen_server:cast(interpolation_server, {interpolate, [X, Y]}),
      input_loop();
    {error, _} ->
      io:fwrite("Incorrect input. Format: X Y\n"),
      input_loop()
  end.

handle_info(_, State) ->
  {noreply, State}.


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



handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
