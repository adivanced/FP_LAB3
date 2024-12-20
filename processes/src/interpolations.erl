-module(interpolations).
-author("adivanced").

%% API
-export([start/1, init/1, interpolation_loop/2, points_generator/3]).

start(InitialValues) ->
  register(interpolationpid, self()),
  init(InitialValues).


init(InitialValues) ->
  Methods = element(3, InitialValues),

  Procs = lists:map(
    fun(X) ->
      case X of
        linear ->
          Pid = linear_interpolation:start_linear(element(1, InitialValues), self()),
          {Pid, linear};
        lagrange ->
          Pid = lagrange_interpolation:start_lagrange(
            self(), element(1, InitialValues), element(2, InitialValues)
          ),
          {Pid, lagrange};
        Method ->
          io:fwrite("Unknown method: ~p, SKIPPED~n", [Method]),
          undefined
      end
    end,
    Methods
  ),

  ValidProcs = lists:filter(fun(X) -> X =/= undefined end, Procs),
  interpolation_loop(ValidProcs, InitialValues).


interpolation_loop(Procs, InitialValues) ->
  receive

    {interpolate, Data} ->
      [Pid ! {data, Data} || {Pid, _} <- Procs],
      interpolation_loop(Procs, InitialValues);


    {ok, Method, Result} ->
      opid ! {ok, Method, Result},
      interpolation_loop(Procs, InitialValues);


    {error, Method} ->
      opid ! {error, Method, "interpolation failed!"},
      interpolation_loop(Procs, InitialValues)
  end.


points_generator(Step, X, Y) when Step > 0, X - Step =< Y ->
  [X | points_generator(Step, X + Step, Y)];
points_generator(_, _, _) ->
  [].

