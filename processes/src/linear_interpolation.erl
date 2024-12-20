-module(linear_interpolation).
-author("adivanced").

%% API
-export([start_linear/2, linear_loop/3, linear_interpolation/2]).

start_linear(Step, From) ->
  spawn(fun() -> linear_loop([], Step, From) end).

linear_interpolation(Step, [[X1, Y1], [X2, Y2]]) ->
  case X1 =:= X2 of
    true ->
      {error, linear};
    false ->
      K = (Y2 - Y1) / (X2 - X1),
      B = Y1 - K * X1,
      GenerateValues = interpolations:points_generator(Step, X1, X2),
      Res = [GenerateValues, lists:map(fun(X) -> K * X + B end, GenerateValues)],
      {ok, linear, Res}
  end.

linear_loop(Points, Step, From) ->
  receive
    {data, Data} ->
      UpdatedPoints =
        case Points of
          [_ | Rest] when length(Points) == 2 -> Rest ++ [Data];
          _ -> Points ++ [Data]
        end,
      case length(UpdatedPoints) of
        2 ->
          % [P1, P2] = UpdatedPoints,
          Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
          Result = linear_interpolation(Step, Sorted),
          case Result of
            {ok, linear, Res} -> From ! {ok, linear, Res};
            _ -> From ! {error, linear}
          end,
          linear_loop(UpdatedPoints, Step, From);
        _ ->
          linear_loop(UpdatedPoints, Step, From)
      end
  end.