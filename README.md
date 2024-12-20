# Лабораторная работа №3 (Interpolation processes)

---

* Студент: `Панин Иван Михайлович`
* Группа: `P34082`
* ИСУ: `369405`
* Язык: `Erlang`

--- 

## Требования

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

    - какие алгоритмы использовать (в том числе два сразу);
    - частота дискретизации результирующих данных;
    - и т.п.;

- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;


В рамках лабораторной работы было реализовано 2 варината:

- Со стандартными процессами в Erlang
- С использованием `OTP`

## Реализация для стандартных процессов:

#### Описание программы:

- Точка входа в программу: [main](processes/src/main.erl)
- Модуль ввода/вывода: [io_module](processes/src/io_module.erl)
- Модуль обработки интерполяций: [interoplations](processes/src/interpolations.erl)
- Линейная интерполяция: [linear_interpolation](processes/src/linear_interpolation.erl)
- Интерполяция Лагранжа: [lagrange_interpolation](processes/src/lagrange_interpolation.erl)

#### Алгоритм работы:

`data -> io_modile (input) -> interpolations -> (linear or lagrange) -> interpolations (results) -> io_module (output) -> result `

### Ключевые элементы реализации:

**Линейная интерполяция:**

````erlang
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
````

Функция start_linear создает процесс, в котором реализована сама интерполяция и отправка результатов в модуль интерполяций

**Интерполяция Лагранжа**
```erlang
start_lagrange(From, Step, Window) ->
  spawn(fun() -> lagrange_loop([], Step, Window, From) end).

lagrange_multiplier(X, Xi, Points) ->
  lists:foldl(
    fun
      ([Xj, _], Acc) when Xj =/= Xi ->
        Acc * (X - Xj) / (Xi - Xj);
      (_, Acc) ->
        Acc
    end,
    1,
    Points
  ).
lagrange_polynomial(X, Points) ->
  lists:foldl(
    fun([Xi, Yi], Acc) ->
      Acc + Yi * lagrange_multiplier(X, Xi, Points)
    end,
    0,
    Points
  ).
evaluate_lagrange(Step, Points) ->
  [X1, _] = hd(Points),
  [X2, _] = lists:last(Points),
  GeneratedDots = interpolations:points_generator(Step, X1, X2),
  [GeneratedDots, [lagrange_polynomial(X, Points) || X <- GeneratedDots]].

lagrange_loop(Points, Step, Window, From) ->
  receive
    {data, Data} ->
      UpdatedPoints =
        case length(Points) of
          Window ->
            tl(Points) ++ [Data];
          _ ->
            Points ++ [Data]
        end,

      case length(UpdatedPoints) of
        Window ->
          Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
          InterpolatedValues = evaluate_lagrange(Step, Sorted),
          From ! {ok, lagrange, InterpolatedValues};
        _ ->
          lagrange_loop(UpdatedPoints, Step, Window, From)
      end,

      lagrange_loop(UpdatedPoints, Step, Window, From);
    _ ->
      lagrange_loop(Points, Step, Window, From)
  end.
```

Та же логика, что и в линейной интерполяции

**Модуль для обработки интерполяций:**

```erlang
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
      opid ! {error, Method, "Interpolation failed"},
      interpolation_loop(Procs, InitialValues)
  end.

```

### Ввод/Вывод программы:

Найдем точки для линейной интерполяции и интерполяции лагранжа, где:

- Frequency - 0.5
- Windows - 4

Для функции ln(x):

```text
Введите точки (X Y): 1 0

Введите точки (X Y): 2 0.693

Результаты интерполяции метода (linear):
Сгенерированные точки:
   1.000    1.500    2.000    2.500 
Интерполированные значения:
   0.000    0.346    0.693    1.039 

Введите точки (X Y): 3 1.0986

Результаты интерполяции метода (linear):
Сгенерированные точки:
   2.000    2.500    3.000    3.500 
Интерполированные значения:
   0.693    0.896    1.099    1.301 

Введите точки (X Y): 4 1.386

Результаты интерполяции метода (linear):
Сгенерированные точки:
   3.000    3.500    4.000    4.500 
Интерполированные значения:
   1.099    1.242    1.386    1.530 

Результаты интерполяции метода (lagrange):
Сгенерированные точки:
   1.000    1.500    2.000    2.500    3.000    3.500    4.000    4.500 
Интерполированные значения:
   0.000    0.393    0.693    0.921    1.099    1.247    1.386    1.538 
```

## Реализация для OTP:

#### Описание программы:
- Точка входа в программу [lab3_gen_servers_app](lab3_gen_servers/src/lab3_gen_servers_app.erl)
- Сервер ввода данных: [input_server](lab3_gen_servers/src/input_server.erl)
- Сервер вывода результата: [output_server](lab3_gen_servers/src/output_server.erl)
- Сервер работы с интерполяциями [interpolation_server](lab3_gen_servers/src/interpolation_server.erl)
- Сервер для выполнения линейной интерполяции [linear_server](lab3_gen_servers/src/linear_server.erl)
- Сервер для выполнения интерполяции лагранжа [lagrange_server](lab3_gen_servers/src/lagrange_server.erl)

#### Алгоритм работы:
В целом алгоритм схожий с процессами, но поменялась логика общения между процессами, используем асинхронные вызовы casts у gen_servers, а ожидаем через handlers

### Ключевые элементы реализации:

**Линейная инерполяция**

```erlang
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Callbacks
init([]) ->
  {ok, []}.

handle_cast({linear, Input, Freq}, State) ->
  %% Add the new point to the state
  NewState =
    case length(State) of
      2 -> tl(State) ++ [Input];
      _ -> State ++ [Input]
    end,

  %% Perform interpolation if there are exactly two points
  NewState =
    case NewState of
      [P1, P2] ->
        Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, [P1, P2]),
        Result = linear_interpolation(Freq, Sorted),
        case Result of
          {ok, linear, Res} ->
            gen_server:cast(interpolation_server, {ok, linear, Res}),
            NewState;
          {error, linear, Msg} ->
            gen_server:cast(interpolation_server, {error, linear, Msg}),
            NewState
        end;
      _ ->
        NewState
    end,

  {noreply, NewState};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call(_, _From, State) ->
  {reply, ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

% Logic

linear_interpolation(Step, [[X1, Y1], [X2, Y2]]) ->
  case X1 =:= X2 of
    true ->
      {error, linear, "X values are the same, Input another X"};
    false ->
      K = (Y2 - Y1) / (X2 - X1),
      B = Y1 - K * X1,
      GenerateValues = interpolation_server:points_generator(Step, X1, X2),
      Res = [GenerateValues, lists:map(fun(X) -> K * X + B end, GenerateValues)],
      {ok, linear, Res}
  end.
```

**Интерполяция Лагранжа**

```erlang
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([]) ->
  {ok, []}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({lagrange, Input, Window, Freq}, State) ->
  UpdatedPoints =
    case length(State) of
      Window ->
        tl(State) ++ [Input];
      _ ->
        State ++ [Input]
    end,
  case length(UpdatedPoints) of
    Window ->
      Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
      InterpolatedValues = evaluate_lagrange(Freq, Sorted),
      gen_server:cast(interpolation_server, {ok, lagrange, InterpolatedValues});
    _ ->
      ok
  end,
  {noreply, UpdatedPoints}.

handle_info(_, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Lagrange multiplier
lagrange_multiplier(X, Xi, Points) ->
  lists:foldl(
    fun
      ([Xj, _], Acc) when Xj =/= Xi ->
        Acc * (X - Xj) / (Xi - Xj);
      (_, Acc) ->
        Acc
    end,
    1,
    Points
  ).

% Lagrange polynomial
lagrange_polynomial(X, Points) ->
  lists:foldl(
    fun([Xi, Yi], Acc) ->
      Acc + Yi * lagrange_multiplier(X, Xi, Points)
    end,
    0,
    Points
  ).

evaluate_lagrange(Step, Points) ->
  [X1, _] = hd(Points),
  [X2, _] = lists:last(Points),
  GeneratedDots = interpolation_server:points_generator(Step, X1, X2),
  [GeneratedDots, [lagrange_polynomial(X, Points) || X <- GeneratedDots]].
```

**Сервер для работы с интерполяциями:**

```erlang
start_link(InitialValues) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [InitialValues], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Callbacks
init([{Freq, Window}]) ->
  {ok, {Freq, Window}}.

handle_cast({interpolate, Input}, State) ->
  gen_server:cast(linear_server, {linear, Input, erlang:element(1, State)}),
  gen_server:cast(
    lagrange_server,
    {lagrange, Input, erlang:element(2, State), erlang:element(1, State)}
  ),
  {noreply, State};
handle_cast({ok, linear, Result}, State) ->
  gen_server:cast(output_server, {ok, linear, Result}),
  {noreply, State};
handle_cast({ok, lagrange, Result}, State) ->
  gen_server:cast(output_server, {ok, lagrange, Result}),
  {noreply, State};
handle_cast({error, Method, Msg}, State) ->
  gen_server:cast(output_server, {error, Method, Msg}),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call(_, _From, State) ->
  {reply, ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
```
### Ввод/Вывод программы:

Найдем точки для линейной интерполяции и интерполяции лагранжа, где:

- Frequency - 0.5
- Windows - 2

Для функции ln(x):

```text
Введите точки (X Y): 1 0

Введите точки (X Y): 2 0.693

linear интерполяция: 
Сгенерированные точки:
   1.000    1.500    2.000    2.500 
Интерполированные значения:
   0.000    0.346    0.693    1.039 

lagrange интерполяция: 
Сгенерированные точки:
   1.000    1.500    2.000    2.500 
Интерполированные значения:
   0.000    0.347    0.693    1.039 

Введите точки (X Y): 

```

## Вывод

В процессе выполнения лабораторной работы я изучил особенности работы процессов в Erlang, их взаимодействие через сообщения и реализацию ввода/вывода. Оба варианта приложений удалось реализовать успешно, однако работа с обычными процессами оказалась немного сложнее из-за необходимости вручную управлять PID-ами для передачи сообщений. В то же время, использование OTP существенно упрощает взаимодействие между процессами. Благодаря предоставляемым абстракциям, взаимодействие выглядит как вызов функций, не требующий явного управления потоками.

OTP показал себя как мощный инструмент для разработки многопроцессных приложений, предоставляя готовые механизмы, которые упрощают реализацию стандартных задач. Он позволяет сосредоточиться на логике приложения, оставляя управление процессами на уровне готовых решений. Однако использование обычных процессов дает больше возможностей для тонкой настройки, что может быть полезным при решении задач, которые не вписываются в типовые шаблоны OTP.