-module(main@foreign).
-export(['_putVar'/0, '_takeVar'/0, makeEmptyVar/0]).

'_putVar'() ->
    fun(Util, Value, AVar, CB) ->
        fun() ->
            Result = rpc(AVar, {put, Util, Value, CB}),
            io:format("['_putVar'] rpc: ~p~n", [Result]),
            Result()
        end
    end.

'_takeVar'() ->
    fun(Util, AVar, CB) ->
        fun() ->
            Result = rpc(AVar, {take, Util, CB}),
            io:format("['_takeVar'] rpc: ~p~n", [Result]),
            Result()
            %% (rpc(AVar, {take, Util, CB}))()
        end
    end.

makeEmptyVar() ->
    fun() ->
        spawn(fun() -> empty() end)
    end.

%% AVar states

empty() ->
    receive
        {From, {put, #{ right := Right }, Value, CB}} ->
            From ! {self(), CB(Right(Value))},
            filled(Value);

        Any ->
            io:format("[empty] Received: ~p~n", [Any]),
            empty()
    end.

filled(Value) ->
    receive
        {From, {take, #{ right := Right }, CB}} ->
            From ! {self(), CB(Right(Value))},
            empty();

        Any ->
            io:format("[filled] Received: ~p~n", [Any]),
            filled(Value)
    end.

%% RPC framework

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.
