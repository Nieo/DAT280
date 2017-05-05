%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a very simple implementation of map-reduce, in both
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ft_map_reduce).
-compile(export_all).

%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce.

%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
          || {K,V} <- Input,
         {K2,V2} <- Map(K,V)],
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
       KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).

group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

map_reduce_par(Map,M,Reduce,R,Input) ->
    process_flag(trap_exit, true),
    Splits = split_into(M,Input),
    Mappeds = worker_pool([ fun() ->
        Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                  || {K,V} <- Split,
                     {K2,V2} <- Map(K,V)],
            group(lists:sort(Mapped)) end || Split <- Splits]),
    Inputs =
    [[KV|| Mapped <- Mappeds,
         {J,KVs} <- Mapped,
         I==J,
         KV <- KVs]
     || I <- lists:seq(0,R-1)],
    Reduceds = worker_pool([fun() -> reduce_seq(Reduce, I) end
                              || I <- Inputs]),

    lists:sort(lists:flatten(Reduceds)).

spawn_mapper(Parent,Map,R,Split) ->
    spawn_link(fun() ->
            Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                  || {K,V} <- Split,
                     {K2,V2} <- Map(K,V)],
            Parent ! {self(),group(lists:sort(Mapped))}
        end).

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
    Inputs = [KV
          || Mapped <- Mappeds,
         {J,KVs} <- Mapped,
         I==J,
         KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).


worker_pool(Funs) ->
    Master = self(),
    Workers = lists:flatten([rpc:call(Node, ?MODULE, spawn_workers, [Master]) || Node<- nodes() ++ [node()]]),
    [link(Worker)|| Worker <- Workers],
    worker_pool(Funs, [], [], Workers).



worker_pool([F|Funs],InProgress, Result, [W|IdleWorkers]) ->
    W ! F,
    worker_pool(Funs, InProgress ++ [{W,F}], Result, IdleWorkers);
worker_pool([], [], Result, IdleWorkers)->
    [exit(Worker, kill)|| Worker <- IdleWorkers],
    Result;
worker_pool([], InProgress, Result, IdleWorkers) ->
    %io:format("~p ~p \n", [InProgress, length(Result)]),
    receive
        {done, Pid, Res} ->
            worker_pool([], [{P, Fun}|| {P, Fun} <- InProgress, P=/=Pid], Result ++ [Res], IdleWorkers ++ [Pid]);
        {'EXIT', Pid, Reason} ->
            %io:format("Exit : ~p ~p\n ",[Pid, Reason]),
            worker_pool([Fun|| {P, Fun} <- InProgress, Pid == P], lists:keydelete(Pid,1,InProgress), Result, [Worker|| Worker <- IdleWorkers, Worker /= Pid])
    end;
worker_pool([F|Funs], InProgress, Result, IdleWorkers) ->
    %io:format("~p ~p ~p \n", [[F|Funs], InProgress, length(Result)]),
    receive
        {done, Pid, Res} ->
            Pid ! F,
            worker_pool(Funs, [{P, Fun}|| {P, Fun} <- InProgress, P=/=Pid] ++ [{Pid, F}], Result ++ [Res], IdleWorkers);
        {'EXIT', Pid, Reason} ->
            %io:format("F Exit : ~p ~p\n ",[Pid, Reason]),
            worker_pool([F|Funs] ++ [Fun|| {P, Fun} <- InProgress, Pid == P], lists:keydelete(Pid,1,InProgress), Result, [Worker|| Worker <- IdleWorkers, Worker /= Pid])
    end.

spawn_workers(Master) ->
    [spawn(?MODULE, worker, [Master])|| _ <- lists:seq(1, erlang:system_info(schedulers))].

worker(Master) ->
    receive
        F -> Master ! {done, self(), F()}
    end,
    worker(Master).

