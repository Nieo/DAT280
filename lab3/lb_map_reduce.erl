%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% By: Erik Pihl & David Ådvall
%% Lab group 11
%% This is an implementation of map-reduce with load-balancing features.
%% It distributes the work so that faster nodes get more work and all
%% processes should finish at approximately the same time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lb_map_reduce).
-compile(export_all).

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

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

worker_pool(Funs) ->
    Master = self(),
    [rpc:call(Node, ?MODULE, spawn_workers, [Master])
        || Node<- nodes() ++ [node()]],
    worker_pool(Funs, [], length(Funs)).

worker_pool(_, Result, 0)-> Result;
worker_pool([], Result, Count) ->
    receive
        {available, _} ->
            worker_pool([], Result, Count);
        {done, _, Res} ->
            worker_pool([], Result ++ [Res], Count-1)
    end;
worker_pool([F|Funs], Result, Count) ->
    receive
        {available, Pid} ->
            Pid ! F,
            worker_pool(Funs, Result, Count);
        {done, Pid, Res} ->
            Pid ! F,
            worker_pool(Funs, Result ++ [Res], Count-1)
    end.

spawn_workers(Master) ->
    [spawn_link(?MODULE, init_worker, [Master])
        || _ <- lists:seq(1, erlang:system_info(schedulers))].

init_worker(Master) ->
    Master ! {available, self()},
    worker(Master).

worker(Master) ->
    receive
        F -> Master ! {done, self(), F()}
    end,
    worker(Master).

