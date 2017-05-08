%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% By: Erik Pihl & David Ådvall
%% Lab group 11
%% This is a implementation of map-reduce that distributes work among
%% a group of nodes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(dist_map_reduce).
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
    Parent = self(),
    Nodes = nodes() ++ [node()],
    NodeSplits = split_into(length(Nodes),split_into(M,Input)),
    Mappers = [rpc:call(Node, ?MODULE, spawn_mapper, [Parent,Map,R,Split])
     || {Splits, Node} <- lists:zip(NodeSplits, Nodes), Split <- Splits],
    Mappeds =
    [receive {Pid,L} -> L end || Pid <- Mappers],
    Iss = split_into(length(Nodes), lists:seq(0, R-1)),
    Reducers = [rpc:call(Node,?MODULE, spawn_reducer,[Parent,Reduce,I,Mappeds])
     || {Is, Node} <- lists:zip(Iss,Nodes), I <- Is],
    Reduceds =
    [receive {Pid,L} -> L end || Pid <- Reducers],
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
    %%io:format("Spawing reducer with id: ~p\n",[I]),
    Inputs = [KV
          || Mapped <- Mappeds,
         {J,KVs} <- Mapped,
         I==J,
         KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).

