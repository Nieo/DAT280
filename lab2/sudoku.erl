-module(sudoku).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%%      ?FORALL(Mat,matrix(M+1,N+1),
%%          transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
            lists:map(fun lists:append/1, X)
        end,
        Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
    lists:map(
      fun lists:append/1,
      lists:map(
        fun(X)->lists:map(fun triples/1,X) end,
        triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%%      unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
      1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
    safe_rows(transpose(M)) andalso
    safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
          X;
     true ->
          Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
    refine_rows(
      transpose(
        refine_rows(
          transpose(
        unblocks(
          refine_rows(
            blocks(M))))))),
    if M==NewM ->
        M;
       true ->
        refine(NewM)
    end.

refine_rows(M) ->
    %lists:map(fun refine_row/1,M).
    %io:format("\nM ~p",[M]),
    Refs = [send_to_pool(Row) || Row <- M],
    [receive {Ref, Result} ->
      case Result of
        no_solution -> exit(no_solution);
        X -> X
      end
     end || Ref <- Refs].


send_to_pool(Row) ->
  Ref = make_ref(),
  whereis(m_worker_pool) ! {Ref, Row, self()},
  Ref.

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
    [if is_list(X) ->
         case X--Entries of
             [] -> no_solution; %exit(no_solution1);
             [Y] -> Y;
             NewX -> NewX
         end;
        true ->
         X
     end
     || X <- Row],
    NewEntries = entries(NewRow),
    case lists:member(no_solution, NewRow) of
      false ->
        %% check we didn't create a duplicate entry
        case length(lists:usort(NewEntries)) == length(NewEntries) of
        true ->
            NewRow;
        false ->
            no_solution %exit(no_solution2)
        end;
      true ->
        no_solution
    end .

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->
    lists:sum(
      [lists:sum(
     [if is_list(X) ->
          length(X);
         true ->
          0
      end
      || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
    lists:min([{length(X),I,J,X}
           || {I,Row} <- lists:zip(Nine,M),
              {J,X} <- lists:zip(Nine,Row),
              is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
    lists:sort(
      [{hard(NewM),NewM}
       || NewM <- Ms,
          not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%%      ?IMPLIES(L/=[],
%%           ?FORALL(I,choose(1,length(L)),
%%               update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

solve(M) ->
    %io:format("Starting a new solve"),
    Solution = solve_refined(refine(fill(M))),
    case valid_solution(Solution) of
    true ->
        Solution;
    false ->
        exit({invalid_solution,Solution})
    end.

solve_refined(M) ->
    case solved(M) of
    true ->
        M;
    false ->
        solve_one(guesses(M))
    end.

solve_one([]) ->
    exit(no_solution);
solve_one([M]) ->
    solve_refined(M);
solve_one([M|Ms]) ->
    case catch solve_refined(M) of
    {'EXIT',no_solution} ->
        solve_one(Ms);
    Solution ->
        Solution
    end.

%% benchmarks

-define(EXECUTIONS,1).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    Pid = spawn_link(fun ppool/0),
    register(m_worker_pool, Pid),
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).

%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).


ppool() ->
  Workers = [spawn_link(fun worker/0) || _ <- lists:seq(1, erlang:system_info(schedulers)-1)],
  ppool(Workers, []).


ppool([],[]) ->
  receive
    {Ref, Row, Parent} -> ppool([], [{Ref, Row, Parent}]);
    {available, Node} -> ppool([Node], [])
  end;
ppool([], [T|Tasks]) ->
  receive
    {Ref, Row, Parent} -> ppool([], [T|Tasks] ++ [{Ref, Row, Parent}]);
    {available, Node} -> Node ! T,
                         ppool([], Tasks)
  end;
ppool([W|Workers], []) ->
  receive
    {Ref, Row, Parent} -> W ! {Ref,  Row, Parent},
                              ppool(Workers, []);
    {available, Node} -> ppool([Node] ++ [W|Workers], [])
  end.

worker() ->
  receive
    {Ref, Row, Parent} ->
      %io:format("Ref ~p Row ~p Parent ~p",[Ref,Row,Parent]),
      Result = refine_row(Row),
      %io:format("Result ~p",[Result]),
      Parent ! {Ref,Result},
      whereis(m_worker_pool) ! {available, self()},
      worker()
  end.



% par_benchmarks([], Completed) when length(Completed) < 7 -> receive
%                                                               {Result} -> par_benchmarks([], Completed ++ [Result])
%                                                             end;
% par_benchmarks([], Completed) -> Completed;
% par_benchmarks([P|Puzzles], Completed) ->
%   case PoolPid ! {self(),[P]} of
%     {no_worker_available} ->
%       receive
%         {Result} -> par_benchmarks([P|Puzzles], Completed ++ [Result])
%       end;
%     {ok} -> par_benchmarks(Puzzles, Completed)
%   end.


% m_worker_pool([A|Available],All) ->
%   receive
%     {Pid, Puzzle} -> case length([A|Available]) =:= 0 of ->
%       true -> Pid ! {no_worker_available},
%               m_worker_pool([], All)
%       false -> A ! {Pid, Puzzle},
%                Pid ! {ok}
%                m_worker_pool(Available, All)
%   end


% worker() ->
%   receive
%     {Source, Work, Parameters} -> Result = Work(Parameters),
%       Source ! {Result},
%       worker()
%   end.

