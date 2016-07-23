:- use_module(library(system)).

log_level(quiet).

test_strategy(0, _, _).

test_strategy(N, AStrategy, BStrategy) :-
	format('Comparing ~w (Player1) / ~w (Player2):~n~n',[AStrategy, BStrategy]),
	statistics(runtime, [Start|_]),                                                %starting time
	test_strategy(N,AStrategy,BStrategy,Moves,Wins),                               %run test
	statistics(runtime, [End|_]),                                                  %finish time

	Time is End - Start,                                                           %time elapsed
	
	%format('Testing finished in ~3d seconds.\n', [Time]),
	
	count(Wins,'b',WinsR),
	format('Wins P1 (blue): ~w~n',[WinsR]),
	count(Wins,'r',WinsB),
        format('Wins P2 (red): ~w~n',[WinsB]),
	count(Wins,'draw',Draws),
	format('Number of draws: ~w~n',[Draws]),
	
	deleteall(Moves, 250, NonExaustiveMoves),                                         
	max(NonExaustiveMoves,Max),
        format('Longest (non-exhaustive) game: ~w~n',[Max]),                           %shortest + longest game
	min(Moves,Min),
        format('Shortest game: ~w~n',[Min]),
	
	sum(Moves, TotalMoves),                                                        %Moves is a list of moves in each game, Total Moves is the sum of all of them                                              
    	AverageLength is TotalMoves / N,
    	format('The average game length was ~3f moves.\n', [AverageLength]),           %Average 
	
	AverageTime is Time / N,                                                       %Time elapsed divided by N which is the number of games played - obvious
        format('Average game time is ~3d seconds.\n', [AverageTime]).

test_strategy(0,_,_,[],[]).
test_strategy(N,AStrategy,BStrategy,[NumMoves|Moves],[Winner|Wins]) :-
	N>0,
	log_level(LogLevel),
	play(LogLevel,AStrategy,BStrategy, NumMoves, Winner),
	NewN is N-1,
	test_strategy(NewN,AStrategy,BStrategy, Moves,Wins).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% H E L P E R S %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


deleteall([],A,[]).
deleteall([H|T],A,Result) :- H=A, deleteall(T,A,Result),!.
deleteall([H|T],A,[H|Result]) :- deleteall(T,A,Result).

min([Min],Min).
min([H1,H2|T],Min) :- (H1<H2;H1=H2),
		      min([H1|T],Min).
min([H1,H2|T],Min) :- H1>H2,
	 	      min([H2|T],Min).

max([Max],Max).
max([H1,H2|T],Max) :- (H1<H2;H1=H2),
		    max([H2|T],Max).
max([H1,H2|T],Max) :- H1>H2,
		    max([H1|T],Max).


count([],_,0).
count([H|T],H,NewCount) :- count(T,H,Count),
	               NewCount is Count+1.
count([_|T], H, R) :- count(T,H,R).


sum([], 0).
sum([H|T], Sum) :- sum(T, Rest),
  		   Sum is H + Rest.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :- bestMove(PlayerColour, CurrentBoardState, bloodlust, NewBoardState, Move, _).

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :- bestMove(PlayerColour, CurrentBoardState, self_preservation, NewBoardState, Move, _).

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :- bestMove(PlayerColour, CurrentBoardState, land_grab, NewBoardState, Move, _).

minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :- bestMove(PlayerColour, CurrentBoardState, minimax, NewBoardState, Move, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% H E L P E R %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opponent('r','b').        %need it for minimax because need to calculate opponents best landgrab move score
opponent('b','r').



%disassembles the CurrentBoardState in reds and blues living pieces

disassemble('r', [Blue,Red], Red,Blue).
disassemble('b', [Blue,Red], Blue,Red).

%assembles it back
assemble('r',Red,Blue,[Blue,Red]).
assemble('b',Blue,Red,[Blue,Red]).

%function finds all possible moves for player by checking what neighbouring positions of all players pieces are free
possMoves(Alive, OppAlive, PossMoves) :-findall([A,B,MA,MB],(member([A,B], Alive), neighbour_position(A,B,[MA,MB]), nonmember([MA,MB],Alive), nonmember([MA,MB],OppAlive)), PossMoves).




%the functions underneath are calculating the score I will use in the MaxMove function to find the best move for each strategy


%bloodlust strategs score is the number of opponents pieces on the CurrentBoardState
calcScore(PlayerColour, CurrentBoardState, bloodlust, Score) :- disassemble(PlayerColour,CurrentBoardState,_,OpponentsAliveCoordinates),
					      			length(OpponentsAliveCoordinates, OpponentsAlive),
					      			Score is -OpponentsAlive.                        %need negative value

%selfpress score is the number of players pieces on the CurrentBoardState
calcScore(PlayerColour, CurrentBoardState, self_preservation, Score) :- disassemble(PlayerColour, CurrentBoardState, Alives, _),
						      			length(Alives, Score).

% landgrabs score is the number of player's pieces - number of opponent's pieces
calcScore(PlayerColour, CurrentBoardState, land_grab, Score) :- disassemble(PlayerColour, CurrentBoardState, AlivesCoordinates, OpponentsAliveCoordinates),
				            		        length(OpponentsAliveCoordinates, OpponentsAlive),
					      			length(AlivesCoordinates, Alives),
					      			Score is Alives-OpponentsAlive.


%if opponent has no pieces
calcScore(PlayerColour, CurrentBoardState, minimax, Score) :- disassemble(PlayerColour,CurrentBoardState, Alives, []), !,
						   	      length(Alives, Score).

%minimum possible landgrab that we could get after opponents move
calcScore(PlayerColour, CurrentBoardState, minimax, Score) :- opponent(PlayerColour, OpponentsPlayerColour),
							      bestMove(OpponentsPlayerColour, CurrentBoardState, land_grab, _, _, OpponentsMaxLandgrab),
							      Score is -OpponentsMaxLandgrab. 


%Recursive function that for every possible move in BestMove function returns best move for the given Strategy
maxMove([],_,_,_,'bla','bla').
maxMove([Move|Moves], PlayerColour, CurrentBoardState, Strategy, BestMove, BestMoveGoal) :-
	disassemble(PlayerColour,CurrentBoardState,Alives,OpponentAlives),
	alter_board(Move,Alives,NewAlives),                                                                        %player makes move                      
	assemble(PlayerColour,NewAlives,OpponentAlives,NewBoardState), 
	next_generation(NewBoardState,NewGeneratedBoardState),                                                     %next generation created    
	calcScore(PlayerColour, NewGeneratedBoardState,Strategy,MoveGoal),                                         %score calculated depending on strategy
	maxMove(Moves,PlayerColour,CurrentBoardState,Strategy, OldBestMove, OldBestMoveGoal),                      %maximal move updated, for first move bla updated
	((OldBestMoveGoal=='bla' ; OldBestMoveGoal<MoveGoal) ->
		(BestMove=Move, BestMoveGoal=MoveGoal) ;
                (BestMove=OldBestMove,BestMoveGoal=OldBestMoveGoal)
	).

%finds best move according to given Strategy and returns this Move and CurrentBoardState configuration
bestMove(PlayerColour,CurrentBoardState,Strategy, NewBoardState, Move, Score) :-                           
	disassemble(PlayerColour,CurrentBoardState,Alive,OpponentsAlive),
	possMoves(Alive, OpponentsAlive, Moves),
	maxMove(Moves, PlayerColour, CurrentBoardState, Strategy, Move, Score),                                    %max move found for all possible moves, Move updated to BestMove
       	alter_board(Move, Alive, NewAlive),
	assemble(PlayerColour, NewAlive, OpponentsAlive, NewBoardState).                                           %max move implemented and new board assembled
