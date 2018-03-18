%%%%%%%%% Simple Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Based on one of the sample programs in:
%%%
%%% Artificial Intelligence:
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%%
%%% Copied by Jose Sosa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module( planner,
           [
               plan/4, change_state/3, conditions_met/2, member_state/2,
               move/3, go/2, test/0, test2/0
           ]).

:- [utils].

%%%%%%%%%%%%%%%%%%%% First we plan %%%%%%%%%%%%%%%%%%%%%%%%

% 1. RECUSRSIVE PLAN
%
% Base Case: State is equal to Goal, then we made it to Goal! YEY!
plan(State, Goal, _, Moves) :- equal_set(State, Goal),
                               write('moves are'), nl,
                               reverse_print_stack(Moves).

% Recursive Descent: State not equal to Goal, then we must bust a move!
plan(State, Goal, Been_list, Moves) :- move(Name, Preconditions, Actions),                %  |=> The cut (!) stops the backtracking
                                       conditions_met(Preconditions, State),              %  |   if the new plan returns true. We
                                       change_state(State, Actions, Child_state),         %  |   only need one path, so if we find
                                       not(member_state(Child_state, Been_list)),         %  |   one path, we don't need to keep looking
                                       stack(Child_state, Been_list, New_been_list),      %  |   for another one.
                                       stack(Name, Moves, New_moves),                     %  |
                                    plan(Child_state, Goal, New_been_list, New_moves), !. %==|


% 2. RECURSIVELY CHANGE STATE FROM A LIST OF ACTIONS
%
% Base Case: If no more Actions to take, then State stays the same.
change_state(S, [], S).

% Recursive Descent: If we want to add action to the Actions list
change_state(S, [add(P)|T], S_new) :- change_state(S, T, S2),
                                      add_to_set(P, S2, S_new), !.

% Recursive Descent: If we want to remove actions from the Actions list
change_state(S, [del(P)|T], S_new) :- change_state(S, T, S2),
                                      remove_from_set(P, S2, S_new), !.


% 3. SOME HELPER FUNCTIONS DEFINED
%
% Chech if a State has the pre-conditions we need to move forward
conditions_met(P, S) :- subset(P, S).

% Check if a State is a member of a State List; used to check if State has been visited
%


