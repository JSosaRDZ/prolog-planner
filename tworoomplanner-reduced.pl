
%%%%%%%%% Simple Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Based on one of the sample programs in:
%%%
%%% Artificial Intelligence:
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* module */
:- module( planner,
        [
            plan/4,change_state/3,conditions_met/2,member_state/2,
            move/3,go/2,test/0,test2/0
        ]).

/* utils */
:- [utils].

/* plan */
plan(State, Goal, _, Moves) :- equal_set(State, Goal),
                               write('moves are'), nl,
                               reverse_print_stack(Moves).

plan(State, Goal, Been_list, Moves) :- move(Name, Preconditions, Actions),
                                       conditions_met(Preconditions, State),
                                       change_state(State, Actions, Child_state),
                                       not(member_state(Child_state, Been_list)),
                                       stack(Child_state, Been_list, New_been_list),
                                       stack(Name, Moves, New_moves),
                                       plan(Child_state, Goal, New_been_list, New_moves),!.


/* change state */
change_state(S, [], S).

change_state(S, [add(P)|T], S_new) :- change_state(S, T, S2), add_to_set(P, S2, S_new), !.

change_state(S, [del(P)|T], S_new) :- change_state(S, T, S2), remove_from_set(P, S2, S_new), !.

/* conditions met */
conditions_met(P, S) :- subset(P, S).

/* member states */
member_state(S, [H|_]) :- equal_set(S, H).

member_state(S, [_|T]) :- member_state(S, T).

/* move types */
move(
     pickup(X),
     [handempty, clear(X), on(X, Y, Z), roomlocation(Z)],
     [del(handempty), del(clear(X)), del(on(X, Y, Z)), add(clear(Y)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X), ontable(X, Z), roomlocation(Z)],
     [del(handempty), del(clear(X)), del(ontable(X, Z)), add(holding(X))]
    ).

move(
     putdown(X),
     [holding(X), roomlocation(X)],
     [del(holding(X)), add(ontable(X, Z)), add(clear(Z)), add(handempty)]
    ).

move(
     stack(X, Y, Z),
     [holding(X), clear(Y), roomlocation(Z)],
     [del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y, Z)), add(clear(X))]
    ).

move(
     goroom1,
     [roomlocation(2)],
     [add(roomlocation(1)), del(roomlocation(2))]
    ).

move(
     goroom2,
     [roomlocation(1)],
     [add(roomlocation(2)), del(roomlocation(1))]
    ).

/* run commands */
go(S, G) :- plan(S, G, [S], []).

test :- go([handempty, ontable(b,roomlocation1), on(a, b, roomlocation1), clear(a), roomlocation1],
            [handempty, ontable(b,roomlocation2), on(a, b, roomlocation2), clear(a), roomlocation1]).


