/*
* CAP 4630 Artificial Intelligence
* University of Central Florida
* Spring 2018
* Program 2 - Prolog Two Room Planner
* Written by: Luis Gamarra Jimenez and Jose Sosa
*/

/* module */
:- module( planner,
        [
            plan/4, change_state/3, conditions_met/2, member_state/2,
            move/3, go/2, test1/0, test2/0, test3/0
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
     [handempty, clear(X, roomlocation1), on(X, Y, roomlocation1), roomlocation1],
     [del(handempty), del(clear(X, roomlocation1)), del(on(X, Y, roomlocation1)), add(clear(Y, roomlocation1)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X, roomlocation2), on(X, Y, roomlocation2), roomlocation2],
     [del(handempty), del(clear(X, roomlocation2)), del(on(X, Y, roomlocation2)), add(clear(Y, roomlocation2)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X, roomlocation1), ontable(X, roomlocation1), roomlocation1],
     [del(handempty), del(clear(X, roomlocation1)), del(ontable(X, roomlocation1)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X, roomlocation2), ontable(X, roomlocation2), roomlocation2],
     [del(handempty), del(clear(X, roomlocation2)), del(ontable(X, roomlocation2)), add(holding(X))]
    ).

move(
     putdown(X),
     [holding(X), roomlocation1],
     [del(holding(X)), add(ontable(X, roomlocation1)), add(clear(X, roomlocation1)), add(handempty)]
    ).

move(
     putdown(X),
     [holding(X), roomlocation2],
     [del(holding(X)), add(ontable(X, roomlocation2)), add(clear(X, roomlocation2)), add(handempty)]
    ).

move(
     stack(X, Y),
     [holding(X), clear(Y, roomlocation1), roomlocation1],
     [del(holding(X)), del(clear(Y, roomlocation1)), add(handempty), add(on(X, Y, roomlocation1)), add(clear(X, roomlocation1))]
    ).

move(
     stack(X, Y),
     [holding(X), clear(Y, roomlocation2), roomlocation2],
     [del(holding(X)), del(clear(Y, roomlocation2)), add(handempty), add(on(X, Y, roomlocation2)), add(clear(X, roomlocation2))]
    ).

move(
     goroom1,
     [roomlocation2],
     [add(roomlocation1), del(roomlocation2)]
    ).

move(
     goroom2,
     [roomlocation1],
     [add(roomlocation2), del(roomlocation1)]
    ).

/* run commands */
go(S, G) :- plan(S, G, [S], []).

/**
* test1 from Program 2 Requirements section.
* test1 is a 0-arity test predicate.
* The start state is: block A, B, and C and robotic arm are all in Room 1.
*                     Block B and C on the table and block A on top of block B.
* The goal state is: all blocks and arm in Room 1, with block C on the table,
*                    block B on C, and block A on B.
*/
test1 :- go(
            [handempty, ontable(b, roomlocation1), ontable(c, roomlocation1), on(a, b, roomlocation1), clear(a, roomlocation1), clear(c, roomlocation1), roomlocation1],
            [handempty, ontable(c, roomlocation1), on(b, c, roomlocation1), on(a, b, roomlocation1), clear(a, roomlocation1), roomlocation1]
            ).

/**
* test2 from Program 2 Requirements section.
* test2 is a 0-arity test predicate.
* The start state is: block A, B, and C and robotic arm are all in Room 1.
*                     Block B and C on the table and block A on top of block B.
* The goal state is: robot arm in Room 1, all blocks in Room 2, with block B on the table,
                     block C on B, and block A on C.
*/
test2 :- go(
            [handempty, ontable(b, roomlocation1), ontable(c, roomlocation1), on(a, b, roomlocation1), clear(a, roomlocation1), clear(c, roomlocation1), roomlocation1],
            [handempty, ontable(b, roomlocation2), on(c, b, roomlocation2), on(a, c, roomlocation2), clear(a, roomlocation2), roomlocation1]
            ).

/**
* test3 from Program 2 Operational Description section.
* test3 is a 0-arity test predicate.
* The start state is: robot arm in Room 1, all blocks in Room 1, with block B on the table,
*                     and block A on B.
* The goal state is: robot arm in Room 1, all blocks in Room 2, with block B on the table,
*                    and block A on B.
*/
test3 :- go(
            [handempty, ontable(b, roomlocation1), on(a, b, roomlocation1), clear(a, roomlocation1), roomlocation1],
            [handempty, ontable(b, roomlocation2), on(a, b, roomlocation2), clear(a, roomlocation2), roomlocation1]
            ).