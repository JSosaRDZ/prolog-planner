
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
            move/3,go/2,testTwoRoomRubric/0,testSingleRoomSlides/0,
            testTwoRoomSlides/0
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
     [handempty, clear(X), on(X, Y, roomlocation1), roomlocation1],
     [del(handempty), del(clear(X)), del(on(X, Y, roomlocation1)), add(clear(Y)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X), on(X, Y, roomlocation2), roomlocation2],
     [del(handempty), del(clear(X)), del(on(X, Y, roomlocation2)), add(clear(Y)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X), ontable(X, roomlocation1), roomlocation1],
     [del(handempty), del(clear(X)), del(ontable(X, roomlocation1)), add(holding(X))]
    ).

move(
     pickup(X),
     [handempty, clear(X), ontable(X, roomlocation2), roomlocation2],
     [del(handempty), del(clear(X)), del(ontable(X, roomlocation2)), add(holding(X))]
    ).

move(
     putdown(X),
     [holding(X), roomlocation1],
     [del(holding(X)), add(ontable(X, roomlocation1)), add(clear(X)), add(handempty)]
    ).

move(
     putdown(X),
     [holding(X), roomlocation2],
     [del(holding(X)), add(ontable(X, roomlocation2)), add(clear(X)), add(handempty)]
    ).

move(
     stack(X, Y),
     [holding(X), clear(Y), roomlocation1],
     [del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y, roomlocation1)), add(clear(X))]
    ).

move(
     stack(X, Y),
     [holding(X), clear(Y), roomlocation2],
     [del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y, roomlocation2)), add(clear(X))]
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

testSingleRoomSlides :- go(
            [handempty, ontable(c,roomlocation1), ontable(b,roomlocation1), on(a, b, roomlocation1), clear(a), clear(c), roomlocation1],
            [handempty, ontable(c,roomlocation1), on(b, c,roomlocation1), on(a, b,roomlocation1), clear(a), roomlocation1]
                          ).

testTwoRoomRubric :- go(
            [handempty, ontable(b,roomlocation1), on(a, b, roomlocation1), clear(a), roomlocation1],
            [handempty, ontable(b,roomlocation2), on(a, b, roomlocation2), clear(a), roomlocation1]
          ).

testTwoRoomSlides :- go(
            [handempty, ontable(b,roomlocation1), on(a, b, roomlocation1), clear(a), ontable(c,roomlocation1), clear(c), roomlocation1],
            [handempty, ontable(b,roomlocation2), on(c, b, roomlocation2), clear(a), on(a, c, roomlocation2), roomlocation1]
          ).


