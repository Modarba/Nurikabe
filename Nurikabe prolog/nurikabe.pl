:- dynamic solve_cell/3.

board_size(7,7).

fxd_cell(1,2,3).
fxd_cell(1,6,1).
fxd_cell(3,1,2).
fxd_cell(3,4,1).
fxd_cell(5,2,1).
fxd_cell(6,3,2).
fxd_cell(7,1,1).
fxd_cell(7,5,1).
fxd_cell(7,7,6).
fxd_cell(5,5,2).

reset_board:-
   retractall(solve_cell(_,_,_)),init_board.

init_board() :-
    \+ set_row().
set_row() :-
    board_size(N, _),
    between(1, N, X),
    \+ set_col(X),
    fail.
set_col(X) :-
    (board_size(_, M),
    between(1, M, Y),
    (fxd_cell(X, Y, _) -> assert(solve_cell(X, Y, green));
    \+ fxd_cell(X, Y, _) -> assert(solve_cell(X, Y, 0))),
    fail).

print_board():- \+ get_row().
get_row():-
    nl, nl,
    board_size(N,_),
    between(1,N,X),
    \+ get_col(X), nl, nl, nl, fail.
get_col(X):-
    board_size(_,M),
    between(1,M,Y),
    (   fxd_cell(X,Y,Num) -> write(Num), write('  ')
    ;   \+ fxd_cell(X,Y,_), solve_cell(X,Y,blue) -> write('-  ')
    ;   \+ fxd_cell(X,Y,_), solve_cell(X,Y,green) -> write('*  ')
    ;   write('0  ')
    ),
    fail.

sea(X,Y):-solve_cell(X,Y,blue).
island(X,Y):-solve_cell(X,Y,green).

one:-
    (
    fxd_cell(X,Y,1),
    Y1 is Y + 1,
    retract(solve_cell(X, Y1, 0)),assert(solve_cell(X, Y1, blue));
    fxd_cell(X,Y,1),
    Y2 is Y - 1,
    retract(solve_cell(X, Y2, 0)),assert(solve_cell(X, Y2, blue));
    fxd_cell(X,Y,1),
    X1 is X + 1,
    retract(solve_cell(X1, Y, 0)),assert(solve_cell(X1, Y, blue));
    fxd_cell(X,Y,1),
    X2 is X - 1,
    retract(solve_cell(X2, Y, 0)),assert(solve_cell(X2, Y, blue))).
two:-
    fxd_cell(X,Y,_),
    X1 is X + 2, X2 is X - 2,
    Y1 is Y + 2, Y2 is Y - 2,
    X3 is X + 1, X4 is X - 1,
    Y3 is Y + 1, Y4 is Y - 1,
    (   fxd_cell(X1,Y,_) -> retract(solve_cell(X3, Y, 0)),assert(solve_cell(X3, Y, blue));
        fxd_cell(X2,Y,_) -> retract(solve_cell(X4, Y, 0)),assert(solve_cell(X4, Y, blue));
        fxd_cell(X,Y1,_) -> retract(solve_cell(X, Y3, 0)),assert(solve_cell(X, Y3, blue));
        fxd_cell(X,Y2,_) -> retract(solve_cell(X, Y4, 0)),assert(solve_cell(X, Y4, blue))
    ).
three:-
    fxd_cell(X,Y,_),
    X1 is X + 1,
    Y1 is Y + 1,
    fxd_cell(X1,Y1,_) ->
    (
        retract(solve_cell(X1, Y, 0)),assert(solve_cell(X1,Y,blue)),
        retract(solve_cell(X, Y1, 0)),assert(solve_cell(X,Y1,blue))
    ).

within_bounds(X, Y) :-
    X > 0, X =< 7,
    Y > 0, Y =< 7.
no9 :-
    sea(X, Y),
    X2 is X - 1,
    Y1 is Y + 1,
    sea(X2, Y),
    sea(X, Y1),
    retract(solve_cell(X2, Y1, 0)),
    assert(solve_cell(X2, Y1, green)).
no9 :-
    sea(X, Y),
    X2 is X - 1,
    Y2 is Y - 1,
    sea(X2, Y),
    sea(X, Y2),
    retract(solve_cell(X2, Y2, 0)),
    assert(solve_cell(X2, Y2, green)).
no9 :-
    sea(X, Y),
    X1 is X + 1,
    Y2 is Y - 1,
    sea(X1, Y),
    sea(X, Y2),
    retract(solve_cell(X1, Y2, 0)),
    assert(solve_cell(X1, Y2, green)).
no9 :-
    sea(X, Y),
    X1 is X + 1,
    Y1 is Y + 1,
    sea(X, Y1),
    sea(X1, Y),
    retract(solve_cell(X1, Y1, 0)),
    assert(solve_cell(X1, Y1, green)).

forth:-
        (

         fxd_cell(X,Y,Num), Num >= 2,
         X1 is X + 1,
         X2 is X - 1,
         Y1 is Y + 1,
         Y2 is Y - 1,
         (   within_bounds(X1,Y)->solve_cell(X1, Y, blue);
         within_bounds(X,Y1)->solve_cell(X, Y1, blue);
         within_bounds(X,Y2)->solve_cell(X, Y2, blue);
         within_bounds(X2,Y)->solve_cell(X2,Y,0))->
        (   retract(solve_cell(X2, Y, 0)), assert(solve_cell(X2, Y, green)));

         fxd_cell(X,Y,Num), Num >= 2,
         X1 is X + 1,
         X2 is X - 1,
         Y1 is Y + 1,
         Y2 is Y - 1,
         (   within_bounds(X1,Y)->solve_cell(X1, Y, blue);
         within_bounds(X2,Y)->solve_cell(X2, Y, blue);
         within_bounds(X,Y2)->solve_cell(X, Y2, blue);
         within_bounds(X,Y1)->solve_cell(X,Y1,0))->
        (   retract(solve_cell(X, Y1, 0)), assert(solve_cell(X, Y1, green)));

         fxd_cell(X,Y,Num), Num >= 2,
         X1 is X + 1,
         X2 is X - 1,
         Y1 is Y + 1,
         Y2 is Y - 1,
        (    within_bounds(X,Y1)->solve_cell(X, Y1, blue);
         within_bounds(X,Y2)->solve_cell(X, Y2, blue);
         within_bounds(X2,Y)->solve_cell(X2, Y, blue);
         within_bounds(X1,Y)->solve_cell(X1,Y,0))->
        (   retract(solve_cell(X1, Y, 0)), assert(solve_cell(X1, Y, green)));

         fxd_cell(X,Y,Num), Num >= 2,
         X1 is X + 1,
         X2 is X - 1,
         Y1 is Y + 1,
         Y2 is Y - 1,
        (    within_bounds(X1,Y)->solve_cell(X1, Y, blue);
         within_bounds(X2,Y)->solve_cell(X2, Y, blue);
         within_bounds(X,Y1)->solve_cell(X, Y1, blue);
         within_bounds(X,Y2)->solve_cell(X,Y2,0))->
        (   retract(solve_cell(X, Y2, 0)), assert(solve_cell(X, Y2, green)))).


five :-
        fxd_cell(X,Y,2),
        X2 is X - 1, Y1 is Y + 1,
        \+ sea(X2,Y), \+ sea(X,Y1),
        \+ island(X2,Y), \+ island(X,Y1),
        retract(solve_cell(X2,Y1,0)),
        assert(solve_cell(X2,Y1,blue)).
five :-
        fxd_cell(X,Y,2),
        X1 is X + 1, Y1 is Y + 1,
        \+ sea(X1,Y), \+ sea(X,Y1),
        \+ island(X1,Y), \+ island(X,Y1),
        retract(solve_cell(X1,Y1,0)),
        assert(solve_cell(X1,Y1,blue)).
five :-
        fxd_cell(X,Y,2),
        X2 is X - 1, Y2 is Y - 1,
        \+ sea(X2,Y), \+ sea(X,Y2),
        \+ island(X2,Y), \+ island(X,Y2),
        retract(solve_cell(X2,Y2,0)),
        assert(solve_cell(X2,Y2,blue)).
five :-
        fxd_cell(X,Y,2),
        X1 is X + 1, Y2 is Y - 1,
        \+ sea(X1,Y), \+ sea(X,Y2),
        \+ island(X1,Y), \+ island(X,Y2),
        retract(solve_cell(X1,Y2,0)),
        assert(solve_cell(X1,Y2,blue)).

sur_sea:-
    (solve_cell(X,Y,0),
    X1 is X + 1,
    X2 is X - 1,
    Y1 is Y + 1,
    Y2 is Y - 1,
    solve_cell(X1,Y,blue),
    solve_cell(X2,Y,blue),
    solve_cell(X,Y1,blue),
    solve_cell(X,Y2,blue)
    ->retract(solve_cell(X,Y,0)),assert(solve_cell(X,Y,blue))
    ).

add_to_list(E,L,[E|L]).

same_color(X1,Y1,X2,Y2):-
    solve_cell(X1,Y1,C1), solve_cell(X2,Y2,C2),
    C1 = C2.

cell_neighbors(I, J, L, NewL):-
    J1 is J - 1, J2 is J + 1,
    I1 is I - 1, I2 is I + 1,
    (   check_neighbor(I, J1, I, J, L, L1) ; L1 = L ),
    (   check_neighbor(I, J2, I, J, L1, L2) ; L2 = L1 ),
    (   check_neighbor(I1, J, I, J, L2, L3) ; L3 = L2 ),
    (   check_neighbor(I2, J, I, J, L3, NewL) ; NewL = L3 ).

check_neighbor(X, Y, I, J, L, Lout) :-
    same_color(X,Y,I,J),
    \+ member((X,Y),L),
    add_to_list((X,Y),L,Ltemp),
    cell_neighbors(X,Y,Ltemp,Lout),!.

seven :-
(
    (sea(X,Y),
    X2 is X - 1,
    Y2 is Y - 1,
    sea(X2,Y),sea(X,Y2),solve_cell(X2,Y2,0))->
    (   retract(solve_cell(X2,Y2,0)), assert(solve_cell(X2,Y2,green)));
    (sea(X,Y),
    X2 is X - 1,
    Y1 is Y + 1,
    sea(X2,Y),sea(X,Y1),solve_cell(X2,Y1,0))->
    (   retract(solve_cell(X2,Y1,0)), assert(solve_cell(X2,Y1,green)));
    (sea(X,Y),
    X1 is X + 1,
    Y2 is Y - 1,
    sea(X1,Y),sea(X,Y2),solve_cell(X1,Y2,0))->
    (   retract(solve_cell(X1,Y2,0)), assert(solve_cell(X1,Y2,green)));
    (sea(X,Y),
    X1 is X + 1,
    Y1 is Y + 1,
    sea(X,Y1),sea(X1,Y),solve_cell(X1,Y1,0))->
    (   retract(solve_cell(X1,Y1,0)), assert(solve_cell(X1,Y1,green)))
).
non_empty_cell(X,Y):- \+solve_cell(X,Y,0).
empty_cell(X,Y):- solve_cell(X,Y,0).
seven_2:-
    island(X,Y),
    \+fxd_cell(X,Y,_),
    X2 is X - 1,
    Y2 is Y - 1,
    X1 is X + 1,
    Y1 is Y + 1,
(
    (    (within_bounds(X2,Y)->non_empty_cell(X2,Y),\+fxd_cell(X2,Y,_);
         within_bounds(X,Y2)->non_empty_cell(X,Y2),\+fxd_cell(X,Y2,_);
         within_bounds(X1,Y)->non_empty_cell(X1,Y)),\+fxd_cell(X1,Y,_),
         solve_cell(X,Y1,0))->
    (   retract(solve_cell(X,Y1,0)), assert(solve_cell(X,Y1,green)));
    (    (within_bounds(X2,Y)->non_empty_cell(X2,Y),\+fxd_cell(X2,Y,_);
         within_bounds(X,Y1)->non_empty_cell(X,Y1),\+fxd_cell(X,Y1,_);
         within_bounds(X,Y2)->non_empty_cell(X,Y2)),\+fxd_cell(X,Y2,_),
         solve_cell(X1,Y,0))->
    (   retract(solve_cell(X1,Y,0)), assert(solve_cell(X1,Y,green)));
    (    (within_bounds(X1,Y)->non_empty_cell(X1,Y),\+fxd_cell(X1,Y,_);
         within_bounds(X,Y1)->non_empty_cell(X,Y1),\+fxd_cell(X,Y1,_);
         within_bounds(X2,Y)->non_empty_cell(X2,Y)),\+fxd_cell(X2,Y,_),
         solve_cell(X,Y2,0))->
    (   retract(solve_cell(X,Y2,0)), assert(solve_cell(X,Y2,green)));
    (    (within_bounds(X,Y1)->non_empty_cell(X,Y1),\+fxd_cell(X,Y1,_);
         within_bounds(X1,Y)->non_empty_cell(X1,Y),\+fxd_cell(X1,Y,_);
         within_bounds(X,Y2)->non_empty_cell(X,Y2)),\+fxd_cell(X,Y2,_),
         solve_cell(X2,Y,0))->
    (   retract(solve_cell(X2,Y,0)), assert(solve_cell(X2,Y,green)))
).

complete :-

      findall(fxd_cell(X,Y,_),count_island_cells(X,Y),_)
      ->
      empty_cell(I,J), retract(solve_cell(I,J,0)), assert(solve_cell(I,J,blue)).
complete_7:-
   (   fxd_cell(X,Y,_),\+count_island_cells(X,Y))->seven_2,complete_7.
complete_7:-true.

island_continuity:-
   seven_2,
   complete_7,
   complete.

wall_continuity :-
    (
    (   sea(X,Y),sea(X1,Y1),island(X1,Y),X1 is X + 1,Y1 is Y + 1,
            solve_cell(X,Y1,0)
     )
    ->(retract(solve_cell(X,Y1,0)),assert(solve_cell(X,Y1,blue)));

    (   sea(X,Y),sea(X1,Y1),island(X,Y1),X1 is X + 1,Y1 is Y + 1,
        solve_cell(X1,Y,0)
    )
    ->(retract(solve_cell(X1,Y,0)),assert(solve_cell(X1,Y,blue)));


    (   sea(X,Y),sea(X1,Y2),island(X,Y2),X1 is X + 1,Y2 is Y - 1,
        solve_cell(X1,Y,0)
    )
    ->(retract(solve_cell(X1,Y,0)),assert(solve_cell(X1,Y,blue)));

    (   sea(X,Y),sea(X1,Y2),island(X1,Y),X1 is X + 1,Y2 is Y - 1,
        solve_cell(X,Y2,0)
    )
    ->(retract(solve_cell(X,Y2,0)),assert(solve_cell(X,Y2,blue)));


    (   sea(X,Y),sea(X2,Y1),island(X2,Y),X2 is X - 1,Y1 is Y + 1,
        solve_cell(X,Y1,0)
    )
    ->(retract(solve_cell(X,Y1,0)),assert(solve_cell(X,Y1,blue)));

    (   sea(X,Y),sea(X2,Y1),island(X,Y1),X2 is X - 1,Y1 is Y + 1,
        solve_cell(X2,Y,0)
    )
    ->(retract(solve_cell(X2,Y,0)),assert(solve_cell(X2,Y,blue)));


    (   sea(X,Y),sea(X2,Y2),island(X2,Y), X2 is X - 1,Y2 is Y - 1,
        solve_cell(X,Y2,0)
    )
    ->(retract(solve_cell(X,Y2,0)),assert(solve_cell(X,Y2,blue)));

    (   sea(X,Y),sea(X2,Y2),island(X,Y2), X2 is X - 1,Y2 is Y - 1,
        solve_cell(X2,Y,0)
    )
    ->(retract(solve_cell(X2,Y,0)),assert(solve_cell(X2,Y,blue)))
).


island_of_two:-
    (  fxd_cell(X,Y,2),
       X1 is X + 1, full_island_of_2(X,Y),solve_cell(X1,Y,0))
       ->(retract(solve_cell(X1,Y,0)),assert(solve_cell(X1,Y,blue)));
    (  fxd_cell(X,Y,2),
       X2 is X - 1, full_island_of_2(X,Y),solve_cell(X2,Y,0))
       ->(retract(solve_cell(X2,Y,0)),assert(solve_cell(X2,Y,blue)));
    (  fxd_cell(X,Y,2),
       Y1 is Y + 1, full_island_of_2(X,Y),solve_cell(X,Y1,0))
       ->(retract(solve_cell(X,Y1,0)),assert(solve_cell(X,Y1,blue)));
    (
       fxd_cell(X,Y,2),
       Y2 is Y - 1, full_island_of_2(X,Y),solve_cell(X,Y2,0))
       ->(retract(solve_cell(X,Y2,0)),assert(solve_cell(X,Y2,blue))).

full_island_of_2(X,Y):-
     fxd_cell(X,Y,2),(
     X1 is X + 1,
     solve_cell(X1,Y,green)->true,!;
     X2 is X - 1,
     solve_cell(X2,Y,green)->true,!;
     Y1 is Y + 1,
     solve_cell(X,Y1,green)->true,!;
     Y2 is Y - 1,
     solve_cell(X,Y2,green)->true,!
    ).

list_length([],0).
list_length([_|T], Res):- list_length(T, Res1), Res is Res1 + 1.

count_island_cells(X,Y):-
    fxd_cell(X,Y,1),
    X1 is X + 1, X2 is X - 1,
    Y1 is Y + 1, Y2 is Y - 1,
    (solve_cell(X1,_,green), false; true),
    (solve_cell(X2,_,green), false; true),
    (solve_cell(_,Y1,green), false; true),
    (solve_cell(_,Y2,green), false; true),true.
count_island_cells(X,Y):-
    fxd_cell(X,Y,Len),
    cell_neighbors(X,Y,[],Res),
    (   Res \= [] -> append([],Res,Res), list_length(Res, Len)),true.
%count_island_cells(_,_):-true.
%full_island:- findall(true,count_island_cells,_).

solve:- findall(true,one,_),
   findall(true,two,_),
   findall(true,three,_),
   findall(true,sur_sea,_),
   findall(true,five,_),
   findall(true,seven,_),
   findall(true,seven,_),
   findall(true,wall_continuity,_),
   findall(true,wall_continuity,_),
   findall(true,wall_continuity,_),
   findall(true,wall_continuity,_),
   findall(true,seven_2,_),
   findall(true,seven_2,_),
   complete,
   print_board.














