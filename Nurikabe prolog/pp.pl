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

solve_cell(1,2,green).
solve_cell(1,6,green).
solve_cell(3,1,green).
solve_cell(3,4,green).
solve_cell(5,2,green).
solve_cell(5,5,green).
solve_cell(6,3,green).
solve_cell(7,1,green).
solve_cell(7,5,green).
solve_cell(7,7,green).

solve_cell(1,1,blue).
solve_cell(1,3,green).
solve_cell(1,4,green).
solve_cell(1,5,blue).
solve_cell(1,7,blue).

solve_cell(2,1,blue).
%%solve_cell(2,2,green).
solve_cell(2,2,blue).
solve_cell(2,3,blue).
solve_cell(2,4,blue).
solve_cell(2,5,blue).
solve_cell(2,6,blue).
solve_cell(2,7,blue).

solve_cell(3,2,green).
%%solve_cell(3,2,blue).
solve_cell(3,3,blue).
solve_cell(3,5,blue).
solve_cell(3,6,green).
solve_cell(3,7,green).

solve_cell(4,1,blue).
solve_cell(4,2,blue).
solve_cell(4,3,blue).
solve_cell(4,4,blue).
solve_cell(4,5,blue).
solve_cell(4,6,blue).
solve_cell(4,7,green).

solve_cell(5,1,blue).
solve_cell(5,3,blue).
solve_cell(5,4,green).
solve_cell(5,6,blue).
solve_cell(5,7,green).

solve_cell(6,1,blue).
solve_cell(6,2,blue).
solve_cell(6,4,blue).
%%solve_cell(6,5,green).
solve_cell(6,5,blue).
solve_cell(6,6,blue).
solve_cell(6,7,green).

solve_cell(7,2,blue).
solve_cell(7,3,green).
solve_cell(7,4,blue).
solve_cell(7,6,blue).

add_to_list(E,L,[E|L]).

same_color(X1,Y1,X2,Y2):-
    solve_cell(X1,Y1,C1), solve_cell(X2,Y2,C2),
    C1 = C2.

cell_neighbors(I, J, L, L) :-
    I < 1; I > 7; J < 1; J > 7.
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

sea(X,Y):-solve_cell(X,Y,blue).
island(X,Y):-solve_cell(X,Y,green).
cell_neighbor_island(I,J):-
    sea(I,J),
    island(I,J-1),
    island(I,J+1),
    island(I-1,J),
    island(I+1,J).
sea_to_another(I,J,I1,J1):-
    sea(I,J),sea(I1,J1),(
              (I==I1,J1 is J+1);
              (I==I1, J1 is J-1);
              (J==J1,I1 is I+1);
              (J==J1,I1 is I-1)).
sea_is_alone:-
    find_sea_alone(1, 1, 7, 7).

find_sea_alone(X, _, _, _):-
    X > 7,
    !,
    false.
find_sea_alone(_, Y, _, _):-
    Y > 7,
    !,
    false.
find_sea_alone(X, Y, _, _):-
    sea(X, Y),
    \+ cell_neighbor_island(X, Y),
    \+ sea_to_another(X, Y, X + 1, Y + 1),
    !,
    true.
find_sea_alone(X,Y,I,J):-
    X1 is X + 1,
    Y1 is Y + 1,
    find_sea_alone(X1, Y, I, J),
    find_sea_alone(X, Y1,I,J).


no2:-
        sea(X,Y),
    X1 is X+1 , Y1 is Y+1,
    sea(X1,Y),
    sea(X,Y1),
    sea(X1,Y1).

count_fixed([], Count, Count).
count_fixed([(X,Y)|T], Acc, Count):-
    fxd_cell(X,Y,_), !,
    NewAcc is Acc + 1,
    count_fixed(T, NewAcc, Count).
count_fixed([_|T], Acc, Count):-
    count_fixed(T, Acc, Count).

island_const:-
    fxd_cell(X,Y,_), cell_neighbors(X,Y,[],L),
    count_fixed(L, 0, Res), Res > 1.

solve1:- \+ no2,
        sea_is_alone,
        \+ island_const,
        count_island_cells().


print_board():- \+ get_row().
get_row():-
    nl,
    board_size(N,_),
    between(1,N,X),
    \+ get_col(X), nl, nl, fail.
get_col(X):-
    board_size(_,M),
    between(1,M,Y),
    (   fxd_cell(X,Y,Num), solve_cell(X,Y,green) -> write(Num), write('  ')
    ;   \+ fxd_cell(X,Y,_), solve_cell(X,Y,blue) -> write('-  ')
    ;   \+ fxd_cell(X,Y,_), solve_cell(X,Y,green) -> write('*  ')
    ;   solve_cell(X,Y,0)->write('0  ')
    ),
    fail.

list_length([],0).
list_length([_|T], Res):- list_length(T, Res1), Res is Res1 + 1.

count_island_cells():-
    fxd_cell(X,Y,1),
    X1 is X + 1, X2 is X - 1,
    Y1 is Y + 1, Y2 is Y - 1,
    (solve_cell(X1,Col,green), false; true),
    (solve_cell(X2,Col,green), false; true),
    (solve_cell(Row,Y1,green), false; true),
    (solve_cell(Row,Y2,green), false; true).

count_island_cells():-
    fxd_cell(X,Y,Len),
    cell_neighbors(X,Y,[],Res),
    (   Res \= [] -> append([],Res,Res), list_length(Res, Len)).

save_to_file(FileName):- tell(FileName),
                         listing,
                         told.










































