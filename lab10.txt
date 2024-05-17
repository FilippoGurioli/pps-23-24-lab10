search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).

search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).

search_two(X, cons(X, cons(_, T))) :- search(X, T).
search_two(X, cons(_, T)) :- search_two(X, T).

search_anytwo(X, cons(X, T)) :- search(X, T).
search_anytwo(X, cons(_, T)) :- search_anytwo(X, T).

size(nil, zero).
size(cons(H, T), s(N)) :- size(T, N).

sum(zero, X, X).
sum(s(X), Y, s(Z)) :- sum(X, Y, Z).

sum_list(nil, zero).
sum_list(cons(H, T), N) :- sum_list(T, R1), sum(H, R1, N).

count(List, E, N) :- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E2, N, M).

less(s(N), s(M)) :- less(N, M).
less(zero, s(M)).
more(s(N), s(M)) :- more(N, M).
more(s(N), zero).

max(cons(H, T), M) :- max(T, H, M).
max(nil, H, H).
max(cons(H, nil), M, H) :- more(H, M).
max(cons(H, nil), M, M) :- less(H, M).
max(cons(H, T), TM, M) :- max(T, H, M), more(H, TM).
max(cons(H, T), TM, M) :- max(T, TM, M), less(H, TM).

min(cons(H, T), M) :- min(T, H, M).
min(cons(H, nil), M, H) :- less(H, M).
min(cons(H, nil), M, M) :- more(H, M).
min(cons(H, T), TM, M) :- min(T, H, M), less(H, TM).
min(cons(H, T), TM, M) :- min(T, TM, M), more(H, TM).

min-max(L, Mi, Ma) :- min(L, Mi), max(L, Ma).

same-num(s(N), s(M)) :- same-num(N, M).
same-num(zero, zero).

same(cons(H, T), cons(H1, T1)) :- same-num(H, H1), same(T, T1).
same(nil, nil).

all-bigger(nil, nil).
all-bigger(cons(H, T), cons(H1, T1)) :- more(H, H1), all-bigger(T, T1).

sublist(nil, L).
sublist(cons(H, T), L) :- search(H, L), sublist(T, L).

seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

seqR(s(N), cons(H, T)) :- same-num(N, H), seqR(N, T).
seqR(zero, nil).

seqR2(N, L) :- seqR2(N, zero, L). 
seqR2(N, S, cons(H, T)) :- same-num(S, H), more(N, S), seqR2(N, s(S), T).
seqR2(N, N, nil).





