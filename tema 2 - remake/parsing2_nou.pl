% 11) Adauga reguli parser-ului de tip BUP ptr gramatica de la 9, fara det -> []
% Apelare: parse(s, [the, cat, sees, the, dog], []).

np(C, S1, S):- parse(vp, S1, S2), s(C, S2, S).
det(C, S1, S):- parse(n, S1, S2), np(C, S2, S).
np(C, S1, S):- parse(conj, S1, S2), parse(np, S2, S3), np(C, S3, S).
v(C, S1, S):- parse(np, S1, S2), parse(pp, S2, S3), vp(C, S3, S).
v(C, S1, S):- parse(np, S1, S2), vp(C, S2, S).
p(C, S1, S):- parse(np, S1, S2), pp(C, S2, S).
np(np, S, S).
s(s, S, S).
vp(vp, S, S).
v(v, S, S).
n(n, S, S).
det(det, S, S).
p(p, S, S).
pp(pp, S, S).
conj(conj, S, S).
cuvant(det, [the|X], X).
cuvant(det, [all|X], X).
cuvant(det, [every|X], X).
cuvant(p, [near|X], X).
cuvant(conj, [and|X], X).
cuvant(n,[cat|X],X).
cuvant(n,[cats|X],X).
cuvant(n,[dog|X],X).
cuvant(n,[dogs|X],X).
cuvant(n,[elephant|X],X).
cuvant(n,[elephants|X],X).
cuvant(v,[chase|X],X).
cuvant(v,[chases|X],X).
cuvant(v,[see|X],X).
cuvant(v,[sees|X],X).
cuvant(v,[amuse|X],X).
cuvant(v,[amuses|X],X).

parse(C, S1, S):- cuvant(W, S1, S2),
					P=..[W, C, S2, S],
					call(P).

