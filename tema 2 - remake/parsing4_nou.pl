% 13) Adauga legaturi
np(np, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, np, An, AnF).
np(np, S1, S, Nr, Nr1, An, AnF):- prelucreaza(Nr1, np, An, An1), parse(conj, S1, S2, Nr2, An1, An2), parse(np, S2, S3, Nr2, An2, An3), np(np, S3, S, plural, 3, An3, AnF).
s(s, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, s, An, AnF).
vp(vp, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, vp, An, AnF).
v(v, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, v, An, AnF).
n(n, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, n, An, AnF).
det(det, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, det, An, AnF).
p(p, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, p, An, AnF).
pp(pp, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, pp, An, AnF).
conj(conj, S, S, _, Nr1, An, AnF):- prelucreaza(Nr1, conj, An, AnF).
np(np, [], _, _, _, _, _):-!, fail.
s(s, [], _, _, _, _, _):-!, fail.
vp(vp, [], _, _, _, _, _):-!, fail.
v(v, [], _, _, _, _, _):-!, fail.
n(n, [], _, _, _, _, _):-!, fail.
det(det, [], _, _, _, _, _):-!, fail.
p(p, [], _, _, _, _, _):-!, fail.
pp(pp, [], _, _, _, _, _):-!, fail.
conj(conj, [], _, _, _, _, _):-!, fail.
np(C, S1, S, Nr, Nr1,An,AnF):- prelucreaza(Nr1, np, An, An1), parse(vp, S1, S2, Nr, An1, An2), s(C, S2, S, Nr, 2, An2, AnF).
det(C, S1, S, Nr, Nr1,An,AnF):- prelucreaza(Nr1, det, An, An1), parse(n, S1, S2, Nr, An1, An2), np(C, S2, S, Nr, 2, An2, AnF).
np(C, S1, S, Nr, Nr1,An,AnF):- prelucreaza(Nr1, np, An, An1),parse(conj, S1, S2, Nr2, An1, An2), parse(np, S2, S3, Nr2,An2,An3), np(C, S3, S, plural, 3, An3, AnF).
v(C, S1, S, Nr, Nr1,An,AnF):- prelucreaza(Nr1, v, An, An1),parse(np, S1, S2, Nr2, An1, An2), parse(pp, S2, S3, Nr3, An2, An3), vp(C, S3, S, Nr, 3, An3, AnF).
v(C, S1, S, Nr, Nr1,An,AnF):- prelucreaza(Nr1, v, An, An1),parse(np, S1, S2, Nr2, An1, An2), vp(C, S2, S, Nr, 2, An2, AnF).
p(C, S1, S, Nr, Nr1,An,AnF):- prelucreaza(Nr1, p, An, An1),parse(np, S1, S2, Nr, An1, An2), pp(C, S2, S, Nr, 2, An2, AnF).

cuvant(det, [the|X], X, Nr).
cuvant(det, [all|X], X, plural).
cuvant(det, [every|X], X, singular).
cuvant(p, [near|X], X, Nr).
cuvant(conj, [and|X], X, Nr).
cuvant(n,[cat|X],X, singular).
cuvant(n,[cats|X],X, plural).
cuvant(n,[dog|X],X, singular).
cuvant(n,[dogs|X],X, plural).
cuvant(n,[elephant|X],X, singular).
cuvant(n,[elephants|X],X, plural).
cuvant(v,[chase|X],X, plural).
cuvant(v,[chases|X],X, singular).
cuvant(v,[see|X],X, plural).
cuvant(v,[sees|X],X, singular).
cuvant(v,[amuse|X],X, plural).
cuvant(v,[amuses|X],X, singular).

legatura(X, X).
legatura(np, s).
legatura(det, np).
legatura(det, s).
legatura(v, vp).
legatura(p, pp).


parse(C, S1, S, Nr, Analiza, AnF):-   legatura(W, C),
					cuvant(W, S1, S2, Nr), [H|T]=S1, (\+ Analiza==[], Analiza1= [H|Analiza]; Analiza==[], Analiza1=[H]),
					P=..[W, C, S2, S, Nr, 1, Analiza1, AnF],
					call(P).
					
prelucreaza(1, Cat, [Elem|Rest], [[Cat, Elem]|Rest]).
prelucreaza(2, Cat, [Elem1, Elem2|Rest], [[Cat, Elem1, Elem2]|Rest]).
prelucreaza(3, Cat, [Elem1, Elem2, Elem3|Rest], [[Cat, Elem1, Elem2, Elem3]|Rest]).

start_parse(C, S1, S, Nr, Analiza, Analiza_finala):- parse(C, S1, S, Nr, Analiza, [AnF]), mwrite(AnF, Analiza_finala).

mwrite([X,A,B],[X,B1,A1]):-mwrite(A,A1),mwrite(B,B1).
mwrite([X,A,B,C],[X,C1,B1,A1]):-mwrite(A,A1),mwrite(B,B1),mwrite(C,C1).
mwrite([Elem1, Elem2], [Elem1, Elem2]).
					