% 12) Implementati acordul dintre subiect si verb si reprezentati arborele PS
% apelare: start_parse(s, [the, cat, sees, the, dog], [], _, [], Analiza).

conj(conj, S, S, _).
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

np(C, S1, S, Nr, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, np, Analiza, Analiza_rezultata),
											parse(vp, S1, S2, Nr, Analiza_rezultata, AnF1), s(C, S2, S, Nr, 2, AnF1, AnF).                                                                  %s -> [np, vp]
det(C, S1, S, Nr,Nr_de_elem, Analiza, AnF):-prelucreaza_analiza(Nr_de_elem, det, Analiza, Analiza_rezultata),
                                            parse(n, S1, S2, Nr, Analiza_rezultata, AnF1), np(C, S2, S, Nr,2, AnF1, AnF).                                         %np -> [det, n]
np(C, S1, S, Nr,Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, np, Analiza, Analiza_rezultata),
                                            parse(conj, S1, S2, Nr2, Analiza_rezultata, AnF1), parse(np, S2, S3, Nr2, AnF1, AnF2), np(C, S3, S, plural,3, AnF2, AnF).                                  %np -> [np, conj, np]
v(C, S1, S, Nr,Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, v, Analiza, Analiza_rezultata),
										   parse(np, S1, S2, Nr2, Analiza_rezultata, AnF1), parse(pp, S2, S3, Nr3, AnF1, AnF2), vp(C, S3, S, Nr, 3, AnF2, AnF).                                         %vp -> [v, np, pp]
v(C, S1, S, Nr,Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, v, Analiza, Analiza_rezultata), 
                                           parse(np, S1, S2, Nr2, Analiza_rezultata, AnF1), vp(C, S2, S, Nr, 2, AnF1, AnF).                                                                 %vp -> [v, np]
p(C, S1, S, Nr,Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, p, Analiza, Analiza_rezultata),
                                           parse(np, S1, S2, Nr, Analiza_rezultata, AnF1), pp(C, S2, S, Nr, 2, AnF1, AnF).                                                                  %pp -> [p, np]
np(np, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, np, Analiza, AnF).
s(s, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, s, Analiza, AnF).
vp(vp, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, vp, Analiza, AnF).
v(v, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, v, Analiza, AnF).
n(n, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, n, Analiza, AnF).
det(det, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, det, Analiza, AnF).
p(p, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, p, Analiza, AnF).
pp(pp, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, pp, Analiza, AnF).
conj(conj, S, S, _, Nr_de_elem, Analiza, AnF):- prelucreaza_analiza(Nr_de_elem, conj, Analiza, AnF).

parse(C, S1, S, Nr, Analiza, AnF):- 
					cuvant(W, S1, S2, Nr), [H|T]=S1, (\+ Analiza==[], Analiza1= [H|Analiza]; Analiza==[], Analiza1=[H]),
					P=..[W, C, S2, S, Nr,1, Analiza1, AnF],
					call(P).
					
					
					
prelucreaza_analiza(1, Cat, [Elem|Rest], [[Cat, Elem]|Rest]).
prelucreaza_analiza(2, Cat, [Elem1, Elem2|Rest], [[Cat, Elem1, Elem2]|Rest]).
prelucreaza_analiza(3, Cat, [Elem1, Elem2, Elem3|Rest], [[Cat, Elem1, Elem2, Elem3]|Rest]).

start_parse(C, S1, S, Nr, Analiza, Analiza_finala):- parse(C, S1, S, Nr, Analiza, [AnF]), mwrite(AnF, Analiza_finala).

mwrite([X,A,B],[X,B1,A1]):-mwrite(A,A1),mwrite(B,B1).
mwrite([X,A,B,C],[X,C1,B1,A1]):-mwrite(A,A1),mwrite(B,B1),mwrite(C,C1).
mwrite([Elem1, Elem2], [Elem1, Elem2]).