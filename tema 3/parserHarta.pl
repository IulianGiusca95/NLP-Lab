regula(s, [np, vp]).
regula(vp, [v]).
regula(vp, [v, np]).
regula(np, [det, n]).                                     
cuvant(det, o).
cuvant(det, niste).
cuvant(n, fata).
cuvant(n, fete).
cuvant(n, carti).
cuvant(n, carte).
cuvant(v, citeste).
cuvant(v, citesc).

muchie(Start, Final, Eticheta, Degasit).

init_agenda([], _, []).
init_agenda([Cuvant|Cuvinte], V0, Agenda):- 
	V1 is V0 +1,
	findall(muchie(V0, V1, Categorie, []), cuvant(Categorie, Cuvant), Agenda1),
	init_agenda(Cuvinte, V1, Agenda2),
	append(Agenda1, Agenda2, Agenda).
	
append([], A, A).
append([H|T], Agenda, [H|Agenda2]):- append(T, Agenda, Agenda2).

extinde_muchii([], Harta, Harta).
extinde_muchii([Muchie|Agenda1], Harta1, Harta2):-
	membru(Muchie, Harta1), !,                   
	extinde_muchii(Agenda1, Harta1, Harta2).
extinde_muchii([Muchie|Agenda1], Harta1, Harta3):-
	Harta2=[Muchie|Harta1],
	muchii_noi(Muchie, Harta2, Muchii),
	adauga_muchii(Muchii, Agenda1, Agenda2),
	extinde_muchii(Agenda2, Harta2, Harta3).
	
membru(muchie(A, B, C, D), [muchie(A, B, C, D)|T]).
membru(muchie(A1, B1, C1, D1), [muchie(A2, B2, C2, D2)|T]):- \+ (A1==A2, B1==B2, C1==C2, D1==D2), membru(muchie(A1, B1, C1, D1), T).



adauga_muchie(Muchie, Muchii, Muchii):- membru(Muchie, Muchii), !.
adauga_muchie(Muchie, Muchii, [Muchie|Muchii]).
adauga_muchii([], Muchii, Muchii).
adauga_muchii([Muchie|Muchii], Muchii1, Muchii3):-
	adauga_muchie(Muchie, Muchii1, Muchii2),
	adauga_muchii(Muchii, Muchii2, Muchii3). 
	
muchii_noi(muchie(V1, V2, Categorie1, []), Harta, Muchii):-
	findall(muchie(V1, V1, Categorie2, [Categorie1|Categorii]), regula(Categorie2, [Categorie1|Categorii]), Muchii1),
	findall(muchie(V0, V2, Categorie3, Categorii2), membru(muchie(V0, V1, Categorie3, [Categorie1|Categorii2]), Harta), Muchii2),
	adauga_muchii(Muchii1, Muchii2, Muchii).
muchii_noi(muchie(V1, V2, Categorie1, [Categorie2|Categorii]), Harta, Muchii):-
	findall(muchie(V1, V3, Categorie1, Categorii), membru(muchie(V2, V3, Categorie2, []), Harta), Muchii).
	
parse(Cat, Sir):-
	init_agenda(Sir, 0, Agenda),
	extinde_muchii(Agenda, [], Harta), write(['Harta ',Harta]),nl,
	membru(muchie(0, M, Cat, []), Harta),write('ho ho ho'),nl,
	N is M+1, \+ (membru(muchie(_, N, _, _), Harta)).