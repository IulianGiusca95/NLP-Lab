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

muchie(Start, Final, Eticheta, Degasit, Gasit).

parse(Categorie, Sir, Analiza):-
	init_agenda(Sir, 0, Agenda),
	init_parse(Categorie, 0, Agenda, Harta),
	membru(muchie(0, M, Categorie, [], Analiza1), Harta),
	mwrite(Analiza1, Analiza),
	N is M+1, 
	\+ (membru(muchie(_, N, _, _, _), Harta)).

init_agenda([], _, []).
init_agenda([Cuvant|Cuvinte], V0, Agenda):- 
	V1 is V0 +1,
	findall(muchie(V0, V1, Categorie, [], [Categorie, Cuvant]), cuvant(Categorie, Cuvant), Agenda1),
	init_agenda(Cuvinte, V1, Agenda2),
	append(Agenda1, Agenda2, Agenda).

	
init_parse(Categorie, V0, Agenda, Harta):-
	findall(muchie(V0, V0, Categorie, Categorii, [Categorie]), regula(Categorie, Categorii), Muchii),
	%write(Muchii),
	pentru_fiecare(Muchii, Agenda, Harta).
	
pentru_fiecare([], Harta, Harta).
pentru_fiecare([H|T], Agenda, Harta):-
	adauga_muchie(H, Agenda, Harta1),
	pentru_fiecare(T, Harta1, Harta).

adauga_muchie(muchie(V1, V2, Categorie, Categorii, Analiza), Muchii, Muchii):-         % muchie existenta
	membru(muchie(V1, V2, Categorie, Categorii, Analiza), Muchii), !.
	
adauga_muchie(muchie(V1, V2, Categorie1, [], Analiza), Harta1, Harta3):-               % muchie inactiva
	Harta2=[muchie(V1, V2, Categorie1, [], Analiza)|Harta1],
	findall(muchie(V0, V2, Categorie2, Categorii, [Analiza|Analize]), membru(muchie(V0, V1, Categorie2, [Categorie1|Categorii], Analize), Harta2), Muchii),
	pentru_fiecare(Muchii, Harta2, Harta3).
	
adauga_muchie(muchie(V1, V2, Categorie1, [Categorie2|Categorii], Analize), Harta1, Harta4):-    %  muchie activa
	Harta2=[muchie(V1, V2, Categorie1, [Categorie2|Categorii], Analize)|Harta1],
	findall(muchie(V1, V3, Categorie1, Categorii, [Analiza|Analize]), membru(muchie(V2, V3, Categorie2, [], Analiza), Harta2), Muchii),
	pentru_fiecare(Muchii, Harta2, Harta3),
	init_parse(Categorie2, V2, Harta3, Harta4).
	

membru(muchie(A, B, C, D, E), [muchie(A, B, C, D, E)|T]).
membru(muchie(A1, B1, C1, D1, E1), [muchie(A2, B2, C2, D2, E2)|T]):- \+ (A1==A2, B1==B2, C1==C2, D1==D2, E1==E2), membru(muchie(A1, B1, C1, D1, E1), T).


mwrite([B, A, X], [X, A1, B1]):- mwrite(A, A1), mwrite(B, B1).
mwrite([Elem1, vp], [vp, Elem1]).
mwrite([Elem1, Elem2], [Elem1, Elem2]):- \+ Elem2 == vp.
	

	

	
