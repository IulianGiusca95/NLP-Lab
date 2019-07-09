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

parse(Categorie, Sir):-
	init_agenda(Sir, 0, Agenda), 
	init_parse(Categorie, 0, Agenda, Harta),
	membru(muchie(0, M, Categorie, []), Harta),
	nl,nl,
	write(['Harta este ', Harta]),
	N is M+1, 
	\+ (membru(muchie(_, N, _, _), Harta)).
	
init_parse(Categorie, V0, Agenda, Harta):-
	findall(muchie(V0, V0, Categorie, Categorii), regula(Categorie, Categorii), Muchii),
	%write(Muchii),
	pentru_fiecare(Muchii, Agenda, Harta).
	
pentru_fiecare([], Harta, Harta).
pentru_fiecare([H|T], Agenda, Harta):-
	write(['Suntem la muchia ', H]),nl,
	adauga_muchie(H, Agenda, Harta1),
	pentru_fiecare(T, Harta1, Harta).

adauga_muchie(muchie(V1, V2, Categorie, Categorii), Muchii, Muchii):-
	membru(muchie(V1, V2, Categorie, Categorii), Muchii),
	write(['Deja exista muchia ', V1,V2, Categorie , Categorii]),nl.
	
adauga_muchie(muchie(V1, V2, Categorie1, []), Harta1, Harta3):-
	Harta2=[muchie(V1, V2, Categorie1, [])|Harta1],
	findall(muchie(V0, V2, Categorie2, Categorii), membru(muchie(V0, V1, Categorie2, [Categorie1|Categorii]), Harta2), Muchii),
	pentru_fiecare(Muchii, Harta2, Harta3).
	
adauga_muchie(muchie(V1, V2, Categorie1, [Categorie2|Categorii]), Harta1, Harta4):-
	Harta2=[muchie(V1, V2, Categorie1, [Categorie2|Categorii])|Harta1],
	findall(muchie(V1, V3, Categorie1, Categorii), membru(muchie(V2, V3, Categorie2, []), Harta2), Muchii),
	pentru_fiecare(Muchii, Harta2, Harta3),
	write('YAHOOOOOOOOOOOOOOOO'),nl,
	init_parse(Categorie2, V2, Harta3, Harta4).
	
	
	

	
	
membru(muchie(A, B, C, D), [muchie(A, B, C, D)|T]).
membru(muchie(A1, B1, C1, D1), [muchie(A2, B2, C2, D2)|T]):- \+ (A1==A2, B1==B2, C1==C2, D1==D2), membru(muchie(A1, B1, C1, D1), T).



	

	

	
