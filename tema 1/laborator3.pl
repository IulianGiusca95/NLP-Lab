% - Inlocuieste un element dintr-o lista cu altul

o_aparitie(_,_,[],[]).
o_aparitie(Elem1, Elem2, [H1|T1], [Elem2|T1]):- H1==Elem1.
o_aparitie(Elem1, Elem2, [H1|T1], [H1|T2]):- \+(H1==Elem1), o_aparitie(Elem1,Elem2,T1,T2).

toate_aparitiile(_,_,[],[]).
toate_aparitiile(Elem1, Elem2, [H1|T1], [Elem2|T2]):- H1==Elem1, toate_aparitiile(Elem1, Elem2, T1, T2).
toate_aparitiile(Elem1, Elem2, [H1|T1], [H1|T2]):- \+(H1==Elem1), toate_aparitiile(Elem1, Elem2, T1, T2).

% - Inverseaza o lista

inversa(L,L_inversa):- inverseaza(L,[],L_inversa).
inverseaza([],L,L).
inverseaza([H|T],L,L_rez):- inverseaza(T,[H|L],L_rez). 

% - Genereaza toate permutarile elementelor unei liste

permuta([],[]).
permuta(L,[H|T]) :- append(V,[H|U],L), 		% L se permuta intr-o lista de tipul [H|T] daca L se poate alcatui din listele V si U, care il contin pe H(un element al listei)
					append(V,U,W), 			% si daca V si U alcatuiesc lista W(fara H)
					permuta(W,T). 			% iar T este o permutare a listei W
					
% - Determina minimul unei liste

getMin(L, Rez):- primulElement(L,Elem), determinaMin(L, Elem, Rez).
primulElement([H|_],H):- number(H).
determinaMin([],Min, Min).
determinaMin([H|T], Min, Rez_final):- number(H), H<Min, determinaMin(T,H,Rez_final).
determinaMin([H|T], Min, Rez_final):- number(H), \+(H<Min), determinaMin(T, Min, Rez_final). 

% - Determina elementul de pe o anumita pozitie

getPoz([H|_],1,H).
getPoz([_|T], Poz, Elem):- Poz>0, Poz1 is Poz-1, getPoz(T, Poz1, Elem).

% - Sa se insereze un element pe o anumita pozitie in lista

insertPoz(Elem, 1, L, [Elem|L]).
insertPoz(Elem, Poz, [H|T], [H|Lista]):- Poz>0, Poz1 is Poz-1, insertPoz(Elem, Poz1, T, Lista).

% - Sa se interclaseze doua liste cu elemente ordonate crescator

interclasare([],L,L).
interclasare(L,[],L).
interclasare([H1|T1],[H2|T2],[H1|T3]):- number(H1), number(H2), H1=<H2, interclasare(T1,[H2|T2],T3).
interclasare([H1|T1],[H2|T2],[H2|T3]):- number(H1), number(H2), H1>H2, interclasare([H1|T1],T2,T3).

% - Sa se imparta o lista in 2 liste, in functie de valoarea data (in prima elementele mai mici si in a doua elementele mai mari)

imparte([],_,[],[]).
imparte([H1|T1], Val, [H1|T2], L3):- number(Val), number(H1), H1<Val, imparte(T1, Val, T2, L3).
imparte([H1|T1], Val, L2, [H1|T3]):- number(Val), number(H1), H1>Val, imparte(T1, Val, L2, T3).
imparte([H1|T1], Val, L2, L3):- number(Val), number(H1), Val==H1, imparte(T1, Val, L2, L3).
