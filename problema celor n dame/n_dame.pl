% n_dame(0,_,_,_):- !, fail.

n_dame(Nr_dame, Linia_curenta, Dame_plasate, Dame_plasate):- Linia_curenta>Nr_dame, nl, draw_rezultat(Nr_dame, Dame_plasate), nl.  % daca ajung in situatia cand nr de linii depaseste nr de dame, inseamna ca am plasat toate damele si urmeaza sa afisez rezultatul

n_dame(Nr_dame, Linia_curenta, Dame_plasate, Rez):-Linia_curenta =< Nr_dame,                                                       % verific daca nu am depasit numarul de dame 
											  determina_pozitii_blocate(Linia_curenta, 1, Nr_dame, Dame_plasate, Pozitii_blocate), % determin ce pozitii de pe linie sunt blocate
											  predicat_for(1, Nr_dame, Pozitie),
											  isMember(Pozitie, Pozitii_blocate), 
											  add_end(Pozitie, Dame_plasate, Dame_actualizate),                                    % daca gasesc o pozitie libera, plasez dama la finalul listei de dame plasate (pozitia in lista indica si linia pe care e plasata)
											  Next_line is Linia_curenta+1,
											  n_dame(Nr_dame, Next_line, Dame_actualizate, Rez).                                   % trec la linia urmatoare
											

determina_pozitii_blocate(_, _, _, [], []).
determina_pozitii_blocate(Poz_curenta, Indice, Nr, [H|T], [H|Pozitii_blocate]):- Val1 is H-Poz_curenta+Indice,
																				 Val2 is H+Poz_curenta-Indice,
																				 Val1<1,
																				 Val2>Nr, 
																				 determina_pozitii_blocate(Poz_curenta, Indice+1, Nr, T, Pozitii_blocate).
																									
determina_pozitii_blocate(Poz_curenta, Indice, Nr, [H|T], [H, Val2|Pozitii_blocate]):- Val1 is H-Poz_curenta+Indice,
																					   Val2 is H+Poz_curenta-Indice,
																				       Val1<1,
																				       Val2=<Nr,
																				       determina_pozitii_blocate(Poz_curenta, Indice+1, Nr, T, Pozitii_blocate).
																				 
determina_pozitii_blocate(Poz_curenta, Indice, Nr, [H|T], [H, Val1|Pozitii_blocate]):- Val1 is H-Poz_curenta+Indice,
																					   Val2 is H+Poz_curenta-Indice,
																				       Val1>=1,
																				       Val2>Nr,
																				       determina_pozitii_blocate(Poz_curenta, Indice+1, Nr, T, Pozitii_blocate).
																						   
determina_pozitii_blocate(Poz_curenta, Indice, Nr, [H|T], [H, Val1, Val2|Pozitii_blocate]):- Val1 is H-Poz_curenta+Indice,
																							 Val2 is H+Poz_curenta-Indice,
																				             Val1>=1,
																				             Val2=<Nr,
																							 determina_pozitii_blocate(Poz_curenta, Indice+1, Nr, T, Pozitii_blocate).

draw_rezultat(_, []).																							 
draw_rezultat(Nr_dame, [X|T]):- draw_linie(1, Nr_dame, X), draw_rezultat(Nr_dame, T).

draw_linie(Indice, Nr, X):- Indice =< Nr, Indice == X, write('X '), Indice2 is Indice+1, draw_linie(Indice2, Nr, X).
draw_linie(Indice, Nr, X):- Indice =< Nr, \+(Indice == X), write('_ '), Indice2 is Indice+1, draw_linie(Indice2, Nr, X).
draw_linie(Indice, Nr, X):- Indice > Nr, nl.
																							 
predicat_for(Indice, Nr, Indice):- Indice =< Nr.
predicat_for(Indice, Nr, Val):- Indice =< Nr, I is Indice+1, predicat_for(I, Nr, Val).

isMember(X,[]).
isMember(X,[H|T]):- X==H,!,fail.
isMember(X,[H|T]):-isMember(X,T).

% - Insereaza un element la sfarsitul listei

add_end(X,[],[X]).
add_end(X,[H|T],[H|Rez]):-add_end(X,T,Rez).