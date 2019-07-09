% - 1) Parser top-down. Implementati acordul. Afisati arborele de derivare
% Apelare: parse(s, [o, fata, citeste], [], _, Analiza).
% extind gramatica
regula(s, [np, vp], Nr).
regula(vp, [v], Nr).
regula(vp, [v, np], Nr).
regula(np, [det, n], Nr).                                     
cuvant(det, o, singular).
cuvant(det, niste, plural).
cuvant(n, fata, singular).
cuvant(n, fete, plural).
cuvant(n, carti, plural).
cuvant(n, carte, singular).
cuvant(v, citeste, singular).
cuvant(v, citesc, plural).


parse(C,[Cuvant|S],S, Nr, [C, Cuvant]):- cuvant(C, Cuvant, Nr).
parse(C, S1, S, Nr, [C|Analiza]):- regula(C, Cs, Nr), parse_lista(Cs, S1, S, Nr, Analiza).

parse_lista([v, np], S1, S, Nr, [Analiza|Analize]):- parse(v, S1, S2, Nr, Analiza), parse_lista([np], S2, S, Nr2, Analize).
parse_lista([C|Cs], S1, S, Nr, [Analiza|Analize]):- (\+ [C|Cs] == [v, np]), parse(C, S1, S2, Nr, Analiza), parse_lista(Cs, S2, S, Nr, Analize).
parse_lista([], S, S, _, []).


% - 2) Acelasi parser, modificat ptr a afisa intotdeauna arborele de derivare(cand regulile nu contin argumente)
% Apelare: parse2(s, [un, baiat, citeste], [], Analiza).
% extind gramatica
regula2(s, [np, vp]).
regula2(vp, [v]).
regula2(vp, [v, np]).
regula2(np, [det, n]).                                     
cuvant2(det, o).
cuvant2(det, niste).
cuvant2(n, fata).
cuvant2(n, fete).
cuvant2(n, carti).
cuvant2(n, carte).
cuvant2(v, citeste).
cuvant2(v, citesc).


parse2(C,[Cuvant|S],S, [C, Cuvant]):- cuvant2(C, Cuvant).
parse2(C, S1, S, [C|Analiza]):- regula2(C, Cs), parse_lista2(Cs, S1, S, Analiza).

parse_lista2([C|Cs], S1, S, [Analiza|Analize]):- parse2(C, S1, S2, Analiza), parse_lista2(Cs, S2, S, Analize).
parse_lista2([], S, S, []).

% - 3) Parser bottom up. Implementati acordul dintre verb si subiect. Sa se reprezinte arborele.
%  Apelare: parse([o, fata, citeste, niste, carti], [s], A).

regula3([vp, np|X], [s|X], Nr).
regula3([n, art|X], [np|X], Nr).
regula3([v|X],[vp|X], Nr).
regula3([np, v|X], [vp|X], Nr).

regula3([o|X], [art|X], singular).
regula3([niste|X], [art|X], plural).
regula3([fata|X], [n|X], singular).
regula3([fete|X], [n|X], plural).
regula3([carte|X], [n|X], singular).
regula3([carti|X], [n|X], plural).
regula3([citeste|X], [v|X], singular).
regula3([citesc|X], [v|X], plural).


parse3(S, Rezultat, Analiza):- depl_red3(S,[],Rezultat, [], Analiza, 1, Nr).

depl_red3(S, Stiva, Rezultat, Analiza_pas_anterior, AnF, Caz_anterior, Nr):- deplasare3(Stiva, S, Stiva_noua, S1, Element),
													  (\+ Analiza_pas_anterior==[],(Caz_anterior==1,Analiza_curenta=[Element|[Analiza_pas_anterior]];Caz_anterior==2,Analiza_curenta=[Element|Analiza_pas_anterior]); Analiza_pas_anterior==[],Analiza_curenta=[Element]),
													  reducere3(Stiva_noua, Stiva_redusa, Analiza_curenta, Analiza, Caz, Nr),
													  (regula3([Element|X],[v|X], Nr), \+S1==[],
													  depl_red3(S1, Stiva_redusa, Rezultat, Analiza, AnF, Caz, Nr1);
													  (regula3([Element|X],[v|X], Nr), S1==[], depl_red3(S1, Stiva_redusa, Rezultat, Analiza, AnF, Caz, Nr);
													  \+regula3([Element|X],[v|X], Nr), depl_red3(S1, Stiva_redusa, Rezultat, Analiza, AnF, Caz, Nr))
												      ).

depl_red3([], Rezultat, Rezultat, Analiza, Analiza_finala, _, Nr):-mwrite3(Analiza, Analiza_finala).

deplasare3(X, [H|Y], [H|X], Y, H).



reducere3(Stiva, Stiva_redusa, Analiza_curenta, AnF, Caz, Nr):-
													   regula3(Stiva, Stiva1, Nr),
													   prelucrare3(Stiva, Stiva1, Analiza_curenta, Analiza_prelucrata),
													   reducere3(Stiva1, Stiva_redusa, Analiza_prelucrata, AnF, Caz, Nr).
													   

reducere3(Stiva, Stiva, [H|T], H, 1, Nr):- T==[].
reducere3(Stiva, Stiva, [H|T], [H|T], 2, Nr):- \+ T==[].

prelucrare3([H1|T],[Categ|T],[Elem|Rest],[[Categ, Elem]|Rest]).
prelucrare3([H1, H2|T],[Categ|T],[Elem1, Elem2|Rest],[[Categ, Elem1, Elem2]|Rest]).

mwrite3([X,A,B],[X,B1,A1]):-mwrite3(A,A1),mwrite3(B,B1).
mwrite3([Elem1, Elem2], [Elem1, Elem2]).



% - 4) Parser din coltul stang
% Apelare: parse4(s, [the, cat, sees, the, elephant], []).
regula4(s, [np, vp]).
regula4(np, [det, n]).
regula4(np, [np, conj, np]).
regula4(vp, [v, np]).
regula4(vp, [v, np, pp]). % - pp optional
regula4(pp, [p, np]).
%cuvant4(det, []).
cuvant4(det, the).
cuvant4(det, all).
cuvant4(det, every).
cuvant4(p, near).
cuvant4(conj, 'and').
cuvant4(n, dog).
cuvant4(n, dogs).
cuvant4(n, cat).
cuvant4(n, cats).
cuvant4(n, elephant).
cuvant4(n, elephants).
cuvant4(v, chase).
cuvant4(v, chases).
cuvant4(v, see).
cuvant4(v, sees).
cuvant4(v, amuse).
cuvant4(v, amuses).


parse4(C,[Cuvant|S2], S):- cuvant4(W, Cuvant), 
						   completeaza4(W, C, S2, S).
						   
completeaza4(C, C, S, S).
completeaza4(W, C, S1, S):- regula4(P, [W|Rest]),
							parse_lista4(Rest, S1, S2),
							completeaza4(P, C, S2, S).
							
parse_lista4([C|Cs], S1, S):- parse4(C, S1, S2), parse_lista4(Cs, S2, S).
parse_lista4([], S, S).
parse4(C, S2, S):- regula4(W, []),
				   completeaza4(W, C, S2, S).



% - 5) Implementati acordul dintre verb si subiect. Reprezinta arborele de derivare
% Apelare: parse5(s, [the, cat, sees, the, elephant], [], _, Analiza).

regula5(s, [np, vp], Nr).
regula5(np, [det, n], Nr).
regula5(np, [np, conj, np], plural).
regula5(vp, [v, np], Nr).
regula5(vp, [v, np, pp], Nr). % - pp optional
regula5(pp, [p, np], Nr).
%cuvant5(det, []).
cuvant5(det, the, singular).
cuvant5(det, the, plural).
cuvant5(det, all, plural).
cuvant5(det, every, singular).
cuvant5(p, near, singular).
cuvant5(p, near, plural).
cuvant5(conj, 'and', singular).
cuvant5(conj, 'and', plural).
cuvant5(n, dog, singular).
cuvant5(n, dogs, plural).
cuvant5(n, cat, singular).
cuvant5(n, cats, plural).
cuvant5(n, elephant, singular).
cuvant5(n, elephants, plural).
cuvant5(v, chase, plural).
cuvant5(v, chases, singular).
cuvant5(v, see, plural).
cuvant5(v, sees, singular).
cuvant5(v, amuse, plural).
cuvant5(v, amuses, singular).


parse5(C,[Cuvant|S2], S, Nr, Analiza):- 
							   cuvant5(W, Cuvant, Nr),
						       completeaza5(W, C, S2, S, Nr, [W, Cuvant], Analiza).
parse5(pp, [Cuvant|S2], S, Nr, Analiza):- cuvant5(W, Cuvant, Nr),
                                 completeaza5(W, C, S2, S, Nr2, [W, Cuvant], Analiza).
						   
completeaza5(C, C, S, S, _, Analiza, Analiza).
completeaza5(v, C, S1, S, Nr, Componenta, AnF):- regula5(vp, [v|Rest], Nr),              % tratam caz particular verb si rest de numar diferit
								parse_lista5(Rest, S1, S2, Nr2, Analiza),
								Componente=[Componenta|Analiza],
							    Comp_final=[vp|Componente],
								completeaza5(vp, C, S2, S, Nr3, Comp_final, AnF).
completeaza5(np, C, S1, S, Nr, Componenta, AnF):- regula5(np, [np, conj, np], plural),   % tratam cazul particular np conj np - mereu plural
								parse_lista5([conj, np], S1, S2, Nr2, Analiza), 
								Componente=[Componenta|Analiza],
							    Comp_final=[np|Componente],
								completeaza5(np, C, S2, S, plural, Comp_final, AnF).
completeaza5(W, C, S1, S, Nr, Componenta, AnF):- regula5(P, [W|Rest], Nr), \+ (P==vp, W==v), \+ (P==np, [W|Rest]==[np, conj, np]),
							parse_lista5(Rest, S1, S2, Nr, Analiza),
							Componente=[Componenta|Analiza],
							Comp_final=[P|Componente],
							completeaza5(P, C, S2, S, Nr, Comp_final, AnF).

						
parse_lista5([C|Cs], S1, S, Nr, [Analiza|Analize]):- parse5(C, S1, S2, Nr, Analiza), parse_lista5(Cs, S2, S, Nr, Analize).
parse_lista5([], S, S, _, []).
parse5(C, S2, S, Nr):- regula5(W, [], _),
				   completeaza5(W, C, S2, S, _).

				   
				   
% - 6) Arborele de derivare
% Apelare: parse6(s, [the, cat, sees, the, elephant], []).

regula6(s, [np, vp]).
regula6(np, [det, n]).
regula6(np, [np, conj, np]).
regula6(vp, [v, np]).
regula6(vp, [v, np, pp]). % - pp optional
regula6(pp, [p, np]).
%cuvant6(det, []).
cuvant6(det, the).
cuvant6(det, all).
cuvant6(det, every).
cuvant6(p, near).
cuvant6(conj, 'and').
cuvant6(n, dog).
cuvant6(n, dogs).
cuvant6(n, cat).
cuvant6(n, cats).
cuvant6(n, elephant).
cuvant6(n, elephants).
cuvant6(v, chase).
cuvant6(v, chases).
cuvant6(v, see).
cuvant6(v, sees).
cuvant6(v, amuse).
cuvant6(v, amuses).


parse6(C,[Cuvant|S2], S, Analiza):- cuvant6(W, Cuvant),
						   completeaza6(W, C, S2, S, [W, Cuvant], Analiza).
						   
completeaza6(C, C, S, S, Analiza, Analiza).
completeaza6(W, C, S1, S, Componenta, AnF):- regula6(P, [W|Rest]),
							parse_lista6(Rest, S1, S2, Analiza), 
							Componente=[Componenta|Analiza],
							Comp_final=[P|Componente],
							completeaza6(P, C, S2, S, Comp_final, AnF).
							
parse_lista6([C|Cs], S1, S, [Analiza|Analize]):- parse6(C, S1, S2, Analiza), parse_lista6(Cs, S2, S, Analize).
parse_lista6([], S, S, []).
parse6(C, S2, S):- regula6(W, []),
				   completeaza6(W, C, S2, S).

				   
% - 7) Rescrieti astfel incat sa se sara peste constituentul pp fara a incerca sa il analizeze
% - Apelare(parse7, s[the, cat, sees, the, dog], [], Analiza).

regula7(s, [np, vp]).
regula7(np, [det, n, pp]).
regula7(vp, [v, np]).
regula7(vp, [v, np, pp]). % - pp optional
regula7(pp, []).
cuvant7(det, the).
cuvant7(det, all).
cuvant7(det, every).
cuvant7(p, near).
cuvant7(conj, 'and').
cuvant7(n, dog).
cuvant7(n, dogs).
cuvant7(n, cat).
cuvant7(n, cats).
cuvant7(n, elephant).
cuvant7(n, elephants).
cuvant7(v, chase).
cuvant7(v, chases).
cuvant7(v, see).
cuvant7(v, sees).
cuvant7(v, amuse).
cuvant7(v, amuses).

parse7(C,[Cuvant|S2], S, Analiza):- cuvant7(W, Cuvant), 
						   completeaza7(W, C, S2, S, [W, Cuvant], Analiza).
						   
completeaza7(C, C, S, S, Analiza, Analiza).

completeaza7(v, C, S1, S, Componenta, AnF):- regula7(vp, [v, np, pp]),
							parse_lista7([np, pp], S1, S2, Analiza),
							Componente=[Componenta|Analiza],
							Comp_final=[vp|Componente],
							completeaza7(vp, C, S2, S, Comp_final, AnF).
											 

completeaza7(W, C, S1, S, Componenta, AnF):- \+ W == v, regula7(P, [W|Rest]),
							parse_lista7(Rest, S1, S2, Analiza),
							Componente=[Componenta|Analiza],
							Comp_final=[P|Componente],
							completeaza7(P, C, S2, S, Comp_final, AnF).
							

parse_lista7([], S, S, []).
parse_lista7([pp|Cs], S1, S, Analiza):-parse_lista7(Cs, S1, S, Analiza).						
parse_lista7([C|Cs], S1, S, [Analiza|Analize]):- \+ C==pp, parse7(C, S1, S2, Analiza), parse_lista7(Cs, S2, S, Analize).
	

parse7(C, S2, S, Analiza):- regula7(W, []),
				   completeaza7(W, C, S2, S, [], Analiza).
				   
% - 8) Cicleaza

% - 9) Construiti tabela de legaturi pentru gramatica. Adaugati tabela de legaturii parserului din coltul stang.
% Apelare parse8(s, X, []).
regula8(s, [np, vp]).
regula8(np, [det, n]).
regula8(np, [np, conj, np]).
regula8(vp, [v, np]).
regula8(vp, [v, np, pp]). % - pp optional
regula8(pp, [p, np]).
regula8(det, []).
cuvant8(det, the).
cuvant8(det, all).
cuvant8(det, every).
cuvant8(p, near).
cuvant8(conj, 'and').
cuvant8(n, dog).
cuvant8(n, dogs).
cuvant8(n, cat).
cuvant8(n, cats).
cuvant8(n, elephant).
cuvant8(n, elephants).
cuvant8(v, chase).
cuvant8(v, chases).
cuvant8(v, see).
cuvant8(v, sees).
cuvant8(v, amuse).
cuvant8(v, amuses).

legatura8(np, s).
legatura8(det, s).
legatura8(det, np).
legatura8(n, s).
legatura8(n, np).
legatura8(v, vp).
legatura8(p, pp).
legatura8(X, X).


parse8(C, [Cuvant|S2], S):- cuvant8(W, Cuvant),
							legatura8(W, C),
							completeaza8(W, C, S2, S).

parse8(C, S2, S):- regula8(W, []),
				   legatura8(W, C),
				   completeaza8(W, C, S2, S).

completeaza8(C, C, S, S).
completeaza8(C, C, [], _):- !, fail.
completeaza8(W, C, S1, S):- regula8(P, [W|Rest]),
							parse_lista8(P, Rest, S1, S2),
							completeaza8(P, C, S2, S).
							
completeaza8(W, C, S1, S):- legatura8(W, np), W == n,
							regula8(np, [np, conj, np]),
							parse_lista8(np, [conj, np], S1, S2),
							completeaza8(np, C, S2, S).

parse_lista8(Cap, [Cap|Cs], S1, S):- parse8(Cap, S1, S).							
parse_lista8(Cap, [C|Cs], S1, S):- \+ Cap == C, parse8(C, S1, S2), parse_lista8(Cap, Cs, S2, S).
parse_lista8(_, [], S, S).











