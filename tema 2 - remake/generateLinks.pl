:- dynamic tab/2.
regula8(s, [np, vp]).
regula8(np, [det, n]).
regula8(np, [np, conj, np]).
regula8(vp, [v, np]).
regula8(vp, [v, np, pp]). % - pp optional
regula8(pp, [p, np]).
regula8(det, []).


generate_links:- retractall(tab(_,_)),
				 regula8(C, X), write(regula8(C, X)),nl,
				 adauga(C, C),
				 prelucreaza_regula(C, X),
				 fail.

prelucreaza_regula(C, []).
prelucreaza_regula(C, [H|T]):- adauga(H, C), write(['parcurge ',[H|T]]), nl,
                               parcurge([H|T]),
							   cauta(H, C),
							   verifica(C, [H|T]).                    %in cazul in care capul unei reguli poate fi nul, urmatorul element al regulii poate fi cap de legatura
							   
verifica(_, _).
verifica(C, [H|T]):- regula8(H, []), prelucreaza_regula(C, T).

parcurge([]).
parcurge([H|T]):- adauga(H,H), parcurge(T).							   
				 
				 
adauga(X, Y):- tab(X, Y).
adauga(X, Y):- \+ tab(X, Y), assert(tab(X, Y)).

cauta(Const, Const_principal).
cauta(Const, Const_principal):- regula8(Next, [Const_principal|_]),
                                adauga(Const, Next).
				 
afiseaza_tabel(X, Y):-tab(X, Y), write('legatura('), write(X), write(', '), write(Y), write(')'), nl.