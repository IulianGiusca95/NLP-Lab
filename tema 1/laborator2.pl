% - Concateneaza doua liste

concateneaza([],L,L).
concateneaza([H1|T1], L2, [H1|T3]):-concateneaza(T1,L2,T3). 

% - Verifica daca o lista este multime, nu exista duplicate

checkSet([]).
checkSet([H|T]):-isMember(H,T),checkSet(T).

isMember(X,[]).
isMember(X,[H|T]):- X==H,!,fail.
isMember(X,[H|T]):-isMember(X,T).

% - Transforma o lista in multime

listToSet([],[]).
listToSet([H|T1],[H|T2]):-isMember(H,T1),!,listToSet(T1,T2).
listToSet([H|T1],T2):-listToSet(T1,T2).

% - Intersectia, reuniunea, diferenta a doua multimi

intersectie([],_,[]).
intersectie([H1|T1],M2,[H1|T3]):- \+(isMember(H1,M2)),!,intersectie(T1,M2,T3).
intersectie([H1|T1],M2,T3):-intersectie(T1,M2,T3).

reuniune([],M2,M2).
reuniune([H1|T1],M2,[H1|T3]):-isMember(H1,M2),!,reuniune(T1,M2,T3).
reuniune([H1|T1],M2,T3):-reuniune(T1,M2,T3).

diferenta([],_,[]).
diferenta([H1|T1],M2,[H1|T3]):- isMember(H1,M2),!,diferenta(T1,M2,T3).
diferenta([H1|T1],M2,T3):- diferenta(T1,M2,T3).

% - Insereaza un element la inceputul listei daca nu exista deja in lista

insereazaLaInceput(Element,L,[Element|L]):-isMember(Element,L),!.
insereazaLaInceput(Element,L,L).

% - Sterge un element din lista

stergeElement(_,[],[]).
stergeElement(Element, [H|T], ListaRezultat):-H==Element,!,stergeElement(Element,T,ListaRezultat).
stergeElement(Element, [H|T], [H|ListaRezultat]):-stergeElement(Element,T,ListaRezultat).

% - Sa calculeze factorial, fib si cmmdc

fact(0,1).
fact(X,Rez):- number(X),X>0,Y is X-1, fact(Y,Val), Rez is X*Val.

fib(0,0).
fib(1,1).
fib(N,Val):- number(N),N>1, X1 is N-1, X2 is N-2, fib(X1,Val1), fib(X2, Val2), Val is Val1+Val2.

cmmdc(X,0,Cmmdc):- Cmmdc is X,!.
cmmdc(X,Y,Cmmdc):- number(X),number(Y), Z is mod(X,Y), X1 is Y, Y1 is Z, cmmdc(X1,Y1,Cmmdc).

% - Sa se determine cate elemente are o lista

nrDeElemente([],0).
nrDeElemente([H|T],Rez):-nrDeElemente(T,Rez1), Rez is Rez1+1.

% - Sa se calculeze suma elementelor pozitive dintr-o lista de intregi

sumaElemPoz([],0).
sumaElemPoz([H|T],Suma):- number(H),H>0,!,sumaElemPoz(T,Suma1),Suma is Suma1+H.
sumaElemPoz([H|T],Suma):- sumaElemPoz(T,Suma).