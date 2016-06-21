%Ejemplo de las entradas y resultados:
%Entrada: El niño dibujó una flor.
%Salida: Una flor fue dibujada por el niño.
%Entrada: El niño dibujó una flor en el cuaderno.
%Salida: Una flor fue dibujada por el niño en el cuaderno.
%Entrada: Yo tomé la decisión.
%Salida: La decisión fue tomada por mí.

pregunta:-
		read(Frase),
		transformacion(Pasiva,Frase,[]),
		write(Pasiva).

transformacion([Objeto,Ser,Participio,por,Suj_P]) --> grupo_nom(Per,Num,_,_,Suj_P), verbo(Per,Num,Per2,Gen2,Num2,_,Ser,Participio),grupo_nom(Per2,Num2,Gen2,Objeto,_).
transformacion([Objeto,Ser,Participio,por,Suj_P,Prep,Compl]) --> grupo_nom(Per,Num,_,_,Suj_P), verbo(Per,Num,Per2,Gen2,Num2,[Prep],Ser,Participio),grupo_nom(Per2,Num2,Gen2,Objeto,_), complemento([Prep],Compl).


%transformacion([Det_2,Sust_2,Ser,Participio,por,Suj_P]) --> grupo_nom(Per,Num,_,_,Suj_P), verbo(Per,Num,Per2,Gen2,Num2,Ser,Participio),grupo_nom_l(Per2,Num2,Gen2,Det_2,Sust_2).
%transformacion([Objeto,Ser,Participio,por,Det_1,Sust_1]) --> grupo_nom_l(Per,Num,_,Det_1,Sust_1), verbo(Per,Num,Per2,Gen2,Num2,Ser,Participio),grupo_nom(Per2,Num2,Gen2,Objeto,_).
%transformacion([Det_2,Sust_2,Ser,Participio,por,Det_1,Sust_1]) --> grupo_nom_l(Per,Num,_,Det_1,Sust_1), verbo(Per,Num,Per2,Gen2,Num2,Ser,Participio),grupo_nom_l(Per2,Num2,Gen2,Det_2,Sust_2).


complemento(Prep,Compl) --> preposicion(Prep), grupo_nom(_,_,_,Compl,_).

grupo_nom(3,sing,Gen,N,N) --> [N],
{
	nombre_propio(N,Gen)
}.

grupo_nom(Per,Num,Gen,P,Suj_P) --> [P],
{
	pronombre(Per,Num,Gen,P,Suj_P)
}.

grupo_nom(3,Num,Gen,[D,S],[D,S]) --> [D,S],
{
	det(Num,Gen,D),
	atom_concat(Lexema, Morfema, S),
	lexema(Lexema),
	morfema(Num, Gen, Morfema)
}.

grupo_nom(3,Num,Gen,[D,S],[D,S]) --> [D,S],
{
	det(Num,Gen,D),
	sustantivo(Gen,Num,S)
}.

verbo(Per,Num,Per2,Gen2,Num2,Prep,Ser,Participio) --> [V],
{
	atom_concat(Raiz, Morfema, V),
	raiz(Raiz,Conj,Prep),
	conjugation(Tiempo, Conj, Per, Num, Morfema),
	ser(Tiempo, Per2, Num2, Ser),
	partic(Num2,Gen2,Part),
	atom_concat(Raiz, Part, Participio)
}.

%si en la posicion de la prep ponemos todas significa que el verbo acepta todas las preposiciones, dara por valida sea la que sea.
verbo(Per,Num,Per2,Gen2,Num2,[_],Ser,Participio) --> [V],
{
	atom_concat(Raiz, Morfema, V),
	raiz(Raiz,Conj,todas),
	conjugation(Tiempo, Conj, Per, Num, Morfema),
	ser(Tiempo, Per2, Num2, Ser),
	partic(Num2,Gen2,Part),
	atom_concat(Raiz, Part, Participio)
}.

preposicion([P]) --> [P],
{
	preposicion(P)
}.



%----------------------------------------------------------------------------------------
%  NOMBRES PROPIOS
%----------------------------------------------------------------------------------------

nombre_propio(pedro,masc).
nombre_propio(jorge,masc).
nombre_propio(ana,fem).


%----------------------------------------------------------------------------------------
%  DETERMINANTES
%----------------------------------------------------------------------------------------

det(sing,masc,un).
det(sing,fem,una).
det(plu,masc,unos).
det(plu,fem,unas).

det(sing,masc,el).
det(sing,fem,la).
det(plu,masc,los).
det(plu,fem,las).


%----------------------------------------------------------------------------------------
%  SUSTANTIVOS
%----------------------------------------------------------------------------------------
%lexemas de los sustantivos que tienen genero y numero
lexema(nin).

%estos morfemas tambien valen para el participio usado en la forma pasiva
morfema(sing,masc,o).
morfema(sing,fem,a).
morfema(plu,masc,os).
morfema(plu,fem,as).

%lexema de los que no tienen las mismas flexiones de genero y numero o solo tienen una forma
sustantivo(fem,sing,flor).
sustantivo(fem,plu,flores).
sustantivo(fem,sing,decision).
sustantivo(masc,sing,cuaderno).


%----------------------------------------------------------------------------------------
%  PRONOMBRES
%----------------------------------------------------------------------------------------
%Cada pronombre tiene su persona su numero su genero y despues su voz activa y pasiva

pronombre(1,sing,masc,yo,mi).
pronombre(1,sing,fem,yo,mi).
pronombre(2,sing,masc,tu,ti).
pronombre(2,sing,fem,tu,ti).
pronombre(3,sing,masc,el,el).
pronombre(3,sing,masc,ella,ella).

pronombre(1,plu,masc,nosotros,nosotros).
pronombre(1,plu,fem,nosotros,nosotros).
pronombre(2,plu,masc,vosotros,vosotros).
pronombre(2,plu,fem,vosotros,vosotros).
pronombre(3,plu,masc,ellos,ellos).
pronombre(3,plu,masc,ellas,ellas).

%----------------------------------------------------------------------------------------
%  VERBOS
%----------------------------------------------------------------------------------------

%Raices con sus complementos y el numero indica su conjugacion, en caso de que admita todas las prep escribir todas en el lugar de las prep
raiz(dibuj,1,[en]).
raiz(tom,1,[]).


%Detalles de la conjugacion del verbo: tiempo, conjugacion, persona, numero, y forma del lexema
conjugation(presente,1, 1 , sing, o).
conjugation(presente,1, 2 , sing, as).
conjugation(presente,1, 3 , sing, a).
conjugation(presente,1, 1 , plu, amos).
conjugation(presente,1, 2 , plu, ais).
conjugation(presente,1, 3 , plu, an).

conjugation(pasado_perf,1, 1 , sing, e).
conjugation(pasado_perf,1, 2 , sing, aste).
conjugation(pasado_perf,1, 3 , sing, o).
conjugation(pasado_perf,1, 1 , plu, amos).
conjugation(pasado_perf,1, 2 , plu, asteis).
conjugation(pasado_perf,1, 3 , plu, aron).

partic(sing,masc,ado).
partic(sing,fem,ada).
partic(plu,masc,ados).
partic(plu,fem,adas).
%----------------------------------------------------------------------------------------
%  FORMAS DEL VERBO SER
%----------------------------------------------------------------------------------------

ser(presente, 1 , sing, soy).
ser(presente, 2 , sing, eres).
ser(presente, 3 , sing, es).
ser(presente, 1 , plu, somos).
ser(presente, 2 , plu, sois).
ser(presente, 3 , plu, son).

ser(pasado_perf, 1 , sing, fui).
ser(pasado_perf, 2 , sing, fuiste).
ser(pasado_perf, 3 , sing, fue).
ser(pasado_perf, 1 , plu, fuimos).
ser(pasado_perf, 2 , plu, fuisteis).
ser(pasado_perf, 3 , plu, fueron).

%----------------------------------------------------------------------------------------
%  FORMAS DEL VERBO SER
%----------------------------------------------------------------------------------------

preposicion(en).