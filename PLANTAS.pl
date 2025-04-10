plantaUsosMedicinales(regaliz, [tos, dolorGarganta, problemasDigestivos]).
plantaUsosMedicinales(retama, [hipertension, diuretico, reumatismo]).
plantaUsosMedicinales(ricino, [estreñimiento, cuidadoPiel, inflamacion]).
plantaUsosMedicinales(rosal, [relajante, digestivo, cuidadoPiel]).
plantaUsosMedicinales(romero, [memoria, digestion, fatiga]).

caracteristicasPlanta(regaliz, [antiinflamatorio, expectorante, digestivo]).
caracteristicasPlanta(retama, [diuretico, hipotensor, depurativo]).
caracteristicasPlanta(ricino, [laxante, hidratante, antiinflamatorio]).
caracteristicasPlanta(rosal, [relajante, astringente, antioxidante]).
caracteristicasPlanta(romero, [estimulante, digestivo, antioxidante]).

componentesQuimicos(regaliz, [saponinas, flavonoides, glicirricina]).
componentesQuimicos(retama, [alcaloides, flavonoides]).
componentesQuimicos(ricino, [acidoRicinoleico, proteinas, vitaminaE]).
componentesQuimicos(rosal, [taninos, vitaminaC, aceitesEsenciales]).
componentesQuimicos(romero, [acidoRosmarinico, flavonoides, aceitesEsenciales]).

metodoElaboracion(cocimiento, 'hervir 40 minutos y dejar reposar 5 minutos').
metodoElaboracion(infusion, 'verter agua caliente sobre las hierbas y dejar reposar').
metodoElaboracion(maceracion, 'dejar reposar en agua fria por varias horas').

formaPrepararPlanta(regaliz, infusion).
formaPrepararPlanta(retama, cocimiento).
formaPrepararPlanta(ricino, maceracion).
formaPrepararPlanta(rosal, infusion).
formaPrepararPlanta(romero, infusion).

modoConsumo(regaliz, 'beber como infusion en pequenas cantidades').
modoConsumo(retama, 'beber en cocimiento con precaucion por su toxicidad').
modoConsumo(ricino, 'usar en forma de aceite para la piel o como laxante').
modoConsumo(rosal, 'beber como infusion o aplicar en la piel').
modoConsumo(romero, 'beber como infusion o usar en aceites esenciales').

obtenerElaboracion(Especie, Metodo, Instrucciones):-
    formaPrepararPlanta(Especie, Metodo),
    metodoElaboracion(Metodo, Instrucciones).

detallarDatosPlanta(Especie):-
    (   plantaUsosMedicinales(Especie, Usos)
    ->  atomics_to_string(Usos, ', ', UsosTexto),
        obtenerElaboracion(Especie, Metodo, Instrucciones),
        (   caracteristicasPlanta(Especie, Propiedades) -> atomics_to_string(Propiedades, ', ', PropiedadesTexto) ; PropiedadesTexto = 'No disponible'),
        (   componentesQuimicos(Especie, Quimicos) -> atomics_to_string(Quimicos, ', ', QuimicosTexto) ; QuimicosTexto = 'No disponible'),
        (   modoConsumo(Especie, Consumo) -> true ; Consumo = 'No disponible'),

        format('-------------------------------------~n'),
        format('Planta: ~w~n', [Especie]),
        format('Propiedades: ~w~n', [PropiedadesTexto]),
        format('Usos medicinales: ~w~n', [UsosTexto]),
        format('Metodo de preparacion: ~w~n', [Metodo]),
        format('Instrucciones: ~w~n', [Instrucciones]),
        format('Modo de consumo: ~w~n', [Consumo]),
        format('Componentes quimicos: ~w~n', [QuimicosTexto]),
        format('-------------------------------------~n')
    ;   write('Planta no registrada'), nl
    ).

trataAfeccion(Planta, Afeccion):-
    plantaUsosMedicinales(Planta, ListaAfecciones),
    member(Afeccion, ListaAfecciones).

plantasConPropiedad(Propiedad, ListaPlantas):-
    findall(Planta, (caracteristicasPlanta(Planta, Propiedades), member(Propiedad, Propiedades)), ListaPlantas).

indicePlantas(Lista):-
    findall(Planta, plantaUsosMedicinales(Planta, _), Lista).
