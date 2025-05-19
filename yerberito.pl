:- use_module(library(pce)).
:- dynamic
    accion_efecto_planta/2,
    trata_enfermedad/2,
    elemento_planta/2,
    modo_preparacion/2,
    modo_tratamiento/2,
    precaucion_planta/2,
    enfermedad_tratada/2.

% Berro
accion_efecto_planta(berro, expectorante).
accion_efecto_planta(berro, depurativa).
accion_efecto_planta(berro, diuretica).
trata_enfermedad(berro, bronquitis).
trata_enfermedad(berro, resfriado).
elemento_planta(berro, vitamina_c).
elemento_planta(berro, glucosinolatos).
modo_preparacion(berro, infusion).
modo_preparacion(berro, ensalada).
modo_tratamiento(berro, tomar_te_dos_veces_dia).
modo_tratamiento(berro, consumir_fresco_en_ensaladas).
precaucion_planta(berro, evitar_dosis_altas_puede_ser_irritante).

% Boldo (complemento)
accion_efecto_planta(boldo, hepatoprotectora).
accion_efecto_planta(boldo, colagoga).
accion_efecto_planta(boldo, diuretica_suave).
trata_enfermedad(boldo, digestion_lenta).
elemento_planta(boldo, boldina).
modo_preparacion(boldo, infusion).
modo_tratamiento(boldo, tomar_te_despues_comidas).
precaucion_planta(boldo, evitar_uso_prolongado_embarazo).

% Borraja
accion_efecto_planta(borraja, sudorifica).
accion_efecto_planta(borraja, emoliente).
accion_efecto_planta(borraja, diuretica_suave).
trata_enfermedad(borraja, fiebre).
trata_enfermedad(borraja, tos_seca).
elemento_planta(borraja, mucilagos).
elemento_planta(borraja, alcaloides_pirrolizidinicos).
modo_preparacion(borraja, infusion_hojas_flores).
modo_preparacion(borraja, aceite_semillas).
modo_tratamiento(borraja, tomar_te_dos_tres_veces_dia).
modo_tratamiento(borraja, aceite_de_semillas_uso_externo).
precaucion_planta(borraja, evitar_uso_prolongado_alcaloides_toxicos).

% Bugambilia
accion_efecto_planta(bugambilia, expectorante).
accion_efecto_planta(bugambilia, antitusiva).
accion_efecto_planta(bugambilia, febrifuga_suave).
trata_enfermedad(bugambilia, tos).
trata_enfermedad(bugambilia, gripe).
elemento_planta(bugambilia, saponinas).
elemento_planta(bugambilia, taninos).
modo_preparacion(bugambilia, cocimiento_flores).
modo_tratamiento(bugambilia, tomar_te_tres_veces_dia).
precaucion_planta(bugambilia, evitar_en_embarazo_dosis_altas).

% Ruda
accion_efecto_planta(ruda, calmante).
accion_efecto_planta(ruda, emenagoga).
accion_efecto_planta(ruda, antiespasmodica).
trata_enfermedad(ruda, menstruacion).
trata_enfermedad(ruda, colicos_menstruales).
trata_enfermedad(ruda, nerviosismo).
elemento_planta(ruda, rutina).
elemento_planta(ruda, alcaloides).
modo_preparacion(ruda, infusion_hojas).
modo_tratamiento(ruda, con_moderacion).
precaucion_planta(ruda, evitar_embarazo).

% Ruibarbo
accion_efecto_planta(ruibarbo, laxante).
accion_efecto_planta(ruibarbo, digestiva).
accion_efecto_planta(ruibarbo, tonica_amarga).
trata_enfermedad(ruibarbo, estrenimiento_ocasional).
trata_enfermedad(ruibarbo, digestion_lenta).
trata_enfermedad(ruibarbo, problemas_hepaticos_leves).
elemento_planta(ruibarbo, antraquinonas).
elemento_planta(ruibarbo, taninos).
modo_preparacion(ruibarbo, cocimiento_raiz).
modo_tratamiento(ruibarbo, pequenas_dosis).
precaucion_planta(ruibarbo, no_usar_en_embarazo).

% Salvia
accion_efecto_planta(salvia, astringente).
accion_efecto_planta(salvia, digestiva).
accion_efecto_planta(salvia, antiseptica).
trata_enfermedad(salvia, dolor_garganta).
trata_enfermedad(salvia, digestion_lenta).
trata_enfermedad(salvia, sudoracion_excesiva).
elemento_planta(salvia, aceites_esenciales).
elemento_planta(salvia, taninos).
modo_preparacion(salvia, infusion_hojas).
modo_preparacion(salvia, gargaras).
modo_tratamiento(salvia, hacer_gargaras_o_beber).
precaucion_planta(salvia, no_usar_en_grandes_cantidades).

% Sen
accion_efecto_planta(sen, laxante).
trata_enfermedad(sen, estrenimiento_ocasional).
elemento_planta(sen, senosidos).
modo_preparacion(sen, infusion_hojas_foliculos).
modo_tratamiento(sen, una_taza_diaria_por_una_semana).
precaucion_planta(sen, no_uso_prolongado).

% Sanguinaria
accion_efecto_planta(sanguinaria, expectorante_suave).
accion_efecto_planta(sanguinaria, estimulante_circulacion_local).
accion_efecto_planta(sanguinaria, antiseptica_suave).
trata_enfermedad(sanguinaria, problemas_respiratorios_leves).
trata_enfermedad(sanguinaria, dolor_garganta_leve).
trata_enfermedad(sanguinaria, tos_leve).
elemento_planta(sanguinaria, sanguinarina).
modo_preparacion(sanguinaria, cocimiento_raiz).
modo_tratamiento(sanguinaria, beber_te_o_hacer_gargaras_en_bajas_dosis).
modo_tratamiento(sanguinaria, uso_externo_para_afecciones_piel).
precaucion_planta(sanguinaria, toxicidad_alta_si_se_abusa).

% Regaliz
accion_efecto_planta(regaliz, expectorante_suave).
accion_efecto_planta(regaliz, antiinflamatorio_suave).
accion_efecto_planta(regaliz, digestivo_suave).
trata_enfermedad(regaliz, tos_seca).
trata_enfermedad(regaliz, dolor_garganta_leve).
trata_enfermedad(regaliz, problemas_digestivos_leves).
elemento_planta(regaliz, glicirricina).
elemento_planta(regaliz, flavonoides).
modo_preparacion(regaliz, infusion_raiz).
modo_preparacion(regaliz, extracto).
modo_tratamiento(regaliz, pequenas_cantidades).
precaucion_planta(regaliz, hipertension).

% Retama
accion_efecto_planta(retama, diuretico).
accion_efecto_planta(retama, hipotensor_suave).
accion_efecto_planta(retama, antireumatico_suave).
trata_enfermedad(retama, hipertension_leve).
trata_enfermedad(retama, reumatismo_leve).
elemento_planta(retama, esparteina).
modo_preparacion(retama, cocimiento_flores_con_precaucion).
modo_tratamiento(retama, con_precaucion).
precaucion_planta(retama, toxicidad).

% Ricino
accion_efecto_planta(ricino, laxante).
accion_efecto_planta(ricino, emoliente).
accion_efecto_planta(ricino, antiinflamatorio_topico).
trata_enfermedad(ricino, estrenimiento_ocasional).
trata_enfermedad(ricino, inflamacion_externa).
elemento_planta(ricino, aceite_de_ricino).
elemento_planta(ricino, trigliceridos_acido_ricinoleico).
modo_preparacion(ricino, aceite_semillas).
modo_tratamiento(ricino, dosis_bajas_aceite_uso_interno).
modo_tratamiento(ricino, aplicar_aceite_externamente).
precaucion_planta(ricino, evitar_embarazo_semillas_toxicas).

% Romero
accion_efecto_planta(romero, antiinflamatorio).
accion_efecto_planta(romero, digestivo).
accion_efecto_planta(romero, estimulante_circulacion).
accion_efecto_planta(romero, analgesica_suave).
trata_enfermedad(romero, dolor_muscular).
trata_enfermedad(romero, digestion_lenta).
trata_enfermedad(romero, fatiga_mental).
elemento_planta(romero, aceites_esenciales).
elemento_planta(romero, taninos).
modo_preparacion(romero, infusion_hojas).
modo_preparacion(romero, aceite_esencial).
modo_tratamiento(romero, tomar_te_tres_veces_dia).
modo_tratamiento(romero, aceite_para_masajes).
precaucion_planta(romero, evitar_en_embarazo_dosis_altas).

% Rosal
accion_efecto_planta(rosal, antiinflamatorio_suave).
accion_efecto_planta(rosal, vitaminico_suave).
accion_efecto_planta(rosal, cicatrizante).
trata_enfermedad(rosal, resfriado_leve).
trata_enfermedad(rosal, trastornos_digestivos_leves).
trata_enfermedad(rosal, cicatrizacion_piel).
elemento_planta(rosal, vitamina_c).
elemento_planta(rosal, acidos_grasos_esenciales).
modo_preparacion(rosal, infusion_petalos).
modo_preparacion(rosal, aceite_de_rosa_mosqueta).
modo_tratamiento(rosal, tomar_te_varias_veces_dia).
modo_tratamiento(rosal, aplicar_aceite_en_piel).
precaucion_planta(rosal, ninguna_relevante).


inicio :-

    new(Ventana, dialog('Yerberito')),
    send(Ventana, size, size(1150, 570)),

    send(Ventana, append, new(_Titulo, label(title,
        'Bienvenido al Yerberito. Consulta informacion de plantas medicinales.'))),

    send(Ventana, append, new(_Instrucciones, label(instr,
        'Seleccione una pregunta y, si es necesario, escriba el nombre de la planta o enfermedad.'))),
    

    send(Ventana, append, new(Resultado, text('Aqui apareceran los resultados de la consulta'))),
    send(Resultado, name, resultado), % <--- ASIGNA UN NOMBRE AL OBJETO
    send(Resultado, font, font(courier, normal, 12)),
    send(Resultado, size, size(400, 250)),

    send(Ventana, append, new(_LabelPregunta, label('Seleccione una consulta:'))),

    send(Ventana, append, new(MenuPreguntas, menu(consultas, cycle))),

    send(MenuPreguntas, append, 'Que plantas son analgesicas?'),
    send(MenuPreguntas, append, 'Que enfermedades cura una planta?'),
    send(MenuPreguntas, append, 'Que plantas curan una enfermedad?'),
    send(MenuPreguntas, append, 'Que elementos contiene una planta?'),
    send(MenuPreguntas, append, 'Cual es el tratamiento para una enfermedad?'),
    send(MenuPreguntas, append, 'Cuales son plantas medicinales?'),
    send(MenuPreguntas, append, 'Que elementos se encuentran en las plantas?'),
    send(MenuPreguntas, append, 'Que elementos tiene una planta en especifico?'),
    send(MenuPreguntas, append, 'Que plantas producen medicamentos?'),
    send(MenuPreguntas, append, 'Que medicamentos produce una planta en especifico?'),
    send(MenuPreguntas, append, 'Que medicamentos provienen de plantas?'),
    send(MenuPreguntas, append, 'Cuales son las acciones o efectos de medicamentos provenientes de plantas?'),
    send(MenuPreguntas, append, 'Cuales son los efectos o acciones de un medicamento en especifico?'),
    send(MenuPreguntas, append, 'Cuales son las acciones o efectos que tienen las plantas?'),
    send(MenuPreguntas, append, 'Significado de palabras que son acciones o efectos de plantas sobre organismo'),
    send(MenuPreguntas, append, 'Listado de plantas y sus acciones o efectos sobre el organismo'),
    send(MenuPreguntas, append, 'Acciones o efectos de una planta en especifico'),
    send(MenuPreguntas, append, 'Listar plantas medicinales y su nombre cientifico'),
    send(MenuPreguntas, append, 'Cuales son las enfermedades que curan las plantas?'),
    send(MenuPreguntas, append, 'Cuales son las enfermedades que cura una planta en especifico?'),
    send(MenuPreguntas, append, 'Cuales son las plantas que curan una enfermedad en especifico?'),
    send(MenuPreguntas, append, 'Cuales son las formas de preparacion para tratamiento de enfermedades con uso de plantas?'),
    send(MenuPreguntas, append, 'Cuales son los modos de preparacion de una planta en especifico?'),
    send(MenuPreguntas, append, 'Cual es el tratamiento y su preparacion para alguna enfermedad?'),
    send(MenuPreguntas, append, 'Cuales son los origenes de las plantas medicinales?'),
    send(MenuPreguntas, append, 'Cual es el origen de una planta?'),
    send(MenuPreguntas, append, 'Cual es el tratamiento para una enfermedad (ya sea con plantas o medicamentos)?'),
    send(MenuPreguntas, append, 'Botiquin de plantas'),

    send(Ventana, append, new(TextParametro, text_item('Ingrese aqui el nombre de la planta o enfermedad (si aplica)'))),

    send(Ventana, append, new(_BotonConsultar, button('Consultar',
        message(@prolog, consulta_con_parametro, MenuPreguntas, TextParametro, Ventana)))),

    send(Ventana, append, new(_BotonSalir, button('Salir', message(Ventana, destroy)))),

    send(Ventana, open).

% --- Mostrar resultados

consulta_con_parametro(MenuPreguntas, TextParametro, Ventana) :-
    get(MenuPreguntas, selection, Pregunta),
    get(TextParametro, value, Parametro),
    ( var(Parametro) -> ParamStr = '' ; ParamStr = Parametro ),
    consulta_dinamica(Pregunta, ParamStr, Ventana).

consulta_dinamica('Que plantas son analgesicas?', _, Ventana) :-
    findall(P, (accion_efecto_planta(P, analgesica); accion_efecto_planta(P, analgesica_suave)), Lista0),
    list_to_set(Lista0, Lista),
    (Lista == [] -> mostrar_resultado(['No se encontraron plantas analgesicas.'], Ventana)
    ; mostrar_resultado(Lista, Ventana)
    ).

consulta_dinamica('Que enfermedades cura una planta?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, trata_enfermedad(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron enfermedades para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Que plantas curan una enfermedad?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall(P, trata_enfermedad(P, Enfermedad), Lista0),
      list_to_set(Lista0, Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron plantas para esa enfermedad.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Que elementos contiene una planta?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, elemento_planta(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron elementos para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Cual es el tratamiento para una enfermedad?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall((P,T), (trata_enfermedad(P, Enfermedad), modo_tratamiento(P, T)), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontro tratamiento para esa enfermedad.'], Ventana)
      ; mostrar_resultado_tratamientos(Lista, Ventana)
      )
    ).

consulta_dinamica('Botiquin de plantas', _, Ventana) :-
    findall(P, accion_efecto_planta(P, _), Plantas),
    list_to_set(Plantas, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Cuales son plantas medicinales?', _, Ventana) :-
    findall(P, accion_efecto_planta(P, _), Plantas),
    list_to_set(Plantas, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Que elementos se encuentran en las plantas?', _, Ventana) :-
    findall(E, elemento_planta(_, E), Elementos),
    list_to_set(Elementos, Unicos),
    mostrar_resultado(Unicos, Ventana).

consulta_dinamica('Que elementos tiene una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, elemento_planta(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron elementos para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

% Medicamentos y plantas asociadas (hechos de ejemplo)
medicamento_planta(aspirina, sauce).
medicamento_planta(morfina, adormidera).
medicamento_planta(digoxina, digital).
medicamento_planta(boldina, boldo).
medicamento_planta(glicirricina, regaliz).

% Anadir hechos para sauce, adormidera, digital
accion_efecto_planta(sauce, analgesica).
accion_efecto_planta(sauce, antiinflamatoria).
trata_enfermedad(sauce, dolor).
elemento_planta(sauce, salicina).
modo_preparacion(sauce, infusion_corteza).
modo_tratamiento(sauce, tomar_te_dos_veces_dia).
precaucion_planta(sauce, evitar_en_ninos_embarazo).

accion_efecto_planta(adormidera, analgesica).
accion_efecto_planta(adormidera, sedante).
trata_enfermedad(adormidera, dolor_intenso).
elemento_planta(adormidera, alcaloides).
modo_preparacion(adormidera, extracto).
modo_tratamiento(adormidera, uso_controlado_medico).
precaucion_planta(adormidera, muy_toxica).

accion_efecto_planta(digital, cardiotonica).
trata_enfermedad(digital, insuficiencia_cardiaca).
elemento_planta(digital, glucosidos).
modo_preparacion(digital, extracto).
modo_tratamiento(digital, uso_controlado_medico).
precaucion_planta(digital, muy_toxica).

consulta_dinamica('Que plantas producen medicamentos?', _, Ventana) :-
    findall(P, medicamento_planta(_, P), Plantas),
    list_to_set(Plantas, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Que medicamentos produce una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(M, medicamento_planta(M, Planta), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron medicamentos para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Que medicamentos provienen de plantas?', _, Ventana) :-
    findall(M, medicamento_planta(M, _), Medicamentos),
    list_to_set(Medicamentos, Unicos),
    mostrar_resultado(Unicos, Ventana).

% Acciones/efectos de medicamentos (hechos de ejemplo)
accion_medicamento(aspirina, analgesica).
accion_medicamento(aspirina, antiinflamatoria).
accion_medicamento(morfina, analgesica).
accion_medicamento(digoxina, cardiotonica).
accion_medicamento(boldina, hepatoprotectora).
accion_medicamento(glicirricina, expectorante).

consulta_dinamica('Cuales son las acciones o efectos de medicamentos provenientes de plantas?', _, Ventana) :-
    findall(A, accion_medicamento(_, A), Acciones),
    list_to_set(Acciones, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Cuales son los efectos o acciones de un medicamento en especifico?', MedStr, Ventana) :-
    ( MedStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre del medicamento.'], Ventana)
    ; atom_string(Med, MedStr),
      findall(A, accion_medicamento(Med, A), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron acciones para ese medicamento.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Cuales son las acciones o efectos que tienen las plantas?', _, Ventana) :-
    findall(A, accion_efecto_planta(_, A), Acciones),
    list_to_set(Acciones, Unicas),
    mostrar_resultado(Unicas, Ventana).

% Significados de acciones/efectos (hechos de ejemplo)
significado_efecto(analgesica, 'Reduce o elimina el dolor').
significado_efecto(antiinflamatoria, 'Disminuye la inflamacion').
significado_efecto(hepatoprotectora, 'Protege el higado').
significado_efecto(expectorante, 'Favorece la expulsion de mucosidad').
significado_efecto(laxante, 'Favorece la evacuacion intestinal').
significado_efecto(digestiva, 'Favorece la digestion').
significado_efecto(antiseptica, 'Previene infecciones').
significado_efecto(diuretica, 'Aumenta la produccion de orina').
significado_efecto(cardiotonica, 'Aumenta la fuerza de contraccion del corazon').
significado_efecto(sedante, 'Disminuye la actividad del sistema nervioso central').

consulta_dinamica('Significado de palabras que son acciones o efectos de plantas sobre organismo', PalabraStr, Ventana) :-
    ( PalabraStr == '' ->
        mostrar_resultado(['Por favor, ingrese la palabra a consultar.'], Ventana)
    ; atom_string(Palabra, PalabraStr),
      ( significado_efecto(Palabra, Significado) ->
            mostrar_resultado([Significado], Ventana)
        ;   mostrar_resultado(['No se encontro el significado para esa palabra.'], Ventana)
      )
    ).

consulta_dinamica('Listado de plantas y sus acciones o efectos sobre el organismo', _, Ventana) :-
    findall(Texto,
        (accion_efecto_planta(P, A), format(atom(Texto), '~w: ~w', [P, A])),
        Lista),
    list_to_set(Lista, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Acciones o efectos de una planta en especifico', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(A, accion_efecto_planta(Planta, A), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron acciones para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

% Nombre cientifico de plantas (hechos de ejemplo)
nombre_cientifico(berro, 'Nasturtium officinale').
nombre_cientifico(boldo, 'Peumus boldus').
nombre_cientifico(borraja, 'Borago officinalis').
nombre_cientifico(bugambilia, 'Bougainvillea glabra').
nombre_cientifico(ruda, 'Ruta graveolens').
nombre_cientifico(ruibarbo, 'Rheum rhabarbarum').
nombre_cientifico(salvia, 'Salvia officinalis').
nombre_cientifico(sen, 'Senna alexandrina').
nombre_cientifico(sanguinaria, 'Sanguinaria canadensis').
nombre_cientifico(regaliz, 'Glycyrrhiza glabra').
nombre_cientifico(retama, 'Spartium junceum').
nombre_cientifico(ricino, 'Ricinus communis').
nombre_cientifico(romero, 'Rosmarinus officinalis').
nombre_cientifico(rosal, 'Rosa spp.').
nombre_cientifico(sauce, 'Salix alba').
nombre_cientifico(adormidera, 'Papaver somniferum').
nombre_cientifico(digital, 'Digitalis purpurea').

consulta_dinamica('Listar plantas medicinales y su nombre cientifico', _, Ventana) :-
    findall(Texto,
        (accion_efecto_planta(P, _), nombre_cientifico(P, NC), format(atom(Texto), '~w: ~w', [P, NC])),
        Lista),
    list_to_set(Lista, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Cuales son las enfermedades que curan las plantas?', _, Ventana) :-
    findall(E, trata_enfermedad(_, E), Enfermedades),
    list_to_set(Enfermedades, Unicas),
    mostrar_resultado(Unicas, Ventana).

consulta_dinamica('Cuales son las enfermedades que cura una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, trata_enfermedad(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron enfermedades para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Cuales son las plantas que curan una enfermedad en especifico?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall(P, trata_enfermedad(P, Enfermedad), Lista0),
      list_to_set(Lista0, Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron plantas para esa enfermedad.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Cuales son las formas de preparacion para tratamiento de enfermedades con uso de plantas?', _, Ventana) :-
    findall(M, modo_preparacion(_, M), Modos),
    list_to_set(Modos, Unicos),
    mostrar_resultado(Unicos, Ventana).

consulta_dinamica('Cuales son los modos de preparacion de una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(M, modo_preparacion(Planta, M), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron modos de preparacion para esa planta.'], Ventana)
      ; mostrar_resultado(Lista, Ventana)
      )
    ).

consulta_dinamica('Cual es el tratamiento y su preparacion para alguna enfermedad?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall((P, T, M),
        (trata_enfermedad(P, Enfermedad), modo_tratamiento(P, T), modo_preparacion(P, M)),
        Lista),
      ( Lista == [] ->
            mostrar_resultado(['No se encontro tratamiento ni preparacion para esa enfermedad.'], Ventana)
        ;   findall(Texto,
                (member((P, T, M), Lista),
                 format(atom(Texto), 'Planta: ~w - Tratamiento: ~w - Preparacion: ~w', [P, T, M])),
                Textos),
            atomic_list_concat(Textos, '\n', TextoFinal),
            enviar_resultado(Ventana, TextoFinal)
        )
    ).

% Origen de plantas (hechos de ejemplo)
origen_planta(berro, europa).
origen_planta(boldo, sudamerica).
origen_planta(borraja, europa).
origen_planta(bugambilia, america).
origen_planta(ruda, europa).
origen_planta(ruibarbo, asia).
origen_planta(salvia, europa).
origen_planta(sen, africa).
origen_planta(sanguinaria, america).
origen_planta(regaliz, asia).
origen_planta(retama, mediterraneo).
origen_planta(ricino, africa).
origen_planta(romero, mediterraneo).
origen_planta(rosal, asia).
origen_planta(sauce, europa).
origen_planta(adormidera, asia).
origen_planta(digital, europa).

consulta_dinamica('Cuales son los origenes de las plantas medicinales?', _, Ventana) :-
    findall(O, origen_planta(_, O), Origenes),
    list_to_set(Origenes, Unicos),
    mostrar_resultado(Unicos, Ventana).

consulta_dinamica('Cual es el origen de una planta?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], Ventana)
    ; atom_string(Planta, PlantaStr),
      ( origen_planta(Planta, Origen) ->
            mostrar_resultado([Origen], Ventana)
        ;   mostrar_resultado(['No se encontro el origen para esa planta.'], Ventana)
      )
    ).

consulta_dinamica('Cual es el tratamiento para una enfermedad (ya sea con plantas o medicamentos)?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall((P, T), (trata_enfermedad(P, Enfermedad), modo_tratamiento(P, T)), ListaPlantas),
      findall((M, A), (accion_medicamento(M, A), medicamento_planta(M, _), A == Enfermedad), ListaMed),
      ( ListaPlantas == [], ListaMed == [] ->
            mostrar_resultado(['No se encontro tratamiento para esa enfermedad.'], Ventana)
        ;   (ListaPlantas \== [] ->
                mostrar_resultado_tratamientos(ListaPlantas, Ventana)
            ; true),
            (ListaMed \== [] ->
                findall(Texto,
                    (member((M, A), ListaMed),
                     format(atom(Texto), 'Medicamento: ~w - Accion: ~w', [M, A])),
                    Textos),
                atomic_list_concat(Textos, '\n', TextoFinal),
                enviar_resultado(Ventana, TextoFinal)
            ; true)
        )
    ).

consulta_dinamica(_, _, Ventana) :-
    mostrar_resultado(['Consulta no implementada.'], Ventana).

mostrar_resultado(Lista, Ventana) :-
    maplist(atom_string, Lista, ListaStr),
    atomic_list_concat(ListaStr, ' - ', Texto),
    enviar_resultado(Ventana, Texto).

% --- Mostrar resultados con tratamientos ---
mostrar_resultado_tratamientos([], Ventana) :-
    enviar_resultado(Ventana, 'No hay tratamientos encontrados.').

mostrar_resultado_tratamientos(Lista, Ventana) :-
    findall(Texto,
        ( member((P,T), Lista),
          format(atom(Texto), 'Planta: ~w - Tratamiento: ~w', [P,T])
        ),
        Textos),
    atomic_list_concat(Textos, '\n', TextoFinal),
    enviar_resultado(Ventana, TextoFinal).

enviar_resultado(Ventana, Texto) :-
    get(Ventana, member, resultado, Resultado),
    send(Resultado, clear),
    (   string(Texto)
    ->  split_string(Texto, " - ", "", Lineas),
        forall(member(L, Lineas), send(Resultado, append, string('%s\n', L)))
    ;   term_to_atom(Texto, AtomTexto),
        send(Resultado, append, AtomTexto)
    ).
