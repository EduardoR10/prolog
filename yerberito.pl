:- use_module(library(pce)).
:- consult('plantas.pl').

inicio :-
    new(Ventana, dialog('Yerberito')),
    send(Ventana, size, size(1150, 570)),

    send(Ventana, append, new(_Titulo, label(title,
        'Bienvenido al Yerberito. Consulta informacion de plantas medicinales.'))),
    send(_Titulo, font, font(helvetica, bold, 16)),

    send(Ventana, append, new(_Instrucciones, label(instr,
        'Seleccione una pregunta y, si es necesario, escriba el nombre de la planta o enfermedad.'))),
    send(_Instrucciones, font, font(helvetica, normal, 14)),

    send(Ventana, append, new(_LabelPregunta, label('Seleccione una consulta:'))),
    send(_LabelPregunta, font, font(helvetica, bold, 14)),

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
    send(TextParametro, font, font(helvetica, normal, 12)),

    send(Ventana, append, new(_BotonConsultar, button('Consultar',
        message(@prolog, consulta_con_parametro, MenuPreguntas, TextParametro, Ventana)))),
    send(_BotonConsultar, font, font(helvetica, bold, 12)),

    send(Ventana, append, new(_BotonSalir, button('Salir', message(Ventana, destroy)))),
    send(_BotonSalir, font, font(helvetica, bold, 12)),

    send(Ventana, open).

consulta_con_parametro(MenuPreguntas, TextParametro, Ventana) :-
    get(MenuPreguntas, selection, Pregunta),
    get(TextParametro, value, Parametro),
    ( var(Parametro) -> ParamStr = '' ; ParamStr = Parametro ),
    consulta_dinamica(Pregunta, ParamStr, Ventana).

consulta_dinamica('Que plantas son analgesicas?', _, Ventana) :-
    findall(P, (accion_efecto_planta(P, analgesica); accion_efecto_planta(P, analgesica_suave)), Lista0),
    list_to_set(Lista0, Lista),
    (Lista == [] -> mostrar_resultado(['No se encontraron plantas analgesicas.'], 'Plantas analgesicas', Ventana)
    ; mostrar_resultado(Lista, 'Plantas analgesicas', Ventana)
    ).

consulta_dinamica('Que enfermedades cura una planta?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Enfermedades tratadas', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, trata_enfermedad(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron enfermedades para esa planta.'], 'Enfermedades tratadas', Ventana)
      ; mostrar_resultado(Lista, 'Enfermedades tratadas', Ventana)
      )
    ).

consulta_dinamica('Que plantas curan una enfermedad?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], 'Plantas para enfermedad', Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall(P, trata_enfermedad(P, Enfermedad), Lista0),
      list_to_set(Lista0, Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron plantas para esa enfermedad.'], 'Plantas para enfermedad', Ventana)
      ; mostrar_resultado(Lista, 'Plantas para enfermedad', Ventana)
      )
    ).

consulta_dinamica('Que elementos contiene una planta?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Elementos de la planta', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, elemento_planta(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron elementos para esa planta.'], 'Elementos de la planta', Ventana)
      ; mostrar_resultado(Lista, 'Elementos de la planta', Ventana)
      )
    ).

consulta_dinamica('Cual es el tratamiento para una enfermedad?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], 'Tratamientos para enfermedad', Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall((P,T), (trata_enfermedad(P, Enfermedad), modo_tratamiento(P, T)), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontro tratamiento para esa enfermedad.'], 'Tratamientos para enfermedad', Ventana)
      ; mostrar_resultado_tratamientos(Lista, 'Tratamientos para enfermedad', Ventana)
      )
    ).

consulta_dinamica('Botiquin de plantas', _, Ventana) :-
    findall(P, accion_efecto_planta(P, _), Plantas),
    list_to_set(Plantas, Unicas),
    mostrar_resultado(Unicas, 'Botiquin de plantas', Ventana).

consulta_dinamica('Cuales son plantas medicinales?', _, Ventana) :-
    findall(P, accion_efecto_planta(P, _), Plantas),
    list_to_set(Plantas, Unicas),
    mostrar_resultado(Unicas, 'Plantas medicinales', Ventana).

consulta_dinamica('Que elementos se encuentran en las plantas?', _, Ventana) :-
    findall(E, elemento_planta(_, E), Elementos),
    list_to_set(Elementos, Unicos),
    mostrar_resultado(Unicos, 'Elementos en plantas', Ventana).

consulta_dinamica('Que elementos tiene una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Elementos de la planta', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, elemento_planta(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron elementos para esa planta.'], 'Elementos de la planta', Ventana)
      ; mostrar_resultado(Lista, 'Elementos de la planta', Ventana)
      )
    ).

consulta_dinamica('Que plantas producen medicamentos?', _, Ventana) :-
    findall(P, medicamento_planta(_, P), Plantas),
    list_to_set(Plantas, Unicas),
    mostrar_resultado(Unicas, 'Plantas que producen medicamentos', Ventana).

consulta_dinamica('Que medicamentos produce una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Medicamentos de la planta', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(M, medicamento_planta(M, Planta), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron medicamentos para esa planta.'], 'Medicamentos de la planta', Ventana)
      ; mostrar_resultado(Lista, 'Medicamentos de la planta', Ventana)
      )
    ).

consulta_dinamica('Que medicamentos provienen de plantas?', _, Ventana) :-
    findall(M, medicamento_planta(M, _), Medicamentos),
    list_to_set(Medicamentos, Unicos),
    mostrar_resultado(Unicos, 'Medicamentos de plantas', Ventana).

consulta_dinamica('Cuales son las acciones o efectos de medicamentos provenientes de plantas?', _, Ventana) :-
    findall(A, accion_medicamento(_, A), Acciones),
    list_to_set(Acciones, Unicas),
    mostrar_resultado(Unicas, 'Efectos de medicamentos', Ventana).

consulta_dinamica('Cuales son los efectos o acciones de un medicamento en especifico?', MedStr, Ventana) :-
    ( MedStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre del medicamento.'], 'Efectos del medicamento', Ventana)
    ; atom_string(Med, MedStr),
      findall(A, accion_medicamento(Med, A), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron acciones para ese medicamento.'], 'Efectos del medicamento', Ventana)
      ; mostrar_resultado(Lista, 'Efectos del medicamento', Ventana)
      )
    ).

consulta_dinamica('Cuales son las acciones o efectos que tienen las plantas?', _, Ventana) :-
    findall(A, accion_efecto_planta(_, A), Acciones),
    list_to_set(Acciones, Unicas),
    mostrar_resultado(Unicas, 'Efectos de plantas', Ventana).

consulta_dinamica('Significado de palabras que son acciones o efectos de plantas sobre organismo', PalabraStr, Ventana) :-
    ( PalabraStr == '' ->
        mostrar_resultado(['Por favor, ingrese la palabra a consultar.'], 'Significado de efecto', Ventana)
    ; atom_string(Palabra, PalabraStr),
      ( significado_efecto(Palabra, Significado) ->
            mostrar_resultado([Significado], 'Significado de efecto', Ventana)
        ;   mostrar_resultado(['No se encontro el significado para esa palabra.'], 'Significado de efecto', Ventana)
      )
    ).

consulta_dinamica('Listado de plantas y sus acciones o efectos sobre el organismo', _, Ventana) :-
    findall(Texto,
        (accion_efecto_planta(P, A), format(atom(Texto), '~w: ~w', [P, A])),
        Lista),
    list_to_set(Lista, Unicas),
    mostrar_resultado(Unicas, 'Plantas y sus efectos', Ventana).

consulta_dinamica('Acciones o efectos de una planta en especifico', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Efectos de la planta', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(A, accion_efecto_planta(Planta, A), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron acciones para esa planta.'], 'Efectos de la planta', Ventana)
      ; mostrar_resultado(Lista, 'Efectos de la planta', Ventana)
      )
    ).

consulta_dinamica('Listar plantas medicinales y su nombre cientifico', _, Ventana) :-
    findall(Texto,
        (accion_efecto_planta(P, _), nombre_cientifico(P, NC), format(atom(Texto), '~w: ~w', [P, NC])),
        Lista),
    list_to_set(Lista, Unicas),
    mostrar_resultado(Unicas, 'Plantas y nombres cientificos', Ventana).

consulta_dinamica('Cuales son las enfermedades que curan las plantas?', _, Ventana) :-
    findall(E, trata_enfermedad(_, E), Enfermedades),
    list_to_set(Enfermedades, Unicas),
    mostrar_resultado(Unicas, 'Enfermedades tratadas por plantas', Ventana).

consulta_dinamica('Cuales son las enfermedades que cura una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Enfermedades tratadas', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(E, trata_enfermedad(Planta, E), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron enfermedades para esa planta.'], 'Enfermedades tratadas', Ventana)
      ; mostrar_resultado(Lista, 'Enfermedades tratadas', Ventana)
      )
    ).

consulta_dinamica('Cuales son las plantas que curan una enfermedad en especifico?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], 'Plantas para enfermedad', Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall(P, trata_enfermedad(P, Enfermedad), Lista0),
      list_to_set(Lista0, Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron plantas para esa enfermedad.'], 'Plantas para enfermedad', Ventana)
      ; mostrar_resultado(Lista, 'Plantas para enfermedad', Ventana)
      )
    ).

consulta_dinamica('Cuales son las formas de preparacion para tratamiento de enfermedades con uso de plantas?', _, Ventana) :-
    findall(M, modo_preparacion(_, M), Modos),
    list_to_set(Modos, Unicos),
    mostrar_resultado(Unicos, 'Formas de preparacion', Ventana).

consulta_dinamica('Cuales son los modos de preparacion de una planta en especifico?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Modos de preparacion', Ventana)
    ; atom_string(Planta, PlantaStr),
      findall(M, modo_preparacion(Planta, M), Lista),
      ( Lista == [] -> mostrar_resultado(['No se encontraron modos de preparacion para esa planta.'], 'Modos de preparacion', Ventana)
      ; mostrar_resultado(Lista, 'Modos de preparacion', Ventana)
      )
    ).

consulta_dinamica('Cual es el tratamiento y su preparacion para alguna enfermedad?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], 'Tratamiento y preparacion', Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall((P, T, M),
        (trata_enfermedad(P, Enfermedad), modo_tratamiento(P, T), modo_preparacion(P, M)),
        Lista),
      ( Lista == [] ->
            mostrar_resultado(['No se encontro tratamiento ni preparacion para esa enfermedad.'], 'Tratamiento y preparacion', Ventana)
        ;   findall(Texto,
                (member((P, T, M), Lista),
                 format(atom(Texto), 'Planta: ~w - Tratamiento: ~w - Preparacion: ~w', [P, T, M])),
                Textos),
            mostrar_resultado(Textos, 'Tratamiento y preparacion', Ventana)
        )
    ).

consulta_dinamica('Cuales son los origenes de las plantas medicinales?', _, Ventana) :-
    findall(O, origen_planta(_, O), Origenes),
    list_to_set(Origenes, Unicos),
    mostrar_resultado(Unicos, 'Origenes de plantas', Ventana).

consulta_dinamica('Cual es el origen de una planta?', PlantaStr, Ventana) :-
    ( PlantaStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la planta.'], 'Origen de la planta', Ventana)
    ; atom_string(Planta, PlantaStr),
      ( origen_planta(Planta, Origen) ->
            mostrar_resultado([Origen], 'Origen de la planta', Ventana)
        ;   mostrar_resultado(['No se encontro el origen para esa planta.'], 'Origen de la planta', Ventana)
      )
    ).

consulta_dinamica('Cual es el tratamiento para una enfermedad (ya sea con plantas o medicamentos)?', EnfermedadStr, Ventana) :-
    ( EnfermedadStr == '' ->
        mostrar_resultado(['Por favor, ingrese el nombre de la enfermedad.'], 'Tratamientos para enfermedad', Ventana)
    ; atom_string(Enfermedad, EnfermedadStr),
      findall((P, T), (trata_enfermedad(P, Enfermedad), modo_tratamiento(P, T)), ListaPlantas),
      findall((M, A), (accion_medicamento(M, A), medicamento_planta(M, _), A == Enfermedad), ListaMed),
      ( ListaPlantas == [], ListaMed == [] ->
            mostrar_resultado(['No se encontro tratamiento para esa enfermedad.'], 'Tratamientos para enfermedad', Ventana)
        ;   findall(Texto,
                ( ( member((P, T), ListaPlantas),
                    format(atom(Texto), 'Planta: ~w - Tratamiento: ~w', [P, T]) )
                ; ( member((M, A), ListaMed),
                    format(atom(Texto), 'Medicamento: ~w - Accion: ~w', [M, A]) )
                ),
                Textos),
            mostrar_resultado(Textos, 'Tratamientos para enfermedad', Ventana)
        )
    ).

consulta_dinamica(_, _, Ventana) :-
    mostrar_resultado(['Consulta no implementada.'], 'Consulta no implementada', Ventana).

% --- Mostrar resultados en una ventana modal con scroll ---
mostrar_resultado(Lista, Titulo, _) :-
    new(Dialog, dialog(Titulo)),
    send(Dialog, kind, transient), % Modal dialog
    send(Dialog, size, size(600, 900)),

    % Crear un browser para mostrar los resultados
    new(Browser, browser('Resultados', size(580, 900))),
    send(Browser, font, font(helvetica, normal, 14)),
    send(Browser, tab_stops, vector(20)), % Para indentacion
    % send(Browser, scrollbar, vertical), % Habilitar barra de desplazamiento vertical (no existe en browser)
    % send(Browser, jump, 10), % Reducir sensibilidad del scroll para rueda del raton (no existe en browser)
    send(Browser, hor_stretch, 100),
    send(Browser, hor_shrink, 100),

    % Agregar cada elemento como una linea en el browser
    ( Lista == [] ->
        send(Browser, append, 'No se encontraron resultados.')
    ; maplist(atom_string, Lista, ListaStr),
      forall(member(L, ListaStr),
             send(Browser, append, string('• %s', L)))
    ),

    % Anadir el browser al dialogo
    send(Dialog, append, Browser),

    % Boton para cerrar el dialogo
    send(Dialog, append, new(_, button('Cerrar', message(Dialog, destroy)))),
    send(Dialog, default_button, 'Cerrar'),

    % Mostrar el dialogo
    send(Dialog, open_centered).

% --- Mostrar resultados con tratamientos con scroll ---
mostrar_resultado_tratamientos(Lista, Titulo, _) :-
    new(Dialog, dialog(Titulo)),
    send(Dialog, kind, transient), % Modal dialog
    send(Dialog, size, size(600, 900)),

    % Crear un browser para mostrar los resultados
    new(Browser, browser('Resultados', size(860, 900))),
    send(Browser, font, font(helvetica, normal, 14)),
    send(Browser, tab_stops, vector(20)),
    % send(Browser, scrollbar, vertical), % Habilitar barra de desplazamiento vertical (no existe en browser)
    % send(Browser, jump, 10), % Reducir sensibilidad del scroll para rueda del raton (no existe en browser)
    send(Browser, hor_stretch, 100),
    send(Browser, hor_shrink, 100),

    % Agregar cada tratamiento como una linea en el browser
    ( Lista == [] ->
        send(Browser, append, 'No hay tratamientos encontrados.')
    ; findall(Texto,
              ( member((P,T), Lista),
                format(atom(Texto), '• Planta: ~w - Tratamiento: ~w', [P,T]) ),
              Textos),
      forall(member(T, Textos),
             send(Browser, append, T))
    ),

    % Anadir el browser al dialogo
    send(Dialog, append, Browser),

    % Boton para cerrar el dialogo
    send(Dialog, append, new(_, button('Cerrar', message(Dialog, destroy)))),
    send(Dialog, default_button, 'Cerrar'),

    % Mostrar el dialogo
    send(Dialog, open_centered).
