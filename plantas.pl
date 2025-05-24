:- dynamic
    accion_efecto_planta/2,
    trata_enfermedad/2,
    elemento_planta/2,
    modo_preparacion/2,
    modo_tratamiento/2,
    precaucion_planta/2,
    medicamento_planta/2,
    accion_medicamento/2,
    significado_efecto/2,
    nombre_cientifico/2,
    origen_planta/2,
    imagen_planta/2.

% Imágenes de las plantas
imagen_planta(abrojo, 'imgs/abrojo.jpg').
imagen_planta(acacia, 'imgs/acacia.jpg').
imagen_planta(acanto, 'imgs/acanto.jpg').
imagen_planta(aguacate, 'imgs/aguacate.jpg').
imagen_planta(ahuehuete, 'imgs/ahuehuete.jpg').
imagen_planta(ajenjo, 'imgs/ajenjo.jpg').
imagen_planta(albahaca, 'imgs/albahaca.jpg').
imagen_planta(alcachofa, 'imgs/alcachofa.jpg').
imagen_planta(alcanfor, 'imgs/alcanfor.jpg').
imagen_planta(amapola_amarilla, 'imgs/amapola_amarilla.jpg').
imagen_planta(amate, 'imgs/amate.jpg').
imagen_planta(anis, 'imgs/anis.jpg').
imagen_planta(anacahuite, 'imgs/anacahuite.jpg').
imagen_planta(arnica, 'imgs/arnica.jpg').
imagen_planta(barbasco, 'imgs/barbasco.jpg').
imagen_planta(belladona, 'imgs/belladona.jpg').
imagen_planta(berro, 'imgs/berro.jpg').
imagen_planta(boldo, 'imgs/boldo.jpg').
imagen_planta(borraja, 'imgs/borraja.jpg').
imagen_planta(brionia, 'imgs/brionia.jpg').
imagen_planta(bugambilia, 'imgs/bugambilia.jpg').
imagen_planta(canela, 'imgs/canela.jpg').
imagen_planta(cardo, 'imgs/cardo.jpg').
imagen_planta(cedron, 'imgs/cedron.jpg').
imagen_planta(cempasuchil, 'imgs/cempasuchil.jpg').
imagen_planta(chaparro_amargoso, 'imgs/chaparro_amargoso.jpg').
imagen_planta(chicalote, 'imgs/chicalote.jpg').
imagen_planta(chichigua, 'imgs/chichigua.jpg').
imagen_planta(cilantro, 'imgs/cilantro.jpg').
imagen_planta(cocolmeca, 'imgs/cocolmeca.jpg').
imagen_planta(cola_de_caballo, 'imgs/cola_de_caballo.jpg').
imagen_planta(colchino, 'imgs/colchino.jpg').
imagen_planta(comino, 'imgs/comino.jpg').
imagen_planta(colpachi, 'imgs/colpachi.jpg').
imagen_planta(cuachalalate, 'imgs/cuachalalate.jpg').
imagen_planta(cuajiote, 'imgs/cuajiote.jpg').
imagen_planta(cuasia, 'imgs/cuasia.jpg').
imagen_planta(damiana, 'imgs/damiana.jpg').
imagen_planta(diente_de_leon, 'imgs/diente_de_leon.jpg').
imagen_planta(digitaria, 'imgs/digitaria.jpg').
imagen_planta(doradilla, 'imgs/doradilla.jpg').
imagen_planta(epazote, 'imgs/epazote.jpg').
imagen_planta(enebro, 'imgs/enebro.jpg').
imagen_planta(estafiate, 'imgs/estafiate.jpg').
imagen_planta(eucalipto, 'imgs/eucalipto.jpg').
imagen_planta(fenogreco, 'imgs/fenogreco.jpg').
imagen_planta(genciana, 'imgs/genciana.jpg').
imagen_planta(geranio, 'imgs/geranio.jpg').
imagen_planta(girasol, 'imgs/girasol.jpg').
imagen_planta(gingseng, 'imgs/gingseng.jpg').
imagen_planta(gordolobo, 'imgs/gordolobo.jpg').
imagen_planta(grama, 'imgs/grama.jpg').
imagen_planta(granado, 'imgs/granado.jpg').
imagen_planta(guaco, 'imgs/guaco.jpg').
imagen_planta(guazuma, 'imgs/guazuma.jpg').
imagen_planta(guayacan, 'imgs/guayacan.jpg').
imagen_planta(hamamelis, 'imgs/hamamelis.jpg').
imagen_planta(helenio, 'imgs/helenio.jpg').
imagen_planta(hierba_del_pollo, 'imgs/hierba_del_pollo.jpg').
imagen_planta(hinojo, 'imgs/hinojo.jpg').
imagen_planta(ipecacuana, 'imgs/ipecacuana.jpg').
imagen_planta(jalapa, 'imgs/jalapa.jpg').
imagen_planta(jazmin_amarillo, 'imgs/jazmin_amarillo.jpg').
imagen_planta(jengibre, 'imgs/jengibre.jpg').
imagen_planta(llanten, 'imgs/llanten.jpg').
imagen_planta(madreselva, 'imgs/madreselva.jpg').
imagen_planta(maguey, 'imgs/maguey.jpg').
imagen_planta(malva, 'imgs/malva.jpg').
imagen_planta(malvavisco, 'imgs/malvavisco.jpg').
imagen_planta(mangle, 'imgs/mangle.jpg').
imagen_planta(manzanilla, 'imgs/manzanilla.jpg').
imagen_planta(marihuana, 'imgs/marihuana.jpg').
imagen_planta(marrubio, 'imgs/marrubio.jpg').
imagen_planta(mastuerzo, 'imgs/mastuerzo.jpg').
imagen_planta(matarique, 'imgs/matarique.jpg').
imagen_planta(menta, 'imgs/menta.jpg').
imagen_planta(nopal, 'imgs/nopal.jpg').
imagen_planta(oregano, 'imgs/oregano.jpg').
imagen_planta(palo_de_flor, 'imgs/palo_de_flor.jpg').
imagen_planta(pasiflora, 'imgs/pasiflora.jpg').
imagen_planta(pericon, 'imgs/pericon.jpg').
imagen_planta(pinguica, 'imgs/pinguica.jpg').
imagen_planta(pirul, 'imgs/pirul.jpg').
imagen_planta(prodigiosa, 'imgs/prodigiosa.jpg').
imagen_planta(pulsatilla, 'imgs/pulsatilla.jpg').
imagen_planta(quebracho, 'imgs/quebracho.jpg').
imagen_planta(quina, 'imgs/quina.jpg').
imagen_planta(retama, 'imgs/retama.jpg').
imagen_planta(ricino, 'imgs/ricino.jpg').
imagen_planta(romero, 'imgs/romero.jpg').
imagen_planta(rosal, 'imgs/rosal.jpg').
imagen_planta(ruda, 'imgs/ruda.jpg').
imagen_planta(ruibarbo, 'imgs/ruibarbo.jpg').
imagen_planta(salvia, 'imgs/salvia.jpg').
imagen_planta(sanguinaria, 'imgs/sanguinaria.jpg').
imagen_planta(sauce, 'imgs/sauce.jpg').
imagen_planta(sen, 'imgs/sen.jpg').
imagen_planta(te_de_milpa, 'imgs/te_de_milpa.jpg').
imagen_planta(tila, 'imgs/tila.jpg').
imagen_planta(toloache, 'imgs/toloache.jpeg').
imagen_planta(tronadora, 'imgs/tronadora.jpg').
imagen_planta(tripa_de_judas, 'imgs/tripa_de_judas.jpg').
imagen_planta(uva, 'imgs/uva.jpg').
imagen_planta(adormidera, 'imgs/adormidera.jpg').
imagen_planta(digital, 'imgs/digital.jpg').


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
nombre_cientifico(berro, 'Nasturtium officinale').
origen_planta(berro, europa).

% Boldo
accion_efecto_planta(boldo, hepatoprotectora).
accion_efecto_planta(boldo, colagoga).
accion_efecto_planta(boldo, diuretica_suave).
trata_enfermedad(boldo, digestion_lenta).
elemento_planta(boldo, boldina).
modo_preparacion(boldo, infusion).
modo_tratamiento(boldo, tomar_te_despues_comidas).
precaucion_planta(boldo, evitar_uso_prolongado_embarazo).
nombre_cientifico(boldo, 'Peumus boldus').
origen_planta(boldo, sudamerica).

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
nombre_cientifico(borraja, 'Borago officinalis').
origen_planta(borraja, europa).

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
nombre_cientifico(bugambilia, 'Bougainvillea glabra').
origen_planta(bugambilia, america).

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
nombre_cientifico(ruda, 'Ruta graveolens').
origen_planta(ruda, europa).

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
nombre_cientifico(ruibarbo, 'Rheum rhabarbarum').
origen_planta(ruibarbo, asia).

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
nombre_cientifico(salvia, 'Salvia officinalis').
origen_planta(salvia, europa).

% Sen
accion_efecto_planta(sen, laxante).
trata_enfermedad(sen, estrenimiento_ocasional).
elemento_planta(sen, senosidos).
modo_preparacion(sen, infusion_hojas_foliculos).
modo_tratamiento(sen, una_taza_diaria_por_una_semana).
precaucion_planta(sen, no_uso_prolongado).
nombre_cientifico(sen, 'Senna alexandrina').
origen_planta(sen, africa).

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
nombre_cientifico(sanguinaria, 'Sanguinaria canadensis').
origen_planta(sanguinaria, america).

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
nombre_cientifico(regaliz, 'Glycyrrhiza glabra').
origen_planta(regaliz, asia).

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
nombre_cientifico(retama, 'Spartium junceum').
origen_planta(retama, mediterraneo).

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
nombre_cientifico(ricino, 'Ricinus communis').
origen_planta(ricino, africa).

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
nombre_cientifico(romero, 'Rosmarinus officinalis').
origen_planta(romero, mediterraneo).

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
nombre_cientifico(rosal, 'Rosa spp.').
origen_planta(rosal, asia).

% Sauce
accion_efecto_planta(sauce, analgesica).
accion_efecto_planta(sauce, antiinflamatoria).
trata_enfermedad(sauce, dolor).
elemento_planta(sauce, salicina).
modo_preparacion(sauce, infusion_corteza).
modo_tratamiento(sauce, tomar_te_dos_veces_dia).
precaucion_planta(sauce, evitar_en_ninos_embarazo).
nombre_cientifico(sauce, 'Salix alba').
origen_planta(sauce, europa).

% Adormidera
accion_efecto_planta(adormidera, analgesica).
accion_efecto_planta(adormidera, sedante).
trata_enfermedad(adormidera, dolor_intenso).
elemento_planta(adormidera, alcaloides).
modo_preparacion(adormidera, extracto).
modo_tratamiento(adormidera, uso_controlado_medico).
precaucion_planta(adormidera, muy_toxica).
nombre_cientifico(adormidera, 'Papaver somniferum').
origen_planta(adormidera, asia).

% Digital
accion_efecto_planta(digital, cardiotonica).
trata_enfermedad(digital, insuficiencia_cardiaca).
elemento_planta(digital, glucosidos).
modo_preparacion(digital, extracto).
modo_tratamiento(digital, uso_controlado_medico).
precaucion_planta(digital, muy_toxica).
nombre_cientifico(digital, 'Digitalis purpurea').
origen_planta(digital, europa).

% Medicamentos y plantas asociadas
medicamento_planta(aspirina, sauce).
medicamento_planta(morfina, adormidera).
medicamento_planta(digoxina, digital).
medicamento_planta(boldina, boldo).
medicamento_planta(glicirricina, regaliz).

% Acciones/efectos de medicamentos
accion_medicamento(aspirina, analgesica).
accion_medicamento(aspirina, antiinflamatoria).
accion_medicamento(morfina, analgesica).
accion_medicamento(digoxina, cardiotonica).
accion_medicamento(boldina, hepatoprotectora).
accion_medicamento(glicirricina, expectorante).

% Significados de acciones/efectos
significado_efecto(analgesica, 'Reduce o elimina el dolor').
significado_efecto(antiinflamatoria, 'Disminuye la inflamacion').
significado_efecto(hepatoprotectora, 'Protege el higado').
significado_efecto(expectorante, 'Favorece la expulsion de mucosidad').
significado_efecto(laxante, 'Favorece la evacuacion intestinal').
significado_efecto(digestiva, 'Favorece la digestion').
significado_efecto(Forms, 'Previene infecciones').
significado_efecto(diuretica, 'Aumenta la produccion de orina').
significado_efecto(cardiotonica, 'Aumenta la fuerza de contraccion del corazon').
significado_efecto(sedante, 'Disminuye la actividad del sistema nervioso central').
significado_efecto(antibacteriana, 'Destruye o inhibe el crecimiento de bacterias').
significado_efecto(antiviral, 'Destruye o inhibe el crecimiento de virus').
significado_efecto(descongestionante, 'Alivia la congestión nasal o respiratoria').
significado_efecto(refrescante, 'Proporciona una sensación de frescura y alivio').
significado_efecto(antimalarica, 'Combate la malaria').
significado_efecto(hipoglucemiante, 'Reduce los niveles de glucosa en sangre').
significado_efecto(antioxidante, 'Protege contra el daño de radicales libres').

% Abrojo
accion_efecto_planta(abrojo, diuretica).
accion_efecto_planta(abrojo, antiinflamatoria_suave).
trata_enfermedad(abrojo, infecciones_urinarias).
trata_enfermedad(abrojo, inflamacion_leve).
elemento_planta(abrojo, saponinas).
elemento_planta(abrojo, flavonoides).
modo_preparacion(abrojo, infusion_hojas).
modo_tratamiento(abrojo, tomar_te_dos_veces_dia).
precaucion_planta(abrojo, evitar_en_embarazo).
nombre_cientifico(abrojo, 'Tribulus terrestris').
origen_planta(abrojo, mediterraneo).

% Acacia
accion_efecto_planta(acacia, astringente).
accion_efecto_planta(acacia, cicatrizante).
trata_enfermedad(acacia, diarrea).
trata_enfermedad(acacia, heridas_leves).
elemento_planta(acacia, taninos).
elemento_planta(acacia, goma_arabiga).
modo_preparacion(acacia, cocimiento_corteza).
modo_preparacion(acacia, goma_uso_externo).
modo_tratamiento(acacia, tomar_cocimiento_dos_veces_dia).
modo_tratamiento(acacia, aplicar_goma_en_heridas).
precaucion_planta(acacia, evitar_dosis_altas).
nombre_cientifico(acacia, 'Acacia senegal').
origen_planta(acacia, africa).

% Acanto
accion_efecto_planta(acanto, emoliente).
accion_efecto_planta(acanto, expectorante_suave).
trata_enfermedad(acanto, tos_seca).
trata_enfermedad(acanto, irritacion_piel).
elemento_planta(acanto, mucilagos).
elemento_planta(acanto, taninos).
modo_preparacion(acanto, infusion_hojas).
modo_preparacion(acanto, cataplasma).
modo_tratamiento(acanto, tomar_te_dos_veces_dia).
modo_tratamiento(acanto, aplicar_cataplasma_en_piel).
precaucion_planta(acanto, ninguna_relevante).
nombre_cientifico(acanto, 'Acanthus mollis').
origen_planta(acanto, mediterraneo).

% Aguacate
accion_efecto_planta(aguacate, nutritivo).
accion_efecto_planta(aguacate, antioxidante).
trata_enfermedad(aguacate, colesterol_alto).
trata_enfermedad(aguacate, piel_seca).
elemento_planta(aguacate, vitamina_e).
elemento_planta(aguacate, acidos_grasos).
modo_preparacion(aguacate, aceite_semillas).
modo_preparacion(aguacate, consumo_fresco).
modo_tratamiento(aguacate, aplicar_aceite_en_piel).
modo_tratamiento(aguacate, consumir_en_ensaladas).
precaucion_planta(aguacate, ninguna_relevante).
nombre_cientifico(aguacate, 'Persea americana').
origen_planta(aguacate, america).

% Ahuehuete
accion_efecto_planta(ahuehuete, astringente).
accion_efecto_planta(ahuehuete, cicatrizante).
trata_enfermedad(ahuehuete, heridas_leves).
trata_enfermedad(ahuehuete, diarrea).
elemento_planta(ahuehuete, taninos).
elemento_planta(ahuehuete, resinas).
modo_preparacion(ahuehuete, cocimiento_corteza).
modo_tratamiento(ahuehuete, tomar_cocimiento_dos_veces_dia).
precaucion_planta(ahuehuete, evitar_dosis_altas).
nombre_cientifico(ahuehuete, 'Taxodium mucronatum').
origen_planta(ahuehuete, america).

% Ajenjo
accion_efecto_planta(ajenjo, digestiva).
accion_efecto_planta(ajenjo, antiparasitaria).
trata_enfermedad(ajenjo, parasitos_intestinales).
trata_enfermedad(ajenjo, digestion_lenta).
elemento_planta(ajenjo, absintina).
elemento_planta(ajenjo, aceites_esenciales).
modo_preparacion(ajenjo, infusion_hojas).
modo_tratamiento(ajenjo, tomar_te_con_moderacion).
precaucion_planta(ajenjo, toxico_en_dosis_altas).
nombre_cientifico(ajenjo, 'Artemisia absinthium').
origen_planta(ajenjo, europa).

% Albahaca
accion_efecto_planta(albahaca, digestiva).
accion_efecto_planta(albahaca, antiespasmodica).
accion_efecto_planta(albahaca, calmante).
trata_enfermedad(albahaca, indigestion).
trata_enfermedad(albahaca, colicos).
trata_enfermedad(albahaca, ansiedad_leve).
elemento_planta(albahaca, aceites_esenciales).
elemento_planta(albahaca, linalool).
modo_preparacion(albahaca, infusion_hojas).
modo_preparacion(albahaca, consumo_fresco).
modo_tratamiento(albahaca, tomar_te_dos_veces_dia).
modo_tratamiento(albahaca, consumir_fresco_en_ensaladas).
precaucion_planta(albahaca, evitar_en_embarazo_dosis_altas).
nombre_cientifico(albahaca, 'Ocimum basilicum').
origen_planta(albahaca, asia).

% Alcachofa
accion_efecto_planta(alcachofa, hepatoprotectora).
accion_efecto_planta(alcachofa, diuretica).
trata_enfermedad(alcachofa, problemas_hepaticos).
trata_enfermedad(alcachofa, colesterol_alto).
elemento_planta(alcachofa, cinarina).
elemento_planta(alcachofa, flavonoides).
modo_preparacion(alcachofa, infusion_hojas).
modo_preparacion(alcachofa, consumo_fresco).
modo_tratamiento(alcachofa, tomar_te_dos_veces_dia).
modo_tratamiento(alcachofa, consumir_en_comidas).
precaucion_planta(alcachofa, ninguna_relevante).
nombre_cientifico(alcachofa, 'Cynara scolymus').
origen_planta(alcachofa, mediterraneo).

% Alcanfor
accion_efecto_planta(alcanfor, antiseptica).
accion_efecto_planta(alcanfor, descongestionante).
trata_enfermedad(alcanfor, congestión_nasal).
trata_enfermedad(alcanfor, dolor_muscular).
elemento_planta(alcanfor, alcanfor).
elemento_planta(alcanfor, aceites_esenciales).
modo_preparacion(alcanfor, aceite_esencial).
modo_tratamiento(alcanfor, aplicar_aceite_externamente).
precaucion_planta(alcanfor, evitar_uso_interno_toxico).
nombre_cientifico(alcanfor, 'Cinnamomum camphora').
origen_planta(alcanfor, asia).

% Amapola amarilla
accion_efecto_planta(amapola_amarilla, sedante_suave).
accion_efecto_planta(amapola_amarilla, antitusiva).
trata_enfermedad(amapola_amarilla, insomnio_leve).
trata_enfermedad(amapola_amarilla, tos_seca).
elemento_planta(amapola_amarilla, alcaloides).
elemento_planta(amapola_amarilla, flavonoides).
modo_preparacion(amapola_amarilla, infusion_flores).
modo_tratamiento(amapola_amarilla, tomar_te_antes_dormir).
precaucion_planta(amapola_amarilla, evitar_en_embarazo).
nombre_cientifico(amapola_amarilla, 'Eschscholzia californica').
origen_planta(amapola_amarilla, america).

% Amate
accion_efecto_planta(amate, astringente).
accion_efecto_planta(amate, cicatrizante).
trata_enfermedad(amate, heridas_leves).
trata_enfermedad(amate, diarrea).
elemento_planta(amate, taninos).
elemento_planta(amate, resinas).
modo_preparacion(amate, cocimiento_corteza).
modo_tratamiento(amate, aplicar_cocimiento_en_heridas).
precaucion_planta(amate, evitar_dosis_altas).
nombre_cientifico(amate, 'Ficus insipida').
origen_planta(amate, america).

% Anis
accion_efecto_planta(anis, digestiva).
accion_efecto_planta(anis, carminativa).
trata_enfermedad(anis, flatulencia).
trata_enfermedad(anis, digestion_lenta).
elemento_planta(anis, anetol).
elemento_planta(anis, aceites_esenciales).
modo_preparacion(anis, infusion_semillas).
modo_tratamiento(anis, tomar_te_despues_comidas).
precaucion_planta(anis, evitar_en_alergicos).
nombre_cientifico(anis, 'Pimpinella anisum').
origen_planta(anis, mediterraneo).

% Anacahuite
accion_efecto_planta(anacahuite, astringente).
accion_efecto_planta(anacahuite, antiinflamatoria).
trata_enfermedad(anacahuite, diarrea).
trata_enfermedad(anacahuite, inflamacion_piel).
elemento_planta(anacahuite, taninos).
elemento_planta(anacahuite, alcaloides).
modo_preparacion(anacahuite, cocimiento_corteza).
modo_tratamiento(anacahuite, tomar_cocimiento_dos_veces_dia).
precaucion_planta(anacahuite, evitar_dosis_altas).
nombre_cientifico(anacahuite, 'Cordia boissieri').
origen_planta(anacahuite, america).

% Árnica
accion_efecto_planta(arnica, antiinflamatoria).
accion_efecto_planta(arnica, analgesica_suave).
trata_enfermedad(arnica, contusiones).
trata_enfermedad(arnica, dolor_muscular).
elemento_planta(arnica, helenalina).
elemento_planta(arnica, flavonoides).
modo_preparacion(arnica, crema_topica).
modo_preparacion(arnica, infusion_hojas).
modo_tratamiento(arnica, aplicar_crema_en_zona_afectada).
modo_tratamiento(arnica, tomar_te_con_moderacion).
precaucion_planta(arnica, evitar_uso_interno_prolongado).
nombre_cientifico(arnica, 'Arnica montana').
origen_planta(arnica, europa).

% Barbasco
accion_efecto_planta(barbasco, antiinflamatoria).
accion_efecto_planta(barbasco, diuretica).
trata_enfermedad(barbasco, reumatismo).
trata_enfermedad(barbasco, infecciones_urinarias).
elemento_planta(barbasco, diosgenina).
elemento_planta(barbasco, saponinas).
modo_preparacion(barbasco, cocimiento_raiz).
modo_tratamiento(barbasco, tomar_cocimiento_dos_veces_dia).
precaucion_planta(barbasco, evitar_en_embarazo).
nombre_cientifico(barbasco, 'Dioscorea villosa').
origen_planta(barbasco, america).

% Belladona
accion_efecto_planta(belladona, antiespasmodica).
accion_efecto_planta(belladona, sedante).
trata_enfermedad(belladona, colicos).
trata_enfermedad(belladona, insomnio_leve).
elemento_planta(belladona, atropina).
elemento_planta(belladona, alcaloides).
modo_preparacion(belladona, extracto).
modo_tratamiento(belladona, uso_controlado_medico).
precaucion_planta(belladona, muy_toxica).
nombre_cientifico(belladona, 'Atropa belladonna').
origen_planta(belladona, europa).

% Brionia
accion_efecto_planta(brionia, laxante).
accion_efecto_planta(brionia, antiinflamatoria).
trata_enfermedad(brionia, estrenimiento_ocasional).
trata_enfermedad(brionia, dolor_articular).
elemento_planta(brionia, cucurbitacinas).
elemento_planta(brionia, resinas).
modo_preparacion(brionia, cocimiento_raiz).
modo_tratamiento(brionia, tomar_cocimiento_con_moderacion).
precaucion_planta(brionia, toxica_en_dosis_altas).
nombre_cientifico(brionia, 'Bryonia dioica').
origen_planta(brionia, europa).

% Canela
accion_efecto_planta(canela, digestiva).
accion_efecto_planta(canela, antimicrobiana).
trata_enfermedad(canela, digestion_lenta).
trata_enfermedad(canela, infecciones_leves).
elemento_planta(canela, cinamaldehido).
elemento_planta(canela, aceites_esenciales).
modo_preparacion(canela, infusion_corteza).
modo_tratamiento(canela, tomar_te_dos_veces_dia).
precaucion_planta(canela, evitar_dosis_altas_embarazo).
nombre_cientifico(canela, 'Cinnamomum verum').
origen_planta(canela, asia).

% Cedron
accion_efecto_planta(cedron, calmante).
accion_efecto_planta(cedron, digestiva).
trata_enfermedad(cedron, ansiedad_leve).
trata_enfermedad(cedron, indigestion).
elemento_planta(cedron, citronelal).
elemento_planta(cedron, aceites_esenciales).
modo_preparacion(cedron, infusion_hojas).
modo_tratamiento(cedron, tomar_te_antes_dormir).
precaucion_planta(cedron, ninguna_relevante).
nombre_cientifico(cedron, 'Aloysia citrodora').
origen_planta(cedron, sudamerica).

% Cardo
accion_efecto_planta(cardo, hepatoprotectora).
accion_efecto_planta(cardo, antioxidante).
trata_enfermedad(cardo, problemas_hepaticos).
trata_enfermedad(cardo, colesterol_alto).
elemento_planta(cardo, silimarina).
elemento_planta(cardo, flavonoides).
modo_preparacion(cardo, infusion_semillas).
modo_tratamiento(cardo, tomar_te_dos_veces_dia).
precaucion_planta(cardo, ninguna_relevante).
nombre_cientifico(cardo, 'Silybum marianum').
origen_planta(cardo, mediterraneo).

% Cempasuchil
accion_efecto_planta(cempasuchil, cicatrizante).
accion_efecto_planta(cempasuchil, antiinflamatoria_suave).
trata_enfermedad(cempasuchil, heridas_leves).
trata_enfermedad(cempasuchil, inflamacion_piel).
elemento_planta(cempasuchil, luteina).
elemento_planta(cempasuchil, aceites_esenciales).
modo_preparacion(cempasuchil, infusion_flores).
modo_preparacion(cempasuchil, cataplasma).
modo_tratamiento(cempasuchil, aplicar_cataplasma_en_piel).
modo_tratamiento(cempasuchil, tomar_te_dos_veces_dia).
precaucion_planta(cempasuchil, ninguna_relevante).
nombre_cientifico(cempasuchil, 'Tagetes erecta').
origen_planta(cempasuchil, america).

% Chaparro amargoso
accion_efecto_planta(chaparro_amargoso, antidiarreico).
accion_efecto_planta(chaparro_amargoso, astringente).
trata_enfermedad(chaparro_amargoso, diarrea).
trata_enfermedad(chaparro_amargoso, colitis).
elemento_planta(chaparro_amargoso, taninos).
elemento_planta(chaparro_amargoso, alcaloides).
modo_preparacion(chaparro_amargoso, cocimiento_corteza).
modo_tratamiento(chaparro_amargoso, tomar_cocimiento_dos_veces_dia).
precaucion_planta(chaparro_amargoso, evitar_dosis_altas).
nombre_cientifico(chaparro_amargoso, 'Castela tortuosa').
origen_planta(chaparro_amargoso, america).

% Chicalote
accion_efecto_planta(chicalote, sedante_suave).
accion_efecto_planta(chicalote, analgesica_suave).
trata_enfermedad(chicalote, insomnio_leve).
trata_enfermedad(chicalote, dolor_leve).
elemento_planta(chicalote, alcaloides).
elemento_planta(chicalote, flavonoides).
modo_preparacion(chicalote, infusion_hojas).
modo_tratamiento(chicalote, tomar_te_antes_dormir).
precaucion_planta(chicalote, evitar_dosis_altas).
nombre_cientifico(chicalote, 'Argemone mexicana').
origen_planta(chicalote, america).

% Chichigua
accion_efecto_planta(chichigua, nutritiva).
accion_efecto_planta(chichigua, galactogoga).
trata_enfermedad(chichigua, desnutricion).
trata_enfermedad(chichigua, baja_produccion_leche_materna).
elemento_planta(chichigua, proteinas).
elemento_planta(chichigua, vitamina_a).
modo_preparacion(chichigua, infusion_hojas).
modo_preparacion(chichigua, consumo_fresco).
modo_tratamiento(chichigua, consumir_en_ensaladas).
modo_tratamiento(chichigua, tomar_te_dos_veces_dia).
precaucion_planta(chichigua, ninguna_relevante).
nombre_cientifico(chichigua, 'Cnidoscolus aconitifolius').
origen_planta(chichigua, america).

% Cilantro
accion_efecto_planta(cilantro, digestiva).
accion_efecto_planta(cilantro, carminativa).
trata_enfermedad(cilantro, flatulencia).
trata_enfermedad(cilantro, indigestion).
elemento_planta(cilantro, linalool).
elemento_planta(cilantro, aceites_esenciales).
modo_preparacion(cilantro, infusion_hojas).
modo_preparacion(cilantro, consumo_fresco).
modo_tratamiento(cilantro, tomar_te_despues_comidas).
modo_tratamiento(cilantro, consumir_fresco_en_comidas).
precaucion_planta(cilantro, ninguna_relevante).
nombre_cientifico(cilantro, 'Coriandrum sativum').
origen_planta(cilantro, mediterraneo).

% Cocolmeca
accion_efecto_planta(cocolmeca, depurativa).
accion_efecto_planta(cocolmeca, diuretica).
trata_enfermedad(cocolmeca, infecciones_urinarias).
trata_enfermedad(cocolmeca, reumatismo).
elemento_planta(cocolmeca, saponinas).
elemento_planta(cocolmeca, resinas).
modo_preparacion(cocolmeca, cocimiento_raiz).
modo_tratamiento(cocolmeca, tomar_cocimiento_dos_veces_dia).
precaucion_planta(cocolmeca, evitar_en_embarazo).
nombre_cientifico(cocolmeca, 'Smilax aristolochiifolia').
origen_planta(cocolmeca, america).

% Cola de caballo
accion_efecto_planta(cola_de_caballo, diuretica).
accion_efecto_planta(cola_de_caballo, remineralizante).
trata_enfermedad(cola_de_caballo, infecciones_urinarias).
trata_enfermedad(cola_de_caballo, fragilidad_osea).
elemento_planta(cola_de_caballo, silice).
elemento_planta(cola_de_caballo, flavonoides).
modo_preparacion(cola_de_caballo, infusion_tallos).
modo_tratamiento(cola_de_caballo, tomar_te_dos_veces_dia).
precaucion_planta(cola_de_caballo, evitar_uso_prolongado).
nombre_cientifico(cola_de_caballo, 'Equisetum arvense').
origen_planta(cola_de_caballo, europa).

% Colchino
accion_efecto_planta(colchino, laxante_suave).
accion_efecto_planta(colchino, digestiva).
trata_enfermedad(colchino, estrenimiento_ocasional).
trata_enfermedad(colchino, digestion_lenta).
elemento_planta(colchino, mucilagos).
elemento_planta(colchino, taninos).
modo_preparacion(colchino, infusion_hojas).
modo_tratamiento(colchino, tomar_te_dos_veces_dia).
precaucion_planta(colchino, ninguna_relevante).
nombre_cientifico(colchino, 'Solanum nigrescens').
origen_planta(colchino, america).

% Comino
accion_efecto_planta(comino, carminativa).
accion_efecto_planta(comino, digestiva).
trata_enfermedad(comino, flatulencia).
trata_enfermedad(comino, indigestion).
elemento_planta(comino, cuminaldehido).
elemento_planta(comino, aceites_esenciales).
modo_preparacion(comino, infusion_semillas).
modo_tratamiento(comino, tomar_te_despues_comidas).
precaucion_planta(comino, ninguna_relevante).
nombre_cientifico(comino, 'Cuminum cyminum').
origen_planta(comino, mediterraneo).

% Colpachi
accion_efecto_planta(colpachi, antiinflamatoria).
accion_efecto_planta(colpachi, diuretica).
trata_enfermedad(colpachi, reumatismo).
trata_enfermedad(colpachi, infecciones_urinarias).
elemento_planta(colpachi, saponinas).
elemento_planta(colpachi, alcaloides).
modo_preparacion(colpachi, cocimiento_raiz).
modo_tratamiento(colpachi, tomar_cocimiento_dos_veces_dia).
precaucion_planta(colpachi, evitar_en_embarazo).
nombre_cientifico(colpachi, 'Berberis trifoliolata').
origen_planta(colpachi, america).

% Cuachalalate
accion_efecto_planta(cuachalalate, cicatrizante).
accion_efecto_planta(cuachalalate, antiinflamatoria).
trata_enfermedad(cuachalalate, ulceras_gastricas).
trata_enfermedad(cuachalalate, heridas_leves).
elemento_planta(cuachalalate, taninos).
elemento_planta(cuachalalate, resinas).
modo_preparacion(cuachalalate, cocimiento_corteza).
modo_tratamiento(cuachalalate, tomar_cocimiento_dos_veces_dia).
precaucion_planta(cuachalalate, ninguna_relevante).
nombre_cientifico(cuachalalate, 'Amphipterygium adstringens').
origen_planta(cuachalalate, america).

% Cuajiote
accion_efecto_planta(cuajiote, antimicrobiana).
accion_efecto_planta(cuajiote, cicatrizante).
trata_enfermedad(cuajiote, infecciones_piel).
trata_enfermedad(cuajiote, heridas_leves).
elemento_planta(cuajiote, taninos).
elemento_planta(cuajiote, aceites_esenciales).
modo_preparacion(cuajiote, cocimiento_corteza).
modo_tratamiento(cuajiote, aplicar_cocimiento_en_piel).
precaucion_planta(cuajiote, evitar_dosis_altas).
nombre_cientifico(cuajiote, 'Bursera simaruba').
origen_planta(cuajiote, america).

% Cuasia
accion_efecto_planta(cuasia, antiparasitaria).
accion_efecto_planta(cuasia, digestiva).
trata_enfermedad(cuasia, parasitos_intestinales).
trata_enfermedad(cuasia, digestion_lenta).
elemento_planta(cuasia, quassina).
elemento_planta(cuasia, alcaloides).
modo_preparacion(cuasia, infusion_corteza).
modo_tratamiento(cuasia, tomar_te_con_moderacion).
precaucion_planta(cuasia, toxica_en_dosis_altas).
nombre_cientifico(cuasia, 'Quassia amara').
origen_planta(cuasia, sudamerica).

% Damiana
accion_efecto_planta(damiana, afrodisíaca).
accion_efecto_planta(damiana, tonica).
trata_enfermedad(damiana, fatiga).
trata_enfermedad(damiana, disfuncion_sexual_leve).
elemento_planta(damiana, aceites_esenciales).
elemento_planta(damiana, taninos).
modo_preparacion(damiana, infusion_hojas).
modo_tratamiento(damiana, tomar_te_dos_veces_dia).
precaucion_planta(damiana, evitar_en_embarazo).
nombre_cientifico(damiana, 'Turnera diffusa').
origen_planta(damiana, america).

% Diente de leon
accion_efecto_planta(diente_de_leon, diuretica).
accion_efecto_planta(diente_de_leon, hepatoprotectora).
trata_enfermedad(diente_de_leon, infecciones_urinarias).
trata_enfermedad(diente_de_leon, problemas_hepaticos).
elemento_planta(diente_de_leon, inulina).
elemento_planta(diente_de_leon, sesquiterpenos).
modo_preparacion(diente_de_leon, infusion_raiz).
modo_tratamiento(diente_de_leon, tomar_te_dos_veces_dia).
precaucion_planta(diente_de_leon, ninguna_relevante).
nombre_cientifico(diente_de_leon, 'Taraxacum officinale').
origen_planta(diente_de_leon, europa).

% Digitaria
accion_efecto_planta(digitaria, diuretica).
accion_efecto_planta(digitaria, depurativa).
trata_enfermedad(digitaria, infecciones_urinarias).
trata_enfermedad(digitaria, intoxicacion_leve).
elemento_planta(digitaria, saponinas).
elemento_planta(digitaria, flavonoides).
modo_preparacion(digitaria, infusion_hojas).
modo_tratamiento(digitaria, tomar_te_dos_veces_dia).
precaucion_planta(digitaria, ninguna_relevante).
nombre_cientifico(digitaria, 'Digitaria sanguinalis').
origen_planta(digitaria, europa).

% Doradilla
accion_efecto_planta(doradilla, diuretica).
accion_efecto_planta(doradilla, antiinflamatoria_suave).
trata_enfermedad(doradilla, infecciones_urinarias).
trata_enfermedad(doradilla, reumatismo_leve).
elemento_planta(doradilla, flavonoides).
elemento_planta(doradilla, taninos).
modo_preparacion(doradilla, infusion_hojas).
modo_tratamiento(doradilla, tomar_te_dos_veces_dia).
precaucion_planta(doradilla, ninguna_relevante).
nombre_cientifico(doradilla, 'Ceterach officinarum').
origen_planta(doradilla, europa).

% Epazote
accion_efecto_planta(epazote, antiparasitaria).
accion_efecto_planta(epazote, carminativa).
trata_enfermedad(epazote, parasitos_intestinales).
trata_enfermedad(epazote, flatulencia).
elemento_planta(epazote, ascaridol).
elemento_planta(epazote, aceites_esenciales).
modo_preparacion(epazote, infusion_hojas).
modo_tratamiento(epazote, tomar_te_con_moderacion).
precaucion_planta(epazote, toxico_en_dosis_altas).
nombre_cientifico(epazote, 'Dysphania ambrosioides').
origen_planta(epazote, america).

% Enebro
accion_efecto_planta(enebro, diuretica).
accion_efecto_planta(enebro, antiseptica).
trata_enfermedad(enebro, infecciones_urinarias).
trata_enfermedad(enebro, reumatismo).
elemento_planta(enebro, aceites_esenciales).
elemento_planta(enebro, terpenos).
modo_preparacion(enebro, infusion_bayas).
modo_tratamiento(enebro, tomar_te_dos_veces_dia).
precaucion_planta(enebro, evitar_en_embarazo).
nombre_cientifico(enebro, 'Juniperus communis').
origen_planta(enebro, europa).

% Estafiate
accion_efecto_planta(estafiate, antiparasitaria).
accion_efecto_planta(estafiate, digestiva).
trata_enfermedad(estafiate, parasitos_intestinales).
trata_enfermedad(estafiate, indigestion).
elemento_planta(estafiate, sesquiterpenos).
elemento_planta(estafiate, aceites_esenciales).
modo_preparacion(estafiate, infusion_hojas).
modo_tratamiento(estafiate, tomar_te_con_moderacion).
precaucion_planta(estafiate, toxico_en_dosis_altas).
nombre_cientifico(estafiate, 'Artemisia ludoviciana').
origen_planta(estafiate, america).

% Eucalipto
accion_efecto_planta(eucalipto, expectorante).
accion_efecto_planta(eucalipto, antiseptica).
accion_efecto_planta(eucalipto, descongestionante).
trata_enfermedad(eucalipto, tos).
trata_enfermedad(eucalipto, sinusitis).
trata_enfermedad(eucalipto, infecciones_respiratorias).
elemento_planta(eucalipto, eucaliptol).
elemento_planta(eucalipto, taninos).
modo_preparacion(eucalipto, infusion_hojas).
modo_preparacion(eucalipto, inhalacion_vapor).
modo_tratamiento(eucalipto, inhalar_vapor_para_alivio_respiratorio).
modo_tratamiento(eucalipto, tomar_te_dos_veces_dia).
precaucion_planta(eucalipto, evitar_en_ninos_embarazo).
nombre_cientifico(eucalipto, 'Eucalyptus globulus').
origen_planta(eucalipto, australia).

% Fenogreco
accion_efecto_planta(fenogreco, galactogoga).
accion_efecto_planta(fenogreco, digestiva).
trata_enfermedad(fenogreco, baja_produccion_leche_materna).
trata_enfermedad(fenogreco, digestion_lenta).
elemento_planta(fenogreco, saponinas).
elemento_planta(fenogreco, mucilagos).
modo_preparacion(fenogreco, infusion_semillas).
modo_tratamiento(fenogreco, tomar_te_dos_veces_dia).
precaucion_planta(fenogreco, evitar_en_embarazo).
nombre_cientifico(fenogreco, 'Trigonella foenum-graecum').
origen_planta(fenogreco, mediterraneo).

% Genciana
accion_efecto_planta(genciana, digestiva).
accion_efecto_planta(genciana, tonica_amarga).
trata_enfermedad(genciana, digestion_lenta).
trata_enfermedad(genciana, falta_apetito).
elemento_planta(genciana, gentiopicrina).
elemento_planta(genciana, alcaloides).
modo_preparacion(genciana, infusion_raiz).
modo_tratamiento(genciana, tomar_te_antes_comidas).
precaucion_planta(genciana, evitar_en_ulceras_gastricas).
nombre_cientifico(genciana, 'Gentiana lutea').
origen_planta(genciana, europa).

% Geranio
accion_efecto_planta(geranio, astringente).
accion_efecto_planta(geranio, cicatrizante).
trata_enfermedad(geranio, heridas_leves).
trata_enfermedad(geranio, diarrea).
elemento_planta(geranio, taninos).
elemento_planta(geranio, aceites_esenciales).
modo_preparacion(geranio, infusion_hojas).
modo_preparacion(geranio, cataplasma).
modo_tratamiento(geranio, aplicar_cataplasma_en_piel).
modo_tratamiento(geranio, tomar_te_dos_veces_dia).
precaucion_planta(geranio, ninguna_relevante).
nombre_cientifico(geranio, 'Pelargonium graveolens').
origen_planta(geranio, africa).

% Girasol
accion_efecto_planta(girasol, nutritivo).
accion_efecto_planta(girasol, diuretica_suave).
trata_enfermedad(girasol, colesterol_alto).
trata_enfermedad(girasol, infecciones_urinarias_leves).
elemento_planta(girasol, vitamina_e).
elemento_planta(girasol, acidos_grasos).
modo_preparacion(girasol, infusion_semillas).
modo_preparacion(girasol, consumo_semillas).
modo_tratamiento(girasol, consumir_semillas_en_comidas).
modo_tratamiento(girasol, tomar_te_dos_veces_dia).
precaucion_planta(girasol, ninguna_relevante).
nombre_cientifico(girasol, 'Helianthus annuus').
origen_planta(girasol, america).

% Gingseng
accion_efecto_planta(gingseng, tonica).
accion_efecto_planta(gingseng, adaptogena).
trata_enfermedad(gingseng, fatiga).
trata_enfermedad(gingseng, estres).
elemento_planta(gingseng, ginsenosidos).
elemento_planta(gingseng, polisacaridos).
modo_preparacion(gingseng, infusion_raiz).
modo_tratamiento(gingseng, tomar_te_dos_veces_dia).
precaucion_planta(gingseng, evitar_en_hipertension).
nombre_cientifico(gingseng, 'Panax ginseng').
origen_planta(gingseng, asia).

% Gordolobo
accion_efecto_planta(gordolobo, expectorante).
accion_efecto_planta(gordolobo, antiinflamatoria_suave).
trata_enfermedad(gordolobo, tos).
trata_enfermedad(gordolobo, bronquitis).
elemento_planta(gordolobo, mucilagos).
elemento_planta(gordolobo, saponinas).
modo_preparacion(gordolobo, infusion_flores).
modo_tratamiento(gordolobo, tomar_te_dos_veces_dia).
precaucion_planta(gordolobo, ninguna_relevante).
nombre_cientifico(gordolobo, 'Verbascum thapsus').
origen_planta(gordolobo, europa).

% Grama
accion_efecto_planta(grama, diuretica).
accion_efecto_planta(grama, depurativa).
trata_enfermedad(grama, infecciones_urinarias).
trata_enfermedad(grama, intoxicacion_leve).
elemento_planta(grama, saponinas).
elemento_planta(grama, polisacaridos).
modo_preparacion(grama, infusion_rizoma).
modo_tratamiento(grama, tomar_te_dos_veces_dia).
precaucion_planta(grama, ninguna_relevante).
nombre_cientifico(grama, 'Agropyron repens').
origen_planta(grama, europa).

% Granado
accion_efecto_planta(granado, astringente).
accion_efecto_planta(granado, antiparasitaria).
trata_enfermedad(granado, diarrea).
trata_enfermedad(granado, parasitos_intestinales).
elemento_planta(granado, taninos).
elemento_planta(granado, alcaloides).
modo_preparacion(granado, cocimiento_corteza).
modo_tratamiento(granado, tomar_cocimiento_con_moderacion).
precaucion_planta(granado, toxico_en_dosis_altas).
nombre_cientifico(granado, 'Punica granatum').
origen_planta(granado, asia).

% Guaco
accion_efecto_planta(guaco, expectorante).
accion_efecto_planta(guaco, antiinflamatoria).
trata_enfermedad(guaco, tos).
trata_enfermedad(guaco, bronquitis).
elemento_planta(guaco, cumarinas).
elemento_planta(guaco, aceites_esenciales).
modo_preparacion(guaco, infusion_hojas).
modo_tratamiento(guaco, tomar_te_dos_veces_dia).
precaucion_planta(guaco, evitar_en_embarazo).
nombre_cientifico(guaco, 'Mikania glomerata').
origen_planta(guaco, sudamerica).

% Guazuma
accion_efecto_planta(guazuma, astringente).
accion_efecto_planta(guazuma, antiinflamatoria).
trata_enfermedad(guazuma, diarrea).
trata_enfermedad(guazuma, heridas_leves).
elemento_planta(guazuma, taninos).
elemento_planta(guazuma, mucilagos).
modo_preparacion(guazuma, cocimiento_corteza).
modo_tratamiento(guazuma, tomar_cocimiento_dos_veces_dia).
precaucion_planta(guazuma, ninguna_relevante).
nombre_cientifico(guazuma, 'Guazuma ulmifolia').
origen_planta(guazuma, america).

% Guayacan
accion_efecto_planta(guayacan, depurativa).
accion_efecto_planta(guayacan, antiinflamatoria).
trata_enfermedad(guayacan, reumatismo).
trata_enfermedad(guayacan, infecciones_piel).
elemento_planta(guayacan, resinas).
elemento_planta(guayacan, saponinas).
modo_preparacion(guayacan, cocimiento_corteza).
modo_tratamiento(guayacan, tomar_cocimiento_dos_veces_dia).
precaucion_planta(guayacan, evitar_dosis_altas).
nombre_cientifico(guayacan, 'Guaiacum officinale').
origen_planta(guayacan, america).

% Hamamelis
accion_efecto_planta(hamamelis, astringente).
accion_efecto_planta(hamamelis, cicatrizante).
trata_enfermedad(hamamelis, hemorroides).
trata_enfermedad(hamamelis, heridas_leves).
elemento_planta(hamamelis, taninos).
elemento_planta(hamamelis, flavonoides).
modo_preparacion(hamamelis, infusion_hojas).
modo_preparacion(hamamelis, crema_topica).
modo_tratamiento(hamamelis, aplicar_crema_en_zona_afectada).
modo_tratamiento(hamamelis, tomar_te_dos_veces_dia).
precaucion_planta(hamamelis, ninguna_relevante).
nombre_cientifico(hamamelis, 'Hamamelis virginiana').
origen_planta(hamamelis, america).

% Helenio
accion_efecto_planta(helenio, expectorante).
accion_efecto_planta(helenio, diuretica).
trata_enfermedad(helenio, tos).
trata_enfermedad(helenio, infecciones_urinarias).
elemento_planta(helenio, inulina).
elemento_planta(helenio, aceites_esenciales).
modo_preparacion(helenio, infusion_raiz).
modo_tratamiento(helenio, tomar_te_dos_veces_dia).
precaucion_planta(helenio, evitar_en_embarazo).
nombre_cientifico(helenio, 'Inula helenium').
origen_planta(helenio, europa).

% Jengibre
accion_efecto_planta(jengibre, digestiva).
accion_efecto_planta(jengibre, antiinflamatoria).
accion_efecto_planta(jengibre, antiemetica).
trata_enfermedad(jengibre, nausea).
trata_enfermedad(jengibre, digestion_lenta).
trata_enfermedad(jengibre, dolor_articular).
elemento_planta(jengibre, gingerol).
elemento_planta(jengibre, aceites_esenciales).
modo_preparacion(jengibre, infusion_raiz).
modo_preparacion(jengibre, consumo_fresco).
modo_tratamiento(jengibre, tomar_te_dos_veces_dia).
modo_tratamiento(jengibre, consumir_fresco_en_comidas).
precaucion_planta(jengibre, evitar_en_casos_de_reflujo).
nombre_cientifico(jengibre, 'Zingiber officinale').
origen_planta(jengibre, asia).

% Llantén
accion_efecto_planta(llanten, expectorante).
accion_efecto_planta(llanten, cicatrizante).
trata_enfermedad(llanten, tos).
trata_enfermedad(llanten, heridas_leves).
elemento_planta(llanten, mucilagos).
elemento_planta(llanten, aucubina).
modo_preparacion(llanten, infusion_hojas).
modo_preparacion(llanten, cataplasma).
modo_tratamiento(llanten, tomar_te_dos_veces_dia).
modo_tratamiento(llanten, aplicar_cataplasma_en_piel).
precaucion_planta(llanten, ninguna_relevante).
nombre_cientifico(llanten, 'Plantago major').
origen_planta(llanten, europa).

% Madreselva
accion_efecto_planta(madreselva, antibacteriana).
accion_efecto_planta(madreselva, antiviral).
trata_enfermedad(madreselva, resfriado).
trata_enfermedad(madreselva, infecciones_leves).
elemento_planta(madreselva, acido_clorogenico).
elemento_planta(madreselva, flavonoides).
modo_preparacion(madreselva, infusion_flores).
modo_tratamiento(madreselva, tomar_te_dos_veces_dia).
precaucion_planta(madreselva, evitar_dosis_altas).
nombre_cientifico(madreselva, 'Lonicera japonica').
origen_planta(madreselva, asia).

% Linaza
accion_efecto_planta(linaza, laxante_suave).
accion_efecto_planta(linaza, antiinflamatoria).
trata_enfermedad(linaza, estrenimiento_ocasional).
trata_enfermedad(linaza, inflamacion_intestinal).
elemento_planta(linaza, mucilagos).
elemento_planta(linaza, acidos_grasos_omega_3).
modo_preparacion(linaza, infusion_semillas).
modo_preparacion(linaza, consumo_semillas).
modo_tratamiento(linaza, tomar_te_o_semillas_con_agua).
modo_tratamiento(linaza, consumir_en_comidas).
precaucion_planta(linaza, ninguna_relevante).
nombre_cientifico(linaza, 'Linum usitatissimum').
origen_planta(linaza, mediterraneo).

% Maguey
accion_efecto_planta(maguey, cicatrizante).
accion_efecto_planta(maguey, antiinflamatoria).
trata_enfermedad(maguey, heridas_leves).
trata_enfermedad(maguey, inflamacion_piel).
elemento_planta(maguey, saponinas).
elemento_planta(maguey, fructanos).
modo_preparacion(maguey, jugo_hojas).
modo_preparacion(maguey, cataplasma).
modo_tratamiento(maguey, aplicar_jugo_en_piel).
modo_tratamiento(maguey, aplicar_cataplasma_en_zona_afectada).
precaucion_planta(maguey, evitar_contacto_con_ojos).
nombre_cientifico(maguey, 'Agave americana').
origen_planta(maguey, america).

% Hinojo
accion_efecto_planta(hinojo, carminativa).
accion_efecto_planta(hinojo, digestiva).
trata_enfermedad(hinojo, flatulencia).
trata_enfermedad(hinojo, digestion_lenta).
elemento_planta(hinojo, anetol).
elemento_planta(hinojo, aceites_esenciales).
modo_preparacion(hinojo, infusion_semillas).
modo_tratamiento(hinojo, tomar_te_despues_comidas).
precaucion_planta(hinojo, ninguna_relevante).
nombre_cientifico(hinojo, 'Foeniculum vulgare').
origen_planta(hinojo, mediterraneo).

% Hierba del pollo
accion_efecto_planta(hierba_del_pollo, diuretica).
accion_efecto_planta(hierba_del_pollo, depurativa).
trata_enfermedad(hierba_del_pollo, infecciones_urinarias).
trata_enfermedad(hierba_del_pollo, intoxicacion_leve).
elemento_planta(hierba_del_pollo, flavonoides).
elemento_planta(hierba_del_pollo, saponinas).
modo_preparacion(hierba_del_pollo, infusion_hojas).
modo_tratamiento(hierba_del_pollo, tomar_te_dos_veces_dia).
precaucion_planta(hierba_del_pollo, ninguna_relevante).
nombre_cientifico(hierba_del_pollo, 'Commelina diffusa').
origen_planta(hierba_del_pollo, america).

% Jalapa
accion_efecto_planta(jalapa, laxante).
accion_efecto_planta(jalapa, purgante).
trata_enfermedad(jalapa, estrenimiento_ocasional).
trata_enfermedad(jalapa, intoxicacion_leve).
elemento_planta(jalapa, resinas).
elemento_planta(jalapa, glucosidos).
modo_preparacion(jalapa, cocimiento_raiz).
modo_tratamiento(jalapa, tomar_cocimiento_con_moderacion).
precaucion_planta(jalapa, toxica_en_dosis_altas).
nombre_cientifico(jalapa, 'Mirabilis jalapa').
origen_planta(jalapa, america).

% Ipecacuana
accion_efecto_planta(ipecacuana, emetica).
accion_efecto_planta(ipecacuana, expectorante).
trata_enfermedad(ipecacuana, intoxicacion).
trata_enfermedad(ipecacuana, tos).
elemento_planta(ipecacuana, emetina).
elemento_planta(ipecacuana, alcaloides).
modo_preparacion(ipecacuana, extracto_raiz).
modo_tratamiento(ipecacuana, uso_controlado_medico).
precaucion_planta(ipecacuana, muy_toxica).
nombre_cientifico(ipecacuana, 'Carapichea ipecacuanha').
origen_planta(ipecacuana, sudamerica).

% Jazmin Amarillo
accion_efecto_planta(jazmin_amarillo, calmante).
accion_efecto_planta(jazmin_amarillo, sedante_suave).
trata_enfermedad(jazmin_amarillo, ansiedad_leve).
trata_enfermedad(jazmin_amarillo, insomnio_leve).
elemento_planta(jazmin_amarillo, aceites_esenciales).
elemento_planta(jazmin_amarillo, flavonoides).
modo_preparacion(jazmin_amarillo, infusion_flores).
modo_tratamiento(jazmin_amarillo, tomar_te_antes_dormir).
precaucion_planta(jazmin_amarillo, evitar_dosis_altas).
nombre_cientifico(jazmin_amarillo, 'Gelsemium sempervirens').
origen_planta(jazmin_amarillo, america).

% Maiz
accion_efecto_planta(maiz, diuretica).
accion_efecto_planta(maiz, nutritiva).
trata_enfermedad(maiz, infecciones_urinarias).
trata_enfermedad(maiz, desnutricion).
elemento_planta(maiz, almidon).
elemento_planta(maiz, vitamina_b).
modo_preparacion(maiz, infusion_barbas).
modo_preparacion(maiz, consumo_granos).
modo_tratamiento(maiz, tomar_te_dos_veces_dia).
modo_tratamiento(maiz, consumir_en_comidas).
precaucion_planta(maiz, ninguna_relevante).
nombre_cientifico(maiz, 'Zea mays').
origen_planta(maiz, america).

% Malva
accion_efecto_planta(malva, emoliente).
accion_efecto_planta(malva, expectorante_suave).
trata_enfermedad(malva, tos_seca).
trata_enfermedad(malva, irritacion_piel).
elemento_planta(malva, mucilagos).
elemento_planta(malva, flavonoides).
modo_preparacion(malva, infusion_hojas).
modo_preparacion(malva, cataplasma).
modo_tratamiento(malva, tomar_te_dos_veces_dia).
modo_tratamiento(malva, aplicar_cataplasma_en_piel).
precaucion_planta(malva, ninguna_relevante).
nombre_cientifico(malva, 'Malva sylvestris').
origen_planta(malva, europa).

% Malvavisco
accion_efecto_planta(malvavisco, emoliente).
accion_efecto_planta(malvavisco, expectorante).
trata_enfermedad(malvavisco, tos).
trata_enfermedad(malvavisco, irritacion_garganta).
elemento_planta(malvavisco, mucilagos).
elemento_planta(malvavisco, pectinas).
modo_preparacion(malvavisco, infusion_raiz).
modo_tratamiento(malvavisco, tomar_te_dos_veces_dia).
precaucion_planta(malvavisco, ninguna_relevante).
nombre_cientifico(malvavisco, 'Althaea officinalis').
origen_planta(malvavisco, europa).

% Manzanilla
accion_efecto_planta(manzanilla, calmante).
accion_efecto_planta(manzanilla, antiinflamatoria).
accion_efecto_planta(manzanilla, digestiva).
trata_enfermedad(manzanilla, insomnio).
trata_enfermedad(manzanilla, indigestion).
trata_enfermedad(manzanilla, irritacion_piel).
elemento_planta(manzanilla, chamazulene).
elemento_planta(manzanilla, flavonoides).
modo_preparacion(manzanilla, infusion_flores).
modo_preparacion(manzanilla, compresa_topica).
modo_tratamiento(manzanilla, tomar_te_antes_dormir).
modo_tratamiento(manzanilla, aplicar_compresa_en_piel).
precaucion_planta(manzanilla, alergia_asteraceas).
nombre_cientifico(manzanilla, 'Matricaria chamomilla').
origen_planta(manzanilla, europa).

% Mangle
accion_efecto_planta(mangle, astringente).
accion_efecto_planta(mangle, cicatrizante).
trata_enfermedad(mangle, diarrea).
trata_enfermedad(mangle, heridas_leves).
elemento_planta(mangle, taninos).
elemento_planta(mangle, polifenoles).
modo_preparacion(mangle, cocimiento_corteza).
modo_tratamiento(mangle, tomar_cocimiento_dos_veces_dia).
precaucion_planta(mangle, evitar_dosis_altas).
nombre_cientifico(mangle, 'Rhizophora mangle').
origen_planta(mangle, america).

% Marrubio
accion_efecto_planta(marrubio, expectorante).
accion_efecto_planta(marrubio, digestiva).
trata_enfermedad(marrubio, tos).
trata_enfermedad(marrubio, digestion_lenta).
elemento_planta(marrubio, marrubina).
elemento_planta(marrubio, aceites_esenciales).
modo_preparacion(marrubio, infusion_hojas).
modo_tratamiento(marrubio, tomar_te_dos_veces_dia).
precaucion_planta(marrubio, evitar_en_embarazo).
nombre_cientifico(marrubio, 'Marrubium vulgare').
origen_planta(marrubio, europa).

% Marihuana
accion_efecto_planta(marihuana, analgesica).
accion_efecto_planta(marihuana, antiemetica).
trata_enfermedad(marihuana, dolor_cronico).
trata_enfermedad(marihuana, nausea).
elemento_planta(marihuana, cannabinoides).
elemento_planta(marihuana, terpenos).
modo_preparacion(marihuana, infusion_hojas).
modo_tratamiento(marihuana, uso_controlado_medico).
precaucion_planta(marihuana, uso_regulado_legalmente).
nombre_cientifico(marihuana, 'Cannabis sativa').
origen_planta(marihuana, asia).

% Mastuerzo
accion_efecto_planta(mastuerzo, expectorante).
accion_efecto_planta(mastuerzo, diuretica).
accion_efecto_planta(mastuerzo, antibacteriana).
trata_enfermedad(mastuerzo, tos).
trata_enfermedad(mastuerzo, infecciones_urinarias).
trata_enfermedad(mastuerzo, infecciones_leves).
elemento_planta(mastuerzo, vitamina_c).
elemento_planta(mastuerzo, glucosinolatos).
modo_preparacion(mastuerzo, infusion_hojas).
modo_preparacion(mastuerzo, consumo_fresco).
modo_tratamiento(mastuerzo, tomar_te_dos_veces_dia).
modo_tratamiento(mastuerzo, consumir_fresco_en_ensaladas).
precaucion_planta(mastuerzo, ninguna_relevante).
nombre_cientifico(mastuerzo, 'Tropaeolum majus').
origen_planta(mastuerzo, sudamerica).


% Matarique
accion_efecto_planta(matarique, analgesica).
accion_efecto_planta(matarique, antiinflamatoria).
trata_enfermedad(matarique, dolor_articular).
trata_enfermedad(matarique, reumatismo).
elemento_planta(matarique, sesquiterpenos).
elemento_planta(matarique, alcaloides).
modo_preparacion(matarique, cocimiento_raiz).
modo_tratamiento(matarique, tomar_cocimiento_dos_veces_dia).
precaucion_planta(matarique, evitar_en_embarazo).
nombre_cientifico(matarique, 'Psacalium decompositum').
origen_planta(matarique, america).

% Menta
accion_efecto_planta(menta, digestiva).
accion_efecto_planta(menta, antiespasmodica).
accion_efecto_planta(menta, refrescante).
trata_enfermedad(menta, indigestion).
trata_enfermedad(menta, sindrome_intestino_irritable).
trata_enfermedad(menta, dolor_cabeza).
elemento_planta(menta, mentol).
elemento_planta(menta, aceites_esenciales).
modo_preparacion(menta, infusion_hojas).
modo_preparacion(menta, aceite_esencial).
modo_tratamiento(menta, tomar_te_despues_comidas).
modo_tratamiento(menta, aplicar_aceite_diluido_dolor_cabeza).
precaucion_planta(menta, evitar_en_reflujo_acido).
nombre_cientifico(menta, 'Mentha piperita').
origen_planta(menta, europa).

% Oregano
accion_efecto_planta(oregano, antimicrobiana).
accion_efecto_planta(oregano, expectorante).
accion_efecto_planta(oregano, digestiva).
trata_enfermedad(oregano, infecciones_respiratorias).
trata_enfermedad(oregano, tos).
trata_enfermedad(oregano, indigestion).
elemento_planta(oregano, carvacrol).
elemento_planta(oregano, aceites_esenciales).
modo_preparacion(oregano, infusion_hojas).
modo_preparacion(oregano, consumo_fresco).
modo_tratamiento(oregano, tomar_te_dos_veces_dia).
modo_tratamiento(oregano, consumir_fresco_en_comidas).
precaucion_planta(oregano, evitar_en_embarazo_dosis_altas).
nombre_cientifico(oregano, 'Origanum vulgare').
origen_planta(oregano, mediterraneo).

% Palo de flor
accion_efecto_planta(palo_de_flor, astringente).
accion_efecto_planta(palo_de_flor, cicatrizante).
trata_enfermedad(palo_de_flor, diarrea).
trata_enfermedad(palo_de_flor, heridas_leves).
elemento_planta(palo_de_flor, taninos).
elemento_planta(palo_de_flor, resinas).
modo_preparacion(palo_de_flor, cocimiento_corteza).
modo_tratamiento(palo_de_flor, tomar_cocimiento_dos_veces_dia).
precaucion_planta(palo_de_flor, evitar_dosis_altas).
nombre_cientifico(palo_de_flor, 'Tabebuia rosea').
origen_planta(palo_de_flor, america).

% Pericon
accion_efecto_planta(pericon, digestiva).
accion_efecto_planta(pericon, calmante).
trata_enfermedad(pericon, indigestion).
trata_enfermedad(pericon, ansiedad_leve).
elemento_planta(pericon, aceites_esenciales).
elemento_planta(pericon, flavonoides).
modo_preparacion(pericon, infusion_hojas).
modo_tratamiento(pericon, tomar_te_dos_veces_dia).
precaucion_planta(pericon, evitar_en_embarazo).
nombre_cientifico(pericon, 'Tagetes lucida').
origen_planta(pericon, america).

% Pasiflora
accion_efecto_planta(pasiflora, sedante).
accion_efecto_planta(pasiflora, calmante).
trata_enfermedad(pasiflora, insomnio).
trata_enfermedad(pasiflora, ansiedad).
elemento_planta(pasiflora, alcaloides).
elemento_planta(pasiflora, flavonoides).
modo_preparacion(pasiflora, infusion_hojas).
modo_tratamiento(pasiflora, tomar_te_antes_dormir).
precaucion_planta(pasiflora, evitar_en_embarazo).
nombre_cientifico(pasiflora, 'Passiflora incarnata').
origen_planta(pasiflora, america).

% Pingüica
accion_efecto_planta(pinguica, diuretica).
accion_efecto_planta(pinguica, antioxidante).
trata_enfermedad(pinguica, infecciones_urinarias).
trata_enfermedad(pinguica, inflamacion_leve).
elemento_planta(pinguica, arbutina).
elemento_planta(pinguica, flavonoides).
modo_preparacion(pinguica, infusion_hojas).
modo_tratamiento(pinguica, tomar_te_dos_veces_dia).
precaucion_planta(pinguica, ninguna_relevante).
nombre_cientifico(pinguica, 'Arctostaphylos pungens').
origen_planta(pinguica, america).

% Prodigiosa
accion_efecto_planta(prodigiosa, hepatoprotectora).
accion_efecto_planta(prodigiosa, digestiva).
trata_enfermedad(prodigiosa, problemas_hepaticos).
trata_enfermedad(prodigiosa, digestion_lenta).
elemento_planta(prodigiosa, acido_clorogenico).
elemento_planta(prodigiosa, flavonoides).
modo_preparacion(prodigiosa, infusion_hojas).
modo_tratamiento(prodigiosa, tomar_te_dos_veces_dia).
precaucion_planta(prodigiosa, ninguna_relevante).
nombre_cientifico(prodigiosa, 'Kalanchoe pinnata').
origen_planta(prodigiosa, america).

% Pirul
accion_efecto_planta(pirul, antimicrobiana).
accion_efecto_planta(pirul, cicatrizante).
trata_enfermedad(pirul, infecciones_piel).
trata_enfermedad(pirul, heridas_leves).
elemento_planta(pirul, aceites_esenciales).
elemento_planta(pirul, taninos).
modo_preparacion(pirul, infusion_hojas).
modo_preparacion(pirul, cataplasma).
modo_tratamiento(pirul, aplicar_cataplasma_en_piel).
modo_tratamiento(pirul, tomar_te_dos_veces_dia).
precaucion_planta(pirul, evitar_dosis_altas).
nombre_cientifico(pirul, 'Schinus molle').
origen_planta(pirul, sudamerica).

% Pulsatilla
accion_efecto_planta(pulsatilla, sedante).
accion_efecto_planta(pulsatilla, antiespasmodica).
trata_enfermedad(pulsatilla, insomnio_leve).
trata_enfermedad(pulsatilla, colicos).
elemento_planta(pulsatilla, anemonina).
elemento_planta(pulsatilla, saponinas).
modo_preparacion(pulsatilla, infusion_hojas).
modo_tratamiento(pulsatilla, tomar_te_con_moderacion).
precaucion_planta(pulsatilla, toxica_en_dosis_altas).
nombre_cientifico(pulsatilla, 'Pulsatilla vulgaris').
origen_planta(pulsatilla, europa).

% Quebracho
accion_efecto_planta(quebracho, astringente).
accion_efecto_planta(quebracho, expectorante).
trata_enfermedad(quebracho, diarrea).
trata_enfermedad(quebracho, tos).
elemento_planta(quebracho, taninos).
elemento_planta(quebracho, alcaloides).
modo_preparacion(quebracho, cocimiento_corteza).
modo_tratamiento(quebracho, tomar_cocimiento_dos_veces_dia).
precaucion_planta(quebracho, evitar_dosis_altas).
nombre_cientifico(quebracho, 'Schinopsis lorentzii').
origen_planta(quebracho, sudamerica).

% Quina
accion_efecto_planta(quina, antimalarica).
accion_efecto_planta(quina, febrifuga).
trata_enfermedad(quina, malaria).
trata_enfermedad(quina, fiebre).
elemento_planta(quina, quinina).
elemento_planta(quina, alcaloides).
modo_preparacion(quina, infusion_corteza).
modo_tratamiento(quina, uso_controlado_medico).
precaucion_planta(quina, toxica_en_dosis_altas).
nombre_cientifico(quina, 'Cinchona officinalis').
origen_planta(quina, sudamerica).
medicamento_planta(quinina, quina).
accion_medicamento(quinina, antimalarica).

% Tila
accion_efecto_planta(tila, calmante).
accion_efecto_planta(tila, sedante).
trata_enfermedad(tila, ansiedad).
trata_enfermedad(tila, insomnio).
elemento_planta(tila, flavonoides).
elemento_planta(tila, aceites_esenciales).
modo_preparacion(tila, infusion_flores).
modo_tratamiento(tila, tomar_te_antes_dormir).
precaucion_planta(tila, ninguna_relevante).
nombre_cientifico(tila, 'Tilia cordata').
origen_planta(tila, europa).

% Te de milpa
accion_efecto_planta(te_de_milpa, diuretica).
accion_efecto_planta(te_de_milpa, digestiva).
trata_enfermedad(te_de_milpa, infecciones_urinarias).
trata_enfermedad(te_de_milpa, indigestion).
elemento_planta(te_de_milpa, flavonoides).
elemento_planta(te_de_milpa, taninos).
modo_preparacion(te_de_milpa, infusion_hojas).
modo_tratamiento(te_de_milpa, tomar_te_dos_veces_dia).
precaucion_planta(te_de_milpa, ninguna_relevante).
nombre_cientifico(te_de_milpa, 'Zea mays').
origen_planta(te_de_milpa, america).

% Toloache
accion_efecto_planta(toloache, sedante).
accion_efecto_planta(toloache, analgesica).
trata_enfermedad(toloache, insomnio).
trata_enfermedad(toloache, dolor_leve).
elemento_planta(toloache, alcaloides).
elemento_planta(toloache, escopolamina).
modo_preparacion(toloache, infusion_hojas).
modo_tratamiento(toloache, uso_controlado_medico).
precaucion_planta(toloache, muy_toxica).
nombre_cientifico(toloache, 'Datura stramonium').
origen_planta(toloache, america).

% Tronadora
accion_efecto_planta(tronadora, hipoglucemiante).
accion_efecto_planta(tronadora, digestiva).
trata_enfermedad(tronadora, diabetes_leve).
trata_enfermedad(tronadora, indigestion).
elemento_planta(tronadora, alcaloides).
elemento_planta(tronadora, flavonoides).
modo_preparacion(tronadora, infusion_hojas).
modo_tratamiento(tronadora, tomar_te_dos_veces_dia).
precaucion_planta(tronadora, evitar_en_hipoglucemia).
nombre_cientifico(tronadora, 'Tecoma stans').
origen_planta(tronadora, america).

% Tripa de judas
accion_efecto_planta(tripa_de_judas, laxante).
accion_efecto_planta(tripa_de_judas, diuretica).
trata_enfermedad(tripa_de_judas, estrenimiento_ocasional).
trata_enfermedad(tripa_de_judas, infecciones_urinarias).
elemento_planta(tripa_de_judas, antraquinonas).
elemento_planta(tripa_de_judas, taninos).
modo_preparacion(tripa_de_judas, infusion_hojas).
modo_tratamiento(tripa_de_judas, tomar_te_con_moderacion).
precaucion_planta(tripa_de_judas, evitar_uso_prolongado).
nombre_cientifico(tripa_de_judas, 'Cassia fistula').
origen_planta(tripa_de_judas, asia).

% Uva
accion_efecto_planta(uva, antioxidante).
accion_efecto_planta(uva, diuretica).
trata_enfermedad(uva, colesterol_alto).
trata_enfermedad(uva, infecciones_urinarias).
elemento_planta(uva, resveratrol).
elemento_planta(uva, vitamina_c).
modo_preparacion(uva, consumo_fresco).
modo_preparacion(uva, infusion_hojas).
modo_tratamiento(uva, consumir_fruta_fresca).
modo_tratamiento(uva, tomar_te_dos_veces_dia).
precaucion_planta(uva, ninguna_relevante).
nombre_cientifico(uva, 'Vitis vinifera').
origen_planta(uva, europa).

% Nopal
accion_efecto_planta(nopal, hipoglucemiante).
accion_efecto_planta(nopal, digestiva).
trata_enfermedad(nopal, diabetes_leve).
trata_enfermedad(nopal, digestion_lenta).
elemento_planta(nopal, mucilagos).
elemento_planta(nopal, fibra).
modo_preparacion(nopal, consumo_fresco).
modo_preparacion(nopal, infusion_hojas).
modo_tratamiento(nopal, consumir_en_ensaladas).
modo_tratamiento(nopal, tomar_te_dos_veces_dia).
precaucion_planta(nopal, ninguna_relevante).
nombre_cientifico(nopal, 'Opuntia ficus-indica').
origen_planta(nopal, america).

