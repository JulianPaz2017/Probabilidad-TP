# 1) Calculamos min/max de tiempo antes de mutate
tiempo_min <- min(filtered_data_set$tiempo_residencia, na.rm = TRUE)
tiempo_max <- max(filtered_data_set$tiempo_residencia, na.rm = TRUE)
uh <- filtered_data_set$cant_integrantes_del_hogar/filtered_data_set$cant_dormitorios

filtered_data_set <- data.frame(filtered_data_set)
filtered_data_set <- filtered_data_set %>%
  mutate(
    # Umbral de hacinamiento
    umbralHacinamiento = uh,
    
    # IFM: puntajes por componente
    score_techo = case_when(
      material_techo == "Losa de viguetas"             ~ 1,
      material_techo == "Chapa"                        ~ 0.75,
      material_techo == "Caña/Adobe"                   ~ 0.25,
      material_techo == "Lona"                         ~ 0,
  ),
  score_paredes = case_when(
    material_paredes == "Mampostería (ladrillo/block"                    ~ 1,
    material_paredes == "Planchón (describir qué es)"                    ~ 0.75,
    material_paredes == "Adobe"                                          ~ 0.25,
    material_paredes == "Trama en madera/tapial de madera"               ~ 0.25,
    material_paredes == "Chapa"                                          ~ 0
  ),
  score_piso = case_when(
    material_piso == "Cerámico"                      ~ 1,
    material_piso == "Madera"                        ~ 0.75,
    material_piso == "Carpeta de cemento"            ~ 0.25,
    material_piso == "Sin piso/tierra"               ~ 0
  ),
  
  IFM = (score_techo + score_paredes + score_piso) / 3,
  
  # IEH: puntaje de tenencia y normalizaciones
  score_tenencia = case_when(
    vinculo_vivienda == "Alquilado"                                ~ 4,
    vinculo_vivienda == "Propio con algún comprobante de tenencia" ~ 4,
    vinculo_vivienda == "Propio sin títulos"                       ~ 3,
    vinculo_vivienda == "Prestado"                                 ~ 2,
    vinculo_vivienda == "Ocupado/Tomado"                           ~ 1
  ),
  tenencia_norm = (score_tenencia - 1) / 3,
  tiempo_norm   = (tiempo_residencia - tiempo_min) / (tiempo_max - tiempo_min),
  IEH           = (tenencia_norm + tiempo_norm) / 2
) %>%
  # Quitamos columnas intermedias si no las necesitamos
  select(-starts_with("score_"), -tenencia_norm, -tiempo_norm)

