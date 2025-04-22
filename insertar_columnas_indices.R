# 1) Calculamos min/max de tiempo antes de mutate
tiempo_min <- min(filtered_data_set$tiempo_residencia, na.rm = TRUE)
tiempo_max <- max(filtered_data_set$tiempo_residencia, na.rm = TRUE)
filtered_data_set <- data.frame(filtered_data_set)
filtered_data_set <- filtered_data_set %>%
  mutate(
    # Umbral de hacinamiento
    umbralHacinamiento = max_personas / num_dormitorios,
    
    # IFM: puntajes por componente
    score_techo = case_when(
      material_techo == "Losa de viguetas"             ~ 1,
      material_techo == "Chapa"                        ~ 0.25,
      material_techo == "Lona"                         ~ 0.25,
      material_techo == "Caña/Adobe"                   ~ 0
  ),
  score_paredes = case_when(
    material_paredes == "Mampostería (ladrillo/block"                    ~ 1,
    material_paredes %in% c("Planchón (describir qué es)", "Adobe")      ~ 0.75,
    material_paredes == "Trama en madera/tapial de madera"               ~ 0.25,
    material_paredes == "Chapa"                                          ~ 0
  ),
  score_piso = case_when(
    material_piso == "Cerámico"                      ~ 1,
    material_piso == "Madera"                        ~ 0.75,
    material_piso == "Carpeta de cemento"            ~ 0.25,
    material_piso == "Sin piso/tierra"               ~ 0
  ),
  score_desague = case_when(
    tipo_desague == "Desagüe a red cloacal pública"                     ~ 1,
    tipo_desague == "Desagüe a red cloacal informal/vecinal"            ~ 1,
    tipo_desague == "A cámara séptica"                                  ~ 0.75,
    tipo_desague == "No sabe"                                           ~ 0.5,
    tipo_desague == "A pozo negro/ciego"                                ~ 0
  ),
  IFM = (score_techo + score_paredes + score_piso + score_desague) / 4,
  
  # IEH: puntaje de tenencia y normalizaciones
  score_tenencia = case_when(
    lugar_habitado == "Alquilado"                                ~ 4,
    lugar_habitado == "Propio con algún comprobante de tenencia" ~ 4,
    lugar_habitado == "Propio sin títulos"                       ~ 3,
    lugar_habitado == "Prestado"                                 ~ 2,
    lugar_habitado == "Ocupado/Tomado"                           ~ 1
  ),
  tenencia_norm = (score_tenencia - 1) / 3,
  tiempo_norm   = (tiempo_residencia - tiempo_min) / (tiempo_max - tiempo_min),
  IEH           = (tenencia_norm + tiempo_norm) / 2
) %>%
  # Quitamos columnas intermedias si no las necesitamos
  select(-starts_with("score_"), -tenencia_norm, -tiempo_norm)
