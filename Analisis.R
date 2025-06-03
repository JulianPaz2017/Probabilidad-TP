library(ggplot2)

# HISTOGRAMA HUMBRAL DE HACINAMIENTO
# Establecemos los intervalos
breakpoints <- 0:10

# Pasamos el dataset values, dividimos los datos en los intervalos dados por breakpoints, eliminamos los 
# ejes X e Y con xaxt e yaxt, y asignamos freq=TRUE para que las alturas de las barras estén bien
uh_histogram <- hist(x = filtered_data_set$umbralHacinamiento,
                           breaks = breakpoints,
                           ylim = c(0,500),
                           xlab = "Umbral de Hacinamiento",
                           ylab = "Frecuencia Absoluta",
                           main = "Distribución del Umbral de Hacinamiento en Villas de Argentina en 2022",
                           col = "lightpink",
                           right = FALSE,
                           include.lowest = TRUE,
                           xaxt = "n",
                           yaxt = "n",
                           freq=TRUE
                           )

# Creamos el eje X manualmente
axis(side = 1,
     font = 2,
     at = seq(0, 10, by = 1),
     labels = seq(0, 10, by = 1),
     cex.axis = 0.8)

# Creamos el eje Y manualmente
axis(side = 2,
     font = 2,
     at = seq(0, 500, by = 100),
     labels = seq(0, 500, by = 100),
     cex.axis = 0.8)

# Agregamos la fuente
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=3,
      at = -1.2,
      adj=0,
      cex=0.9)

# GRÁFICO DE BARRAS PARA PISO
filtered_data_set %>%
  mutate(material_piso = factor(material_piso,
                                levels = c("Cerámico", "Madera", "Carpeta de cemento", "Sin piso/tierra"),
                                labels = c("Cerámico", "Madera", "Cemento", "Tierra"))) %>%
  ggplot() +
  aes(x = factor(material_piso, levels = c( "Tierra", "Cemento", "Madera", "Cerámico")),
      y = ..count.. / sum(..count..)) +
  geom_bar(width = 0.75,
           fill = '#7ed021',
           col = "black",
           alpha = 0.6) +
  labs(y = "Proporción de hogares",
       x = "Material del piso",
       caption = "Fuente: Fundación Rosa Luxemurgo") +
  ggtitle("Proporción de Hogares según el Material del Piso en Villas de Argentina 2022") +
  coord_flip() +
  theme_classic()


# GRÁFICO DE BARRAS PARA PAREDES
filtered_data_set %>%
  mutate(material_paredes = factor(material_paredes,
                                levels = c("Chapa", 
                                           "Trama en madera/tapial de madera", 
                                           "Adobe", 
                                           "Planchón (describir qué es)",
                                           "Mampostería (ladrillo/block"),
                                labels = c("Chapa", "Madera", "Adobe", "Planchón", "Mampostería"))) %>%
  ggplot() +
  aes(x = factor(material_paredes, levels = c("Chapa", "Madera", "Adobe", "Planchón", "Mampostería")),
      y = ..count.. / sum(..count..)) +
  geom_bar(width = 0.75,
           fill = '#7ed021',
           col = "black",
           alpha = 0.6) +
  labs(y = "Proporción de hogares(%)",
       x = "Material",
       caption = "Fuente: Fundación Rosa Luxemurgo") +
  ggtitle("Proporción de Hogares según el Material de las Paredes en Villas de Argentina 2022") +
  coord_flip() +
  theme_classic()


# GRÁFICO DE BARRAS PARA TECHOS
filtered_data_set %>%
  mutate(material_techo = factor(material_techo,
                                levels = c("Lona", "Chapa", "Caña/adobe", "Losa de viguetas"),
                                labels = c("Lona", "Chapa", "Caña/Adobe", "Losa de Viguetas"))) %>%
  ggplot() +
  aes(x = factor(material_techo, levels = c( "Lona", "Caña/Adobe", "Chapa", "Losa de Viguetas")),
      y = ..count.. / sum(..count..)) +
  geom_bar(width = 0.75,
           fill = '#7ed021',
           col = "black",
           alpha = 0.6) +
  labs(y = "Proporción de hogares",
       x = "Material del techo",
       caption = "Fuente: Fundación Rosa Luxemurgo") +
  ggtitle("Proporción de Hogares según el Material del Techo en Villas de Argentina 2022") +
  coord_flip() +
  theme_classic()


# BOXPLOT IFM

boxplot(filtered_data_set$IFM,
        main = "Boxplot del IFM en las Villas de Argentina 2022",
        ylab = "IFM",
        ylim = c(0,1),
        col = c("lightgreen"))


# IEH 

filtered_IEH <- as.vector(na.omit(filtered_data_set$IEH))

df_IEH <- data.frame(valor = filtered_IEH)

ggplot(df_IEH, aes(x = "", y = valor)) +
  geom_boxplot(width = 0.3, coef = 1.5) +
  labs(
    title = "Boxplot del IEH en las Villas de Argentina, 2022",
    y = "IEH",
    caption = "Fuente: Fundación Rosa Luxemurgo"
  ) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 8, face = "italic", hjust = 0)
  )


# GRÁFICO DE TORTA PARA PLAGAS
aux <- filtered_data_set$problemas_plagas

aux2 <- round(length(aux[aux == "No"])/length(aux),2)*100

x <- c(100-aux2,aux2)
labels <- c("Sufre de Plagas","No Sufre de Plagas")

labels_with_percent = paste(paste(labels,"%",sep="\n"),x,sep="")

plagas_pie <- pie(x,
                    labels=labels_with_percent,
                    radius=1,
                    clockwise=TRUE,
                    init.angle=0,
                    col = c("lightblue","lightyellow"),
                    main="Proporción de Hogares con Plagas\n en Barrios Populares de Argentina en 2022")

# Agregamos la fuente
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=0,
      at = -1.3,
      adj=0,
      cex=0.9)


# GRÁFICO DE BARRAS DE LAS PLAGAS
aux_mosq <- as.vector(na.omit(filtered_data_set$plagas_mosquitos))
aux_cuca <- as.vector(na.omit(filtered_data_set$plagas_cucarachas))
aux_rata <- as.vector(na.omit(filtered_data_set$plagas_roedores))

plaga_counts <- data.frame(
  plaga = c("Mosquitos", "Cucarachas", "Ratas"),
  frecuencia = c(length(aux_mosq),
                 length(aux_cuca),
                 length(aux_rata)))

plaga_counts$plaga <- factor(plaga_counts$plaga, levels = c("Cucarachas","Ratas","Mosquitos"))


ggplot(plaga_counts, aes(x = plaga, y = frecuencia, fill = plaga)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Distribución de Plagas en Barrios Populares de Argentina en 2022",
    x = "Tipo de Plaga",
    y = "Cantidad de Reportes",
    caption = "Fuente: Fundación Rosa Luxemurgo"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic", hjust = 0),
        axis.title.x = element_text(face = "bold"),  
        axis.title.y = element_text(face = "bold"),  
        axis.text.x  = element_text(face = "bold"),  
        axis.text.y  = element_text(face = "bold"),  
        axis.line = element_line(color = "black", linewidth = 0.8))+
  scale_fill_manual(values = c("darkorange", "darkred", "darkgreen"))






# DIAGRAMA DE DISPERSIÓN ENTRE EL IEH E IFM
filtered_data_set %>%
  filter(!is.na(IFM), !is.na(IEH)) %>%
  ggplot() + 
  aes(x=IFM,y=IEH) +
  geom_point() +
  labs(x="IFM", 
       y="IEH",
       caption = "Fuente: Fundación Rosa Luxemurgo") +
  ggtitle("Diagrama de dispersión entre el IFM y el IEH en Villas, Argentinas 2022") +
  theme_classic()


# BOXPLOT ENTRE LAS PLAGAS Y EL IEH
filtered_data_set %>%
  filter(!is.na(problemas_plagas), !is.na(IEH)) %>%
  ggplot() +
  aes(x=IEH,y=problemas_plagas) +
  geom_boxplot(show.legend = F, fill = "lightblue") +
  labs(x = "IEH",
       y = "¿Sufre de plagas?",
       title = "Boxplot Comparativo entre el IEH y los Problemas de Plagas en Villas, Argentina 2022",
       caption = "Fuente: Fundación Rosa Luxemurgo") +
  theme_light()


# TABLA DE CONTINGENCIA ENTRE "SUFRE DE HACINAMIENTO CRÍTICO" Y "SUFRE DE PLAGAS"
filtered_data_set %>%
  filter(!is.na(umbralHacinamiento), !is.na(problemas_plagas)) %>%
  mutate(hacinamiento_critico = ifelse(umbralHacinamiento >= 3, "Sí", "No")) %>%

  tabyl(hacinamiento_critico,problemas_plagas) %>%
  adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", 
              row_name  = "¿Sufre de hacinamiento crítico?", 
              col_name  = "¿Sufre de algún tipo de plaga?")


# CALCULOS DE MEDIDAS RESUMEN
quantiles_UH <- round(quantile(filtered_data_set$umbralHacinamiento, na.rm = TRUE),2)

quantiles_IFM <- round(quantile(filtered_data_set$IFM, na.rm = TRUE),2)

quantiles_IEH <- round(quantile(filtered_data_set$IEH, na.rm = TRUE),2)

corr_IEH_IFM <- round(cor(filtered_data_set$IFM,
                          filtered_data_set$IEH,
                          use = "complete.obs",
                          method = "pearson"),2)

aux_db <- filtered_data_set %>%
  select(problemas_plagas,IEH)

aux_db <- na.omit(aux_db)

IEH_con_plagas <- aux_db[aux_db$problemas_plagas == "Sí",]
IEH_sin_plagas <- aux_db[aux_db$problemas_plagas == "No",]

quantiles_IEH_con_plagas <- round(quantile(IEH_con_plagas$IEH),2)
quantiles_IEH_sin_plagas <- round(quantile(IEH_sin_plagas$IEH),2)



