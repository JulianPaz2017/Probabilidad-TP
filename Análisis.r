library(ggplot2)

# HISTOGRAMA HUMBRAL DE HACINAMIENTO
uh_histogram <- ggplot(filtered_data_set) +
                aes(x=umbralHacinamiento) +
                geom_histogram(fill = "lightgray", 
                               col = "black",
                               breaks= seq(0,10,1))+
                scale_x_continuous(breaks = seq(0,10,1)) +
                coord_cartesian(ylim = c(0, 600)) +
                labs(x="Umbral de Hacinamiento", 
                     y="Frecuencia Absoluta",
                     main="Distribución del Umbral de Hacinamiento en Villas de Argentina en 2022",
                     caption="Fuente: Fundación Rosa Luxemurgo")


# GRÁFICO DE BARRAS PARA PISO
floor_names = c("Tierra",
                "Cemento",
                "Madera",
                "Cerámico")

floor_mat <- filtered_data_set$material_piso
length_fm <- length(floor_mat)

heights_fm <- c(length(floor_mat[floor_mat == "Sin piso/tierra"])/length_fm,
                length(floor_mat[floor_mat == "Carpeta de cemento"])/length_fm,
                length(floor_mat[floor_mat == "Madera"])/length_fm,
                length(floor_mat[floor_mat == "Cerámico"])/length_fm)*100

# Ajustamos los márgenes para que entren todas las cosas
par(mar = c(6, 8, 4,2))

# Creamos el gráfico de barras
floors_barplot <- barplot(height=heights_fm,
                            main = "Proporción de Hogares según el Material del Piso en Villas de Argentina 2022",
                            horiz = TRUE,
                            xlim = c(0,100),
                            names.arg=floor_names,
                            las=1,
                            cex.names=0.8,
                            )

# Agregamos las etiquetas de los ejes (Lo hago acá para ajustar sus pocisiones)
mtext("Material", side = 2, line = 6, font = 2)
mtext("Proporción de Hogares (%)", side = 1, line = 2, font = 2)


# Agregamos el texto debajo del gráfico
# Agregamos la fuente
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=3,
      at = 2,
      adj=0,
      cex=0.9)




# GRÁFICO DE BARRAS PARA PAREDES
wall_names = c("Chapa",
               "Madera",
               "Adobe",
               "Planchón",
               "Mampostería")

wall_mat <- filtered_data_set$material_paredes
length_wm <- length(wall_mat)

heights_wm <- c(length(wall_mat[wall_mat == "Chapa"])/length_wm,
                length(wall_mat[wall_mat == "Trama en madera/tapial de madera"])/length_wm,
                length(wall_mat[wall_mat == "Adobe"])/length_wm,
                length(wall_mat[wall_mat == "Planchón (describir qué es)"])/length_wm,
                length(wall_mat[wall_mat == "Mampostería (ladrillo/block"])/length_wm)*100

# Ajustamos los márgenes para que entren todas las cosas
par(mar = c(6, 8, 4,2))

# Creamos el gráfico de barras
floors_barplot <- barplot(height=heights_wm,
                            main = "Proporción de Hogares según el Material de las Paredes en Villas de Argentina 2022",
                            horiz = TRUE,
                            xlim = c(0,100),
                            names.arg=wall_names,
                            las=1,
                            cex.names=0.8,
                            )

# Agregamos las etiquetas de los ejes (Lo hago acá para ajustar sus pocisiones)
mtext("Material", side = 2, line = 6, font = 2)
mtext("Proporción de Hogares (%)", side = 1, line = 2, font = 2)

# Agregamos el texto debajo del gráfico
# Agregamos la fuente
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=3,
      at = 2,
      adj=0,
      cex=0.9)





# GRÁFICO DE BARRAS PARA TECHOS
ceil_names = c("Lona",
               "Caña/Adobe",
               "Chapa",
               "Losa de Viguetas")

ceil_mat <- filtered_data_set$material_techo
length_cm <- length(ceil_mat)

heights_cm <- c(length(ceil_mat[ceil_mat == "Lona"])/length_cm,
                length(ceil_mat[ceil_mat == "Caña/adobe"])/length_cm,
                length(ceil_mat[ceil_mat == "Chapa"])/length_cm,
                length(ceil_mat[ceil_mat == "Losa de viguetas"])/length_cm)*100

# Ajustamos los márgenes para que entren todas las cosas
par(mar = c(6, 8, 4,2))

# Creamos el gráfico de barras
floors_barplot <- barplot(height=heights_cm,
                            main = "Proporción de Hogares según el Material del Techo en Villas de Argentina 2022",
                            horiz = TRUE,
                            xlim = c(0,100),
                            names.arg=ceil_names,
                            las=1,
                            cex.names=0.8
                            )

# Agregamos las etiquetas de los ejes (Lo hago acá para ajustar sus pocisiones)
mtext("Material", side = 2, line = 6, font = 2)
mtext("Proporción de Hogares (%)", side = 1, line = 2, font = 2)

# Agregamos el texto debajo del gráfico
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=3,
      at = 2,
      adj=0,
      cex=0.9)



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
main <- "Proporción de Hogares con Plagas\n en Barrios Populares de Argentina en 2022"

# c)
labels_with_percent = paste(paste(labels,"%",sep="\n"),x,sep="")

plagas_pie <- pie(x,
                    labels=labels_with_percent,
                    radius=1,
                    clockwise=TRUE,
                    init.angle=0,
                    col = c("lightblue","lightyellow"),
                    main=main)

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





# DIAGRAMA DE DISPERSIÓN ENTRE EL HACINAMIENTO E IFM
disp_diag_ifm_uh <- ggplot(filtered_data_set) + 
                    aes(x=umbralHacinamiento,y=IFM) +
                    geom_point() +
                    labs(x="Umbral Hacinamiento", y="IFM",
                    caption = "Fuente: Fundación Rosa Luxemurgo") +
                    ggtitle("Relación entre el UH y el IFM") +
                    theme_classic()


# BOXPLOT COMPARATIVO ENTRE EL HACINAMIENTO Y LAS PLAGAS
copy <- filtered_data_set
copy <- filtered_data_set %>%
  filter(!is.na(problemas_plagas))
boxplot_uh_plag <- ggplot(copy) + 
                   aes(x=problemas_plagas , y=umbralHacinamiento) +
                   geom_boxplot(show.legend=FALSE, fill = "lightblue") +
                   labs(x="Problemas Plagas", y="Umbral Hacinamiento",
                   caption = "Fuente: Fundación Rosa Luxemurgo") +
                   coord_flip() +
                   ggtitle("Boxplot Comparativo del UH Según si Tiene o no Plagas") +
                   theme_light()


# CALCULOS DE MEDIDAS RESUMEN
quantiles_UH <- round(quantile(filtered_data_set$umbralHacinamiento, na.rm = TRUE),2)

quantiles_IFM <- round(quantile(filtered_data_set$IFM, na.rm = TRUE),2)

quantiles_IEH <- round(quantile(filtered_data_set$IEH, na.rm = TRUE),2)

aux_df1 <- filtered_data_set %>%
  select(umbralHacinamiento,IFM)

aux_df1 <- na.omit(aux_df1)

corr_UH_IFM <- round(cor(aux_df1$umbralHacinamiento,aux_df1$IFM),2)

aux_df2 <- filtered_data_set %>%
  select(problemas_plagas,umbralHacinamiento)

aux_df2 <- na.omit(aux_df2)

uh_con_plagas <- aux_df2[aux_df2$problemas_plagas == "Sí",]
uh_sin_plagas <- aux_df2[aux_df2$problemas_plagas == "No",]

quantiles_uh_con_plagas <- round(quantile(uh_con_plagas$umbralHacinamiento),2)
quantiles_uh_sin_plagas <- round(quantile(uh_sin_plagas$umbralHacinamiento),2)



