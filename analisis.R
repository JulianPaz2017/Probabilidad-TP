library(ggplot2)

# Histograma umbral de hacinamiento

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


# Histograma IFM

# Establecemos los intervalos
breakpoints <- seq(0,1, by =0.1)

filtered_IFM <- as.vector(na.omit(filtered_data_set$IFM))

# Pasamos el dataset values, dividimos los datos en los intervalos dados por breakpoints, eliminamos los 
# ejes X e Y con xaxt e yaxt, y asignamos freq=TRUE para que las alturas de las barras estén bien
uh_histogram <- hist(x = filtered_IFM,
                     breaks = breakpoints,
                     ylim = c(0,500),
                     xlab = "IFM",
                     ylab = "Frecuencia Absoluta",
                     main = "Distribución del IFM en Villas de Argentina en 2022",
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
     at = seq(0, 1, by = 0.1),
     labels = seq(0, 1, by = 0.1),
     cex.axis = 0.8)

# Creamos el eje Y manualmente
axis(side = 2,
     font = 2,
     at = seq(0, 500, by = 50),
     labels = seq(0, 500, by = 50),
     cex.axis = 0.8)

# Agregamos la fuente
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=3,
      at = -1.2,
      adj=0,
      cex=0.9)


boxplot(filtered_data_set$IFM,
        main = "Boxplot del IFM en las Villas de Argentina 2022",
        ylab = "IFM",
        ylim = c(0,1),
        col = c("lightgreen"))


# IEH 

filtered_IEH <- as.vector(na.omit(filtered_data_set$IEH))

df_IEH <- data.frame(valor = filtered_IEH)

ggplot(df, aes(x = "", y = valor)) +
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


# Agregamos la fuente
mtext("Fuente: Fundación Rosa Luxemurgo",
      side=1,
      font = 2,
      line=3,
      at = 0,
      adj=0,
      cex=0.9)



# Plagas
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

aux_mosq <- as.vector(na.omit(filtered_data_set$plagas_mosquitos))
aux_cuca <- as.vector(na.omit(filtered_data_set$plagas_cucarachas))
aux_rata <- as.vector(na.omit(filtered_data_set$plagas_roedores))

plaga_counts <- data.frame(
  plaga = c("Mosquitos", "Cucarachas", "Ratas"),
  frecuencia = c(length(aux_mosq),
                 length(aux_cuca),
                 length(aux_rata))
)

plaga_counts$plaga <- factor(plaga_counts$plaga, levels = c("Cucarachas","Ratas","Mosquitos"))

# Graficar
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


