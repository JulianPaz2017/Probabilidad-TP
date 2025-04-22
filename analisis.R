mean(filtered_data_set$umbralHacinamiento, na.rm = TRUE) # Promedio de umbral de hacinamiento

nrow(filtered_data_set %>% filter(umbralHacinamiento >= 3)) / nrow(filtered_data_set)

mean(filtered_data_set$IFM, na.rm = TRUE) # Promedio de umbral de hacinamiento

nrow(filtered_data_set %>% filter(IFM >= 0.65)) / nrow(filtered_data_set)

mean(filtered_data_set$IEH, na.rm = TRUE) # Promedio de umbral de hacinamiento

nrow(filtered_data_set %>% filter(IEH >= 0.65)) / nrow(filtered_data_set)
  
library(ggplot2)

ggplot(filtered_data_set, aes(x = IEH)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0.65, color = "red", linetype = "dashed", size = 1) + 
  labs(
    title = "Distribución del IEH",
    x = "Indice de Estabilidad Habitacional (puntos)",
    y = "Frecuencia"
  ) +
  theme_minimal()

nrow(filtered_data_set %>% filter(problemas_plagas == "Sí")) / nrow(filtered_data_set) * 100

library(dplyr)
library(ggplot2)

# Contar cada tipo de plaga
plaga_counts <- data.frame(
  Plaga = c("Cucarachas", "Ratas", "Mosquitos"),
  Cantidad = c(
    sum(filtered_data_set$plagas_cucarachas == "Cucarachas", na.rm = TRUE),
    sum(filtered_data_set$plagas_roedores == "Ratas", na.rm = TRUE),
    sum(filtered_data_set$plagas_otros != "" & !is.na(filtered_data_set$plagas_otros))
  )
)

# Graficar
ggplot(plaga_counts, aes(x = Plaga, y = Cantidad, fill = Plaga)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Cantidad de Hogares con Presencia de Plagas",
    x = "Tipo de Plaga",
    y = "Cantidad de Reportes"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("darkorange", "darkred", "darkgreen"))
