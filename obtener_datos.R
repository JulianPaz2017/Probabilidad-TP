install.packages("dplyr")
install.packages("readxl")


# Cargamos la librería
library(dplyr)
library(readxl)

# Fijamos la ruta del archivo de datos
setwd("D:\\Facu\\LCC\\tpPYE")

# Cargo el archivo de datos .xlsx
data_set <- readxl::read_excel("Datos_LP.xlsx", 
                               col_names = FALSE, 
                               skip = 3)

# Veo la estructura del dataset
head(data_set)

# Fijo el dataset
attach(data_set)

# Renombramos las columnas para trabajar con más comodidad
colnames(data_set) <- 1:118

# Filtramos las columnas que queremos:
filtered_data_set <- data_set %>%
  select(   # Seleccionar las columnas que quiero conservar
    "5",                            # Tiempo de residencia
    "14",                           # Máximo de personas
    "19",                           # Lugar que habitan
    "33",                           # Desague
    "62",                           # Material del piso
    "63",                           # Material del techo
    "69",                           # Material de las paredes exteriores
    "92",                           # Problemas de plagas
    "93","94","95",                 # Columnas Plagas
    "13",                           # Cantidad de dormitorios
    "6"                             # Cantidad de habitantes en vivienda
  )


colnames(filtered_data_set) <- c(
  "tiempo_residencia",
  "max_personas",
  "lugar_habitado",
  "tipo_desague",
  "material_piso",
  "material_techo",
  "material_paredes",
  "problemas_plagas",
  "plagas_cucarachas",
  "plagas_roedores",
  "plagas_otros",
  "num_dormitorios",
  "max_personas"
  
)

head(filtered_data_set)

write.csv(filtered_data_set, "filtered_data_set.csv", row.names = FALSE)

