# Instalamos el paquete en caso de no tenerlo instalado
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
		"24",                           # Forma de obtención del agua
		"33",                           # Desague
		"43","44","45","46","47","48",  # Columnas fuentes de energía para cocinar
		"50",                           # Tipo conexión eléctrica
		"56",                           # Conexión a internet
		"62",                           # Material del piso
		"63",                           # Material del techo
		"69",                           # Material de las paredes exteriores
		"73","74","75","76","77","78",  # Problemas de humedad
		"92",                           # Problemas de plagas
		"93","94","95"                  # Columnas Plagas
	)


colnames(filtered_data_set) <- c(
  "tiempo_residencia",
  "max_personas",
  "lugar_habitado",
  "forma_obtencion_agua",
  "tipo_desague",
  "energia_garrafa",
  "energia_electrica",
  "energia_lena",
  "energia_carbon",
  "energia_gas_red",
  "energia_otro",
  "tipo_conexion_electrica",
  "conexion_internet",
  "material_piso",
  "material_techo",
  "material_paredes",
  "humedad_goteras",
  "humedad_paredes_pintura",
  "humedad_piso",
  "humedad_techos",
  "humedad_filtraciones",
  "humedad_otro",
  "problemas_plagas",
  "plagas_cucarachas",
  "plagas_roedores",
  "plagas_otros"
)

head(filtered_data_set)
  
