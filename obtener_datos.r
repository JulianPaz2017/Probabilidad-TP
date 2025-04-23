# Instalamos el paquete en caso de no tenerlo instalado
#install.packages("dplyr")
#install.packages("readxl")

# Cargamos la librería
library(dplyr)
library(readxl)

# Definimos la ruta donde estamos trabajando
path <- "/home/julian/Facultad/PyE/TP - Primera Parte/Archivos R/"

# Fijamos la ruta del archivo de datos
setwd(path)

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
		"6",                            # Cantidad de integrantes del hogar
    "13",                           # Cantidad de dormitorios del hogar
		"19",                           # Vínculo con la vivienda
		"62",                           # Material del piso
		"63",                           # Material del techo
		"69",                           # Material de las paredes exteriores
		"92",                           # Problemas de plagas
		"93","94","95"                  # Columnas Plagas
	)


colnames(filtered_data_set) <- c(
  "tiempo_residencia",
  "cant_integrantes_del_hogar",
  "cant_dormitorios",
  "vinculo_vivienda",
  "material_piso",
  "material_techo",
  "material_paredes",
  "problemas_plagas",
  "plagas_cucarachas",
  "plagas_mosquitos",
  "plagas_roedores"
)

  


