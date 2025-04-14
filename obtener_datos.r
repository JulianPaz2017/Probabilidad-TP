# Instalamos el paquete en caso de no tenerlo instalado
# install.packages("dplyr")

# Cargamos la librería
library(dplyr)

# Fijamos la ruta del archivo de datos
setwd("/home/julian/Facultad/PyE/TP - Primera Parte/")

# Cargo el archivo de datos .xlsx
data_set <- readxl::read_excel("Datos_LP.xlsx", 
														   col_names = FALSE, 
														   skip = 3)

# Veo la estructura del dataset
str(data_set)

# Fijo el dataset
attach(data_set)

# Renombramos las columnas para trabajar con más comodidad
colnames(data_set) <- 1:118

# Filtramos las columnas que queremos:
filtered_date_set <- data_set %>%
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

