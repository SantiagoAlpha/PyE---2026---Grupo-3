library(googlesheets4)

# Link al archivo
url="https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=1481484702#gid=1481484702"

# Evito loggeo
gs4_deauth()

# Leo el archivo y almaceno los datos en un data frame
datos <- read_sheet(url, skip = 1,sheet=2)

# Definimos el orden lógico de menor a mayor
niveles_orden <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
names(datos)[18] <- "dim_mejor_punt"


# Transformamos las columnas de texto a categorías (Factores)
datos$GIRAI_region <- as.factor(datos$GIRAI_region)
datos$NU_region <- as.factor(datos$NU_region)
datos$UN_subregion <- as.factor(datos$UN_subregion)
datos$sec_mng <- factor(datos$sec_mng, levels = niveles_orden, ordered = TRUE)
datos$sec_ag  <- factor(datos$sec_ag,  levels = niveles_orden, ordered = TRUE)
datos$sec_ane <- factor(datos$sec_ane, levels = niveles_orden, ordered = TRUE)
datos$GIRAI_region   <- as.factor(datos$GIRAI_region)
datos$NU_region      <- as.factor(datos$NU_region)
datos$UN_subregion   <- as.factor(datos$UN_subregion)
datos$academia       <- as.factor(datos$academia)
datos$privado        <- as.factor(datos$privado)
datos$dim_mejor_punt <- as.factor(datos$dim_mejor_punt)
datos$tipo_academia_es <- as.factor(datos$tipo_academia_es)
datos$tipo_privado_es  <- as.factor(datos$tipo_privado_es)
# Ahora probá correr el summary de nuevo
summary(datos)

str(datos)