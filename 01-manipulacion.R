# ==============================================================================
# Archivo: 01-manipulacion.R
# Objetivo: Limpieza, renombrado y corrección de tipos de datos
# ==============================================================================

library(tidyverse)

# 1. Selección y Renombrado
datos_limpios <- datos %>%
  select(
    ranking = 1, pais = 4, region_girai = 5, region_onu = 6,
    indice_girai = 8, marcos_norm = 9, acciones_gob = 10, actores_no_est = 11,
    ddhh = 12, gobernanza = 13, capacidades = 14,
    nivel_marcos = 15, nivel_acciones = 16, nivel_actores = 17,
    dim_mejor_punt = 18, p70_sesgo = 19, p70_infancia = 20, p70_diversidad = 21,
    p70_datos = 22, p70_genero = 23, p70_supervision = 24, p70_laboral = 25,
    p70_seguridad = 26, p70_transparencia = 27,
    cant_areas_marcos = 28, cant_areas_acciones = 29, cant_areas_parlam = 30,
    cant_areas_concien = 31, cant_areas_actores = 32,
    academia_presencia = 33, tipo_academia = 35, 
    privado_presencia = 36, tipo_privado = 38
  )

# 2. Conversión de Categorías y Arreglo de NAs
niveles_logicos <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

datos_final <- datos_limpios %>%
  mutate(
    # Convertimos a character antes que a factor para "despertar" a las columnas con NAs
    tipo_academia = as.factor(as.character(tipo_academia)),
    tipo_privado  = as.factor(as.character(tipo_privado)),
    
    # Resto de Nominales
    region_girai = as.factor(region_girai),
    region_onu = as.factor(region_onu),
    dim_mejor_punt = as.factor(dim_mejor_punt),
    academia_presencia = as.factor(academia_presencia),
    privado_presencia = as.factor(privado_presencia),
    
    # Ordinales
    nivel_marcos   = factor(nivel_marcos, levels = niveles_logicos, ordered = TRUE),
    nivel_acciones = factor(nivel_acciones, levels = niveles_logicos, ordered = TRUE),
    nivel_actores  = factor(nivel_actores, levels = niveles_logicos, ordered = TRUE)
  )

# 3. Verificación
# Con esto vemos si hay niveles en las variables que antes daban error
summary(datos_final)

datos <- datos_final
