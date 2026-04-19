# ==============================================================================
# 05-ANALISIS-NUMERICO.R: Medidas de Dispersión y Variabilidad
# ==============================================================================

# 0. Estructura del conjunto de datos
str(datos)

# 1. Medidas para Acciones Gubernamentales
# ------------------------------------------------------------------------------
resumen_dispersion_acciones <- datos %>%
  summarise(
    Minimo = min(cant_areas_acciones, na.rm = TRUE),
    Maximo = max(cant_areas_acciones, na.rm = TRUE),
    Rango = Maximo - Minimo,
    Desvio_Estandar = sd(cant_areas_acciones, na.rm = TRUE),
    Varianza = var(cant_areas_acciones, na.rm = TRUE),
    Coef_Variacion = (Desvio_Estandar / mean(cant_areas_acciones, na.rm = TRUE)) * 100
  )

# 2. Medidas para Actores No Estatales 
# ------------------------------------------------------------------------------
resumen_dispersion_actores <- datos %>%
  summarise(
    Minimo = min(cant_areas_actores, na.rm = TRUE),
    Maximo = max(cant_areas_actores, na.rm = TRUE),
    Rango = Maximo - Minimo,
    Desvio_Estandar = sd(cant_areas_actores, na.rm = TRUE),
    Varianza = var(cant_areas_actores, na.rm = TRUE),
    Coef_Variacion = (Desvio_Estandar / mean(cant_areas_actores, na.rm = TRUE)) * 100
  )

# 3. Análisis de dispersión 
# ------------------------------------------------------------------------------
print("--- Análisis de Dispersión: Acciones de Gobierno ---")
print(resumen_dispersion_acciones)

print("--- Análisis de Dispersión: Actores No Estatales ---")
print(resumen_dispersion_actores)


r_ag_cap <- cor(datos$acciones_gob, datos$capacidades, use = "complete.obs")
r_ane_cap <- cor(datos$actores_no_est, datos$capacidades, use = "complete.obs")

cat("Correlación ag vs cap:", r_ag_cap, "\n")
cat("Correlación ane vs cap:", r_ane_cap, "\n")


# 4. Análisis de dispersión de capaciadad y summary de capacidad 
# ------------------------------------------------------------------------------
resumen_cap <- datos %>% 
  summarise(
    Media = mean(capacidades, na.rm = TRUE),
    Mediana = median(capacidades, na.rm = TRUE),
    Desv_Std = sd(capacidades, na.rm = TRUE),
    Min = min(capacidades, na.rm = TRUE),
    Max = max(capacidades, na.rm = TRUE)
  )
print(resumen_cap)

sd(datos$capacidades, na.rm = TRUE)

