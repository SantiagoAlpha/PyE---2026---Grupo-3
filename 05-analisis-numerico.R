
# Estructura del conjunto de datos
str(datos)
# Cálculo simple del coeficiente (r)
# El argumento use = "complete.obs" es fundamental para omitir filas con datos faltantes (NA)
r_ag_cap <- cor(datos$acciones_gob, datos$capacidades, use = "complete.obs")
r_ane_cap <- cor(datos$actores_no_est, datos$capacidades, use = "complete.obs")

# Imprimir resultados en consola
cat("Correlación ag vs cap:", r_ag_cap, "\n")
cat("Correlación ane vs cap:", r_ane_cap, "\n")

# Si necesitas el p-valor para el informe de Probabilidad y Estadística:
test_ag <- cor.test(datos$acciones_gob, datos$capacidades)
print(test_ag)





# ==============================================================================
# 05-ANALISIS-NUMERICO.R: Medidas de Dispersión y Variabilidad
# ==============================================================================

# 1. Medidas para Acciones Gubernamentales (cant_areas_acciones)
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

# 2. Medidas para Actores No Estatales (cant_areas_actores)
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

# Imprimir resultados para el informe
print("--- Análisis de Dispersión: Acciones de Gobierno ---")
print(resumen_dispersion_acciones)

print("--- Análisis de Dispersión: Actores No Estatales ---")
print(resumen_dispersion_actores)



