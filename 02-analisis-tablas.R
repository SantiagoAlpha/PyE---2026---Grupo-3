# ==============================================================================
# Archivo: 02-analisis-tablas.R
# Objetivo: Tablas de frecuencia univariadas
# ==============================================================================

library(tidyverse)

# 1. VARIABLE CATEGÓRICA NOMINAL: region_girai
# Cantidad de países por región (ni) y porcentaje (fi)
tabla_region <- datos %>%
  count(region_girai, name = "ni") %>%
  mutate(
    fi = ni / sum(ni),
    porcentaje = round(fi * 100, 2)
  )

print("--- Tabla: Países por Región ---")
print(tabla_region)


# 2. VARIABLES CATEGÓRICAS ORDINALES: nivel_acciones y nivel_actores
# Análisis general con frecuencias acumuladas (Ni, Fi)
tabla_nivel_acciones <- datos %>%
  count(nivel_acciones, name = "ni") %>%
  mutate(
    fi = ni / sum(ni),
    Ni = cumsum(ni),
    Fi = cumsum(fi),
    porcentaje = round(fi * 100, 2)
  )

tabla_nivel_actores <- datos %>%
  count(nivel_actores, name = "ni") %>%
  mutate(
    fi = ni / sum(ni),
    Ni = cumsum(ni),
    Fi = cumsum(fi),
    porcentaje = round(fi * 100, 2)
  )

print("--- Nivel de Acciones Gubernamentales ---")
print(tabla_nivel_acciones)

print("--- Nivel de Actores No Estatales ---")
print(tabla_nivel_actores)

# 3. VARIABLES CUANTITATIVAS CONTINUAS: Análisis de Sensibilidad (Intervalos)
#Intervalos (Amplitud = 10)

tabla_marcos <- datos %>%
  mutate(intervalos = cut(marcos_norm, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  count(intervalos, name = "ni") %>%
  mutate(fi = ni / sum(ni), Ni = cumsum(ni), Fi = cumsum(fi))

tabla_acciones <- datos %>%
  mutate(intervalos = cut(acciones_gob, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  count(intervalos, name = "ni") %>%
  mutate(fi = ni / sum(ni), Ni = cumsum(ni), Fi = cumsum(fi))

print("--- Marcos Normativos ---")
print(tabla_marcos)

print("--- Acciones de Gobierno: ---")
print(tabla_acciones)


# 4. VARIABLES CUANTITATIVAS DISCRETAS (Conteos de áreas)


# Cantidad de Áreas en Acciones
tabla_cant_acciones <- datos %>%
  count(cant_areas_acciones, name = "ni") %>%
  mutate(
    fi = ni / sum(ni),
    Ni = cumsum(ni),
    Fi = cumsum(fi),
    porcentaje = round(fi * 100, 2)
  )

#Cantidad de Áreas en Actores
tabla_cant_actores <- datos %>%
  count(cant_areas_actores, name = "ni") %>%
  mutate(
    fi = ni / sum(ni),
    Ni = cumsum(ni),
    Fi = cumsum(fi),
    porcentaje = round(fi * 100, 2)
  )

print("--- Cantidad de Áreas (Acciones) ---")
print(tabla_cant_acciones)

print("--- Cantidad de Áreas (Actores) ---")
print(tabla_cant_actores)



# 5. VARIABLE DE RESPUESTA MÚLTIPLE (Indicadores p70)


# Creamos una tabla resumen
tabla_p70_multiple <- datos %>%
  summarise(
    Datos = sum(p70_datos == 1, na.rm = TRUE),
    Seguridad = sum(p70_seguridad == 1, na.rm = TRUE),
    Transparencia = sum(p70_transparencia == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Caracteristica", values_to = "ni") %>%
  mutate(
    fi = ni / nrow(datos), # Proporción de países sobre el total que cumplen
    porcentaje = round(fi * 100, 2)
  )

print("--- Indicadores p70 ---")
print(tabla_p70_multiple)
