# ==============================================================================
# Archivo: 02-analisis-tablas.R
# ==============================================================================

library(tidyverse)

# =============================================================================
# VARIABLE CATEGÓRICA NOMINAL
# =============================================================================

tabla_region <- datos %>%
  count(region_girai, name = "ni") %>%
  mutate(
    fi = ni / sum(ni),
    porcentaje = round(fi * 100, 2)
  )

print("--- Tabla: Países por Región ---")
print(tabla_region)

# =============================================================================
# VARIABLES CATEGÓRICAS ORDINALES
# =============================================================================

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


# =============================================================================
# VARIABLES CUANTITATIVAS CONTINUAS
# =============================================================================

tabla_marcos <- datos %>%
  mutate(intervalos = cut(marcos_norm, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  count(intervalos, name = "ni") %>%
  mutate(fi = ni / sum(ni), Ni = cumsum(ni), Fi = cumsum(fi))

tabla_acciones <- datos %>%
  mutate(intervalos = cut(acciones_gob, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  count(intervalos, name = "ni") %>%
  mutate(fi = ni / sum(ni), Ni = cumsum(ni), Fi = cumsum(fi))

tabla_actores <- datos %>% 
  mutate(intervalos = cut(actores_no_est, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>% 
  count(intervalos, name = "ni") %>%
  mutate(fi = ni / sum(ni), Ni = cumsum(ni), Fi = cumsum(fi))

print("--- Marcos Normativos ---")
print(tabla_marcos)

print("--- Acciones de Gobierno: ---")
print(tabla_acciones)

print("--- Acciones de Actores No Gubernamentales: ---")
print(tabla_actores)


# =============================================================================
#  VARIABLES CUANTITATIVAS DISCRETAS
# =============================================================================



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



# =============================================================================
# VARIABLE DE RESPUESTA MÚLTIPLE 
# =============================================================================

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

# ==============================================================================
# Categorica con Cuantitativa
# ==============================================================================

# 1. Tabla: Acciones de Gobierno por Región
tabla_resumen_acciones <- datos %>%
  group_by(region_girai) %>%
  summarise(
    n = n(),
    Media = round(mean(acciones_gob, na.rm = TRUE), 2),
    Mediana = round(median(acciones_gob, na.rm = TRUE), 2),
    Desvio = round(sd(acciones_gob, na.rm = TRUE), 2)
  )

# 2. Tabla: Actores No Estatales por Región
tabla_resumen_actores <- datos %>%
  group_by(region_girai) %>%
  summarise(
    n = n(),
    Media = round(mean(actores_no_est, na.rm = TRUE), 2),
    Mediana = round(median(actores_no_est, na.rm = TRUE), 2),
    Desvio = round(sd(actores_no_est, na.rm = TRUE), 2)
  )

# 3. Tabla: Capacidades por Región
tabla_resumen_capacidades <- datos %>%
  group_by(region_girai) %>%
  summarise(
    n = n(),
    Media = round(mean(capacidades, na.rm = TRUE), 2),
    Mediana = round(median(capacidades, na.rm = TRUE), 2),
    Desvio = round(sd(capacidades, na.rm = TRUE), 2)
  )

print(tabla_resumen_acciones)
print(tabla_resumen_actores)
print(tabla_resumen_capacidades)


# ==============================================================================
# Categoría vs. Categórica
# ==============================================================================

# 1. Tabla: Región vs. Nivel de Acciones
tabla_biv_acciones <- datos %>%
  count(region_girai, nivel_acciones) %>%
  group_by(region_girai) %>%
  mutate(
    fi = n / sum(n),
    porcentaje = round(fi * 100, 2)
  )



# 2. Tabla: Región vs. Nivel de Actores
tabla_biv_actores <- datos %>%
  count(region_girai, nivel_actores) %>%
  group_by(region_girai) %>%
  mutate(
    fi = n / sum(n),
    porcentaje = round(fi * 100, 2)
  )



print("--- Proporción de Niveles de Acciones por Región ---")
print(tabla_biv_acciones)
print("--- Proporción de Niveles de Actores por Región ---")
print(tabla_biv_actores)