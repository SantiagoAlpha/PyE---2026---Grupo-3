# ==============================================================================
# Archivo: 03-analisis-graficos-univariados.R
# ==============================================================================

# ==============================================================================
#  VARIABLES CATEGÓRICA NOMINAL
# ==============================================================
# Gráfico de barras: region_girai

grafico_nominal <- ggplot(datos, aes(x = fct_infreq(region_girai))) +
  geom_bar(fill = "gray80", color = "black") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Distribución Global de Países por Región",
    subtitle = "Frecuencia absoluta de países participantes en el GIRAI 2024",
    x = "Región",
    y = "Cantidad de Países"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ==============================================================================
#  VARIABLES CUANTITATIVAS CONTINUAS
# ===========================================================================

# Histograma: Marcos Normativos
grafico_marcos_hist <- ggplot(datos, aes(x = marcos_norm)) +
  geom_histogram(bins = 10, fill = "gray90", color = "black", boundary = 0) +
  theme_minimal() +
  labs(
    title = "Distribución de Puntajes: Marcos Normativos",
    subtitle = "Análisis de 10 intervalos de clase (Amplitud = 10)",
    x = "Puntaje (0 - 100)",
    y = "Frecuencia (n° de países)"
  )



# Histograma: Acciones de Gobierno
grafico_acciones_hist <- ggplot(datos, aes(x = acciones_gob)) +
  geom_histogram(bins = 10, fill = "gray90", color = "black", boundary = 0) +
  theme_minimal() +
  labs(
    title = "Distribución de Puntajes: Acciones de Gobierno",
    subtitle = "Análisis de 10 intervalos de clase (Amplitud = 10)",
    x = "Puntaje (0 - 100)",
    y = "Frecuencia (n° de países)"
  )



# ==============================================================================
#  VARIABLES CUANTITATIVAS DISCRETAS
# ==============================================================================

# Gráfico: Cantidad de Áreas con Acciones Gubernamentales
grafico_cant_acciones <- ggplot(datos, aes(x = cant_areas_acciones)) +
  geom_bar(fill = "gray85", color = "black") + # Estética neutra
  scale_x_continuous(breaks = seq(0, 19, by = 2)) + # Forzamos escala de 0 a 19
  theme_minimal() +
  labs(
    title = "Cobertura Temática: Acciones de Gobierno",
    subtitle = "Distribución de países según cantidad de áreas con acciones (0-19)",
    x = "Cantidad de Áreas Temáticas",
    y = "Frecuencia (n° de países)"
  )




# Gráfico: Cantidad de Áreas con Actores No Estatales
grafico_cant_actores <- ggplot(datos, aes(x = cant_areas_actores)) +
  geom_bar(fill = "gray85", color = "black") +
  scale_x_continuous(breaks = seq(0, 19, by = 2)) +
  theme_minimal() +
  labs(
    title = "Cobertura Temática: Actores No Estatales",
    subtitle = "Distribución de países según cantidad de áreas con presencia de actores (0-19)",
    x = "Cantidad de Áreas Temáticas",
    y = "Frecuencia (n° de países)"
  )

# ==============================================================================
#  VARIABLES CATEGÓRICAS ORDINALES
# ==============================================================================

# Gráfico: Nivel de Acciones Gubernamentales
# El orden (Muy bajo -> Muy alto) se respeta por la configuración del factor en el script 01
grafico_nivel_acciones <- ggplot(datos, aes(x = nivel_acciones)) +
  geom_bar(fill = "gray85", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Nivel de Acciones Gubernamentales",
    subtitle = "Distribución de países según su nivel de desarrollo en acciones de gobierno",
    x = "Nivel de Desarrollo",
    y = "Cantidad de Países"
  )




# Gráfico: Nivel de Actores No Estatales
grafico_nivel_actores <- ggplot(datos, aes(x = nivel_actores)) +
  geom_bar(fill = "gray85", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Nivel de Actores No Estatales",
    subtitle = "Distribución de países según la participación de actores no estatales",
    x = "Nivel de Desarrollo",
    y = "Cantidad de Países"
  )




# ==============================================================================
#  VARIABLE DE RESPUESTA MÚLTIPLE
# ==============================================================================

# Usamos tabla_p70_multiple
grafico_p70 <- ggplot(tabla_p70_multiple, aes(x = reorder(Caracteristica, -porcentaje), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "gray85", color = "black") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Incidencia de Estándares de IA Responsable",
    subtitle = "Porcentaje de países que superan el 70% de puntaje por área temática",
    x = "Área Temática (Indicadores p70)",
    y = "Porcentaje de países (%)"
  )



# ==============================================================================
#  Print de Graficos
# ==============================================================
print(grafico_nominal)
print(grafico_marcos_hist)
print(grafico_acciones_hist)
print(grafico_cant_acciones)
print(grafico_cant_actores)
print(grafico_nivel_acciones)
print(grafico_nivel_actores)
print(grafico_p70)