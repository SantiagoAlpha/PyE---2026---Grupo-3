# ==============================================================================
# Archivo: 03-analisis-graficos-univariados.R
# Objetivo: Visualización univariada (Estética en escala de grises/neutra)
# ==============================================================================

library(tidyverse)

# 1. VARIABLE CATEGÓRICA NOMINAL: region_girai
# Gráfico de barras simple (sin colores por categoría)
grafico_nominal <- ggplot(datos, aes(x = fct_infreq(region_girai))) +
  geom_bar(fill = "gray80", color = "black") + # Relleno neutro y borde definido
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Distribución Global de Países por Región",
    subtitle = "Frecuencia absoluta de países participantes en el GIRAI 2024",
    x = "Región",
    y = "Cantidad de Países"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_nominal)


# 2. VARIABLES CUANTITATIVAS CONTINUAS: Marcos Normativos y Acciones de Gob.
# Histogramas neutros con 10 intervalos

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

print(grafico_marcos_hist)

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

print(grafico_acciones_hist)