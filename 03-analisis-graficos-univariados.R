# ==============================================================================
# Archivo: 03-analisis-graficos-univariados.R
# ==============================================================================

# ==============================================================================
#  VARIABLES CATEGÓRICA NOMINAL
# ==============================================================


# Gráfico de barras: region_girai
grafico_nominal <- ggplot(datos, aes(x = fct_infreq(region_girai))) +
  geom_bar(fill = "steelblue", alpha = 0.5, color = "darkblue") + 
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  
  theme_minimal(base_size = 16) +
  labs(
    title = "Distribución Global de Países por Región",
    subtitle = "Frecuencia absoluta de países participantes en el GIRAI 2024",
    x = "Región",
    y = "Cantidad de Países"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14)
  )




# ==============================================================================
#  VARIABLES CUANTITATIVAS CONTINUAS
# ===========================================================================

grafico_cap_hist <- ggplot(datos, aes(x = capacidades)) +
  geom_histogram(bins = 10, fill = "steelblue", alpha = 0.5, color = "darkblue", boundary = 0) +
  
  theme_minimal(base_size = 16) +
  labs(
    title = "Distribución Global de Capacidades",
    x = "Puntaje de Capacidad (0 - 100)",
    y = "Frecuencia (n° de países)"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )


# ==============================================================================
#  VARIABLES CUANTITATIVAS DISCRETAS
# ==============================================================================


# Gráfico: Cantidad de Áreas con Acciones Gubernamentales
grafico_cant_acciones <-ggplot(datos, aes(x = cant_areas_acciones)) +
  geom_bar(fill = "steelblue", alpha = 0.5, color = "darkblue", bins = 30) + # Estética neutra
  scale_x_continuous(breaks = seq(0, 19, by = 2)) + # Forzamos escala de 0 a 19
  labs(
    title = "Distribución de países según cantidad de áreas con acciones gubernamentales (0-19)",
    x = "Cantidad de Áreas Temáticas",
    y = "Frecuencia (n° de países)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )




# Gráfico: Cantidad de Áreas con Actores No Estatales
grafico_cant_actores <- ggplot(datos, aes(x = cant_areas_actores)) +
  geom_bar(fill = "steelblue", alpha = 0.5, color = "darkblue") +
  scale_x_continuous(breaks = seq(0, 19, by = 2)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) + 
  labs(
    title = "Distribución de países según cantidad de áreas con presencia de actores no estatales (0-19)",
    x = "Cantidad de Áreas Temáticas",
    y = "Frecuencia (n° de países)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )



# ==============================================================================
#  VARIABLES CATEGÓRICAS ORDINALES
# ==============================================================================

grafico_nivel_acciones<-ggplot(datos, aes(x = nivel_acciones)) +
  geom_bar(fill = "steelblue", alpha = 0.5, color = "darkblue", bins = 30) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(
    title = "Nivel de Acciones Gubernamentales",
    subtitle = "Distribución de países según su nivel de desarrollo en acciones de gobierno",
    x = "Nivel de Desarrollo",
    y = "Cantidad de Países"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


# Gráfico: Nivel de Actores No Estatales
grafico_nivel_actores <- ggplot(datos, aes(x = nivel_actores)) +
  geom_bar(fill = "steelblue", alpha = 0.5, color = "darkblue", bins = 30) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(
    title = "Nivel de Actores No Estatales",
    subtitle = "Distribución de países según la participación de actores no estatales",
    x = "Nivel de Desarrollo",
    y = "Cantidad de Países"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


# ==============================================================================
#  VARIABLE DE RESPUESTA MÚLTIPLE
# ==============================================================================

grafico_top20_p70 <- ggplot(tabla_top20_p70, aes(x = reorder(Variable, porcentaje), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "darkblue", alpha = 0.5) +
  geom_text(aes(label = paste0(porcentaje, "%")), hjust = -0.2, size = 5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Top 20 GIRAI: Cumplimiento de Estándares Éticos",
    subtitle = "Porcentaje de cumplimiento (p70) en los 20 países con mayor índice",
    x = "Área Temática",
    y = "Porcentaje de Países que cumplen (%)"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )


# ==============================================================================
#  Print de Graficos
# ==============================================================

print(grafico_nominal)
print(grafico_cap_hist)
print(grafico_nivel_acciones)
print(grafico_nivel_actores)
print(grafico_cant_acciones)
print(grafico_cant_actores)
print(grafico_top20_p70)











