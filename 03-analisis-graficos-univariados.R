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


# Histograma: Actores No Estatales
grafico_actores_hist <- ggplot(datos, aes(x = actores_no_est)) +
  geom_histogram(bins = 10, fill = "gray90", color = "black", boundary = 0) +
  theme_minimal() +
  labs(title = "Distribución: Actores No Estatales", 
       x = "Puntaje (0-100)", 
       y = "Frecuencia")



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
grafico_cant_actores <-ggplot(datos, aes(x = cant_areas_actores)) +
  geom_bar(fill = "steelblue", alpha = 0.5, color = "darkblue", bins = 30) +
  scale_x_continuous(breaks = seq(0, 19, by = 2)) +
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




# Gráfico: Comparación de Cantidad de Áreas con Actores No Estatales
#           y Cantidad de Áreas con Acciones Gubernamentales
#Preparamos los datos en formato largo
datos_comparativos <- datos %>%
  select(cant_areas_acciones, cant_areas_actores) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Sector",
    values_to = "Cantidad_Areas"
  ) %>%
  mutate(Sector = factor(recode(Sector, 
                                "cant_areas_acciones" = "Acciones Gubernamentales",
                                "cant_areas_actores" = "Actores No Estatales"),
                         levels = c("Actores No Estatales", "Acciones Gubernamentales")))

#  Gráfico de barras apiladas (encimadas)
comparación_cantidad_acciones_actores <- ggplot(datos_comparativos, aes(x = Cantidad_Areas, fill = Sector)) +
  # 'position = stack' las pone una sobre la otra
  # El borde negro ayuda a distinguir donde termina cada barra
  geom_bar(position = "stack", color = "black", alpha = 0.8) +
  
  # Definimos colores Rojo (firebrick) y Azul (steelblue)
  scale_fill_manual(values = c("Acciones Gubernamentales" = "steelblue", 
                               "Actores No Estatales" = "firebrick")) +
  
  # Mantenemos la escala de 0 a 19
  scale_x_continuous(breaks = seq(0, 19, by = 1)) + 
  
  labs(
    title = "Acción Gubernamental vs. Actores No Estatales",
    subtitle = "Frecuencia acumulada de países según la cantidad de áreas cubiertas",
    x = "Cantidad de Áreas Temáticas",
    y = "N° de países (Acumulado)",
    fill = "Sector"
  ) +
  
  # Mantenemos las fuentes grandes de los gráficos anteriores
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank()
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

#Prioridad

print(grafico_nivel_acciones)
print(grafico_nivel_actores)
print(grafico_cant_acciones)
print(grafico_cant_actores)
print(comparación_cantidad_acciones_actores)
#---------------------------------------------------

print(grafico_nominal)
print(grafico_marcos_hist)
print(grafico_acciones_hist)
print(grafico_actores_hist)



print(grafico_p70)












