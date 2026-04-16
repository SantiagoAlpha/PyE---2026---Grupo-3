# ==============================================================================
# Archivo: 04-analisis-graficos-bivariados.R
# ==============================================================================

# ==============================================================================
# Categorica Nominal con Cuantitativa Continua
# ==============================================================================

# 1. Acciones Gubernamentales por Región
grafico_box_acciones <- ggplot(datos, aes(x = region_girai, y = acciones_gob)) +
  geom_boxplot(fill = "gray95", color = "black") + # Sin colores, solo gris muy claro
  theme_minimal() +
  labs(
    title = "Acciones Gubernamentales según Región",
    x = "Región",
    y = "Puntaje Acciones (0-100)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Actores No Estatales por Región
grafico_box_actores <- ggplot(datos, aes(x = region_girai, y = actores_no_est)) +
  geom_boxplot(fill = "gray95", color = "black") +
  theme_minimal() +
  labs(
    title = "Participación de Actores No Estatales por Región",
    x = "Región",
    y = "Puntaje Actores (0-100)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Capacidades por Región
grafico_box_capacidades <- ggplot(datos, aes(x = region_girai, y = capacidades)) +
  geom_boxplot(fill = "gray95", color = "black") +
  theme_minimal() +
  labs(
    title = "Capacidades de IA por Región",
    x = "Región",
    y = "Puntaje Capacidades (0-100)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# GRAFICOS BIVARIADOS: Categoría vs. Categórica
# ==============================================================================

# 1. Gráfico: Relación Región y Nivel de Acciones
grafico_biv_acciones <- ggplot(datos, aes(x = region_girai, fill = nivel_acciones)) +
  geom_bar(position = "fill", color = "black") + # 'fill' estandariza al 100%
  scale_fill_grey(start = 0.9, end = 0.2) +      # Escala de grises: claro a oscuro
  theme_minimal() +
  labs(
    title = "Distribución del Nivel de Acciones por Región",
    subtitle = "Análisis proporcional del desarrollo gubernamental en IA",
    x = "Región",
    y = "Proporción (0 a 1)",
    fill = "Nivel de Acciones"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# 2. Gráfico: Relación Región y Nivel de Actores
grafico_biv_actores <- ggplot(datos, aes(x = region_girai, fill = nivel_actores)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_grey(start = 0.9, end = 0.2) +
  theme_minimal() +
  labs(
    title = "Distribución del Nivel de Actores por Región",
    subtitle = "Análisis proporcional de la participación no estatal",
    x = "Región",
    y = "Proporción (0 a 1)",
    fill = "Nivel de Actores"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








print(grafico_box_acciones)
print(grafico_box_actores)
print(grafico_box_capacidades)
print(grafico_biv_acciones)
print(grafico_biv_actores)