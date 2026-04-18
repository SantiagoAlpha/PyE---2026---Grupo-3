# ==============================================================================
# Archivo: 04-analisis-graficos-bivariados.R
# ==============================================================================

# ==============================================================================
# Categorica Nominal con Cuantitativa Continua
# =============================================================================

#  Acciones Gubernamentales por Región
grafico_box_acciones <- ggplot(datos, aes(x = region_girai, y = acciones_gob)) +
  geom_boxplot(fill = "gray95", color = "black") + # Sin colores, solo gris muy claro
  theme_minimal() +
  labs(
    title = "Acciones Gubernamentales según Región",
    x = "Región",
    y = "Puntaje Acciones (0-100)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Actores No Estatales por Región
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




# BOXPLOT: CAPACIDAD VS. INICIATIVAS ACADÉMICAS

# Preparación de datos
datos_box_acad <- datos %>%
  mutate(tipo_academia = as.character(tipo_academia)) %>%
  mutate(tipo_academia = gsub("éticas, normas", "éticas y normas", tipo_academia)) %>%
  mutate(tipo_academia = replace_na(tipo_academia, "Sin iniciativas académicas")) %>%
  separate_rows(tipo_academia, sep = ",\\s*") %>%
  filter(!is.na(capacidades))

datos_acad_cap <- ggplot(datos_box_acad, aes(x = reorder(tipo_academia, capacidades, FUN = median, na.rm = TRUE), y = capacidades)) +
  # Boxplot con colores azules
  geom_boxplot(fill = "steelblue", color = "darkblue", alpha = 0.7, outlier.shape = 16) +
  coord_flip() + 
  labs(
    title = "Capacidad según Iniciativas Académicas",
    subtitle = "Distribución del puntaje de capacidad por tipo de actividad",
    x = "Tipo de Iniciativa",
    y = "Nivel de Capacidad (cap)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )


#  BOXPLOT: CAPACIDAD VS. INICIATIVAS PRIVADAS

# Preparación de datos
datos_box_priv <- datos %>%
  mutate(tipo_privado = as.character(tipo_privado)) %>%
  mutate(tipo_privado = gsub("éticas, normas", "éticas y normas", tipo_privado)) %>%
  mutate(tipo_privado = replace_na(tipo_privado, "Sin iniciativas privadas")) %>%
  separate_rows(tipo_privado, sep = ",\\s*") %>%
  filter(!is.na(capacidades))

datos_priv_cap <- ggplot(datos_box_priv, aes(x = reorder(tipo_privado, capacidades, FUN = median, na.rm = TRUE), y = capacidades)) +
  # Boxplot con colores azules
  geom_boxplot(fill = "steelblue", color = "darkblue", alpha = 0.7, outlier.shape = 16) +
  coord_flip() + 
  labs(
    title = "Capacidad según Iniciativas Privadas",
    subtitle = "Distribución del puntaje de capacidad por tipo de actividad",
    x = "Tipo de Iniciativa",
    y = "Nivel de Capacidad (cap)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )












# ==============================================================================
# GRAFICOS BIVARIADOS: Categoría vs. Categórica
# ==============================================================================

grafico_biv_acciones_mult <- ggplot(datos, aes(x = region_girai, fill = nivel_acciones)) +
  # 'dodge' pone las barras una al lado de la otra
  geom_bar(position = "dodge", color = "darkblue", alpha = 0.9) + 
  
  # Esta es la clave: Paleta de azules secuencial
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Comparativa de Cantidad de Países por Nivel de Acción",
    subtitle = "Distribución regional de acciones gubernamentales",
    x = "Región",
    y = "Cantidad de Países",
    fill = "Nivel de Acciones"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14)
  )

 grafico_biv_actores_mult <- ggplot(datos, aes(x = region_girai, fill = nivel_actores)) +
   # 'dodge' pone las barras una al lado de la otra
   geom_bar(position = "dodge", color = "darkblue", alpha = 0.9) + 
   
   # Esta es la clave: Paleta de azules secuencial
   scale_fill_brewer(palette = "Blues") +
   labs(
     title = "Comparativa de Cantidad de Países por Nivel de Actores",
     subtitle = "Distribución regional de acciones no estatales",
     x = "Región",
     y = "Cantidad de Países",
     fill = "Nivel de Actores"
   ) +
   theme_minimal(base_size = 16) +
   theme(
     plot.title = element_text(size = 22, face = "bold"),
     axis.title.x = element_text(size = 18),
     axis.title.y = element_text(size = 18),
     axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
     axis.text.y = element_text(size = 14)
   )
 
 

# ==============================================================================
# Cuantitativa Continua con Cuantitativa Continua
# ==============================================================================


# Capacidades versus actores no estatales
CAPyANE <- ggplot(filter(datos,), aes(x = actores_no_est, y = capacidades)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Relación entre Acciones no Estatales y Capacidad",
    x = "Acciones no estatales (ane)",
    y = "Capacidad (cap)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Capacidad versus acciones gubernamentales
CAPyAG <-ggplot(filter(datos,), aes(x = acciones_gob, y = capacidades)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Relación entre Acciones Gubernamentales y Capacidad",
    x = "Acciones gubernamentales (ag)",
    y = "Capacidad (cap)"
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
# CUANTITATIVA CONTINUA VS VARIABLE DE RESPUESTA MÚTIPLE  
# ==============================================================================





# ==============================================================================
# ANÁLISIS BIVARIADO: CAPACIDAD VS. ESTÁNDARES p70 (ESTILO PREMIUM)
# ==============================================================================


# ==============================================================================
# ANÁLISIS BIVARIADO: CAPACIDAD VS. ESTÁNDARES p70 (ESTRUCTURA SOLICITADA)
# ==============================================================================


colores_p70 <- c("No Cumple" = "gray90", "Cumple" = "steelblue")

# SEGURIDAD, PROTECCIÓN DE DATOS Y PROTECCIÓN LABORAL 
grafico_p70_1 <- datos %>%
  select(capacidades, p70_seguridad, p70_datos, p70_laboral) %>%
  pivot_longer(cols = starts_with("p70"), names_to = "Indicador", values_to = "Cumple") %>%
  mutate(Cumple_label = factor(Cumple, levels = c(0, 1), labels = c("No Cumple", "Cumple"))) %>%
  ggplot(aes(x = Cumple_label, y = capacidades, fill = Cumple_label)) +
  geom_boxplot(alpha = 0.8, color = "black", outlier.size = 2, outlier.shape = 16) + 
  facet_wrap(~Indicador, nrow = 1, labeller = as_labeller(c(
    "p70_seguridad" = "Seguridad",
    "p70_datos" = "Protección de Datos",
    "p70_laboral" = "Protección Laboral"
  ))) +
  scale_fill_manual(values = colores_p70) +
  theme_minimal(base_size = 16) +
  labs(title = "Capacidad vs. Estándares: Seguridad, Datos y Laboral", x = NULL, y = "Capacidad (cap)") +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14)
  )


#  MITIGACIÓN DE SESGOS Y SUPERVISIÓN HUMANA

grafico_p70_2 <- datos %>%
  select(capacidades, p70_sesgo, p70_supervision) %>%
  pivot_longer(cols = starts_with("p70"), names_to = "Indicador", values_to = "Cumple") %>%
  mutate(Cumple_label = factor(Cumple, levels = c(0, 1), labels = c("No Cumple", "Cumple"))) %>%
  ggplot(aes(x = Cumple_label, y = capacidades, fill = Cumple_label)) +
  geom_boxplot(alpha = 0.8, color = "black", outlier.size = 2, outlier.shape = 16) + 
  facet_wrap(~Indicador, nrow = 1, labeller = as_labeller(c(
    "p70_sesgo" = "Mitigación de Sesgos",
    "p70_supervision" = "Supervisión Humana"
  ))) +
  scale_fill_manual(values = colores_p70) +
  theme_minimal(base_size = 16) +
  labs(title = "Capacidad vs. Estándares: Sesgos y Supervisión", x = NULL, y = "Capacidad (cap)") +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14)
  )


# DERECHOS DE INFANCIA Y TRANSPARENCIA 

grafico_p70_3 <- datos %>%
  select(capacidades, p70_infancia, p70_transparencia) %>%
  pivot_longer(cols = starts_with("p70"), names_to = "Indicador", values_to = "Cumple") %>%
  mutate(Cumple_label = factor(Cumple, levels = c(0, 1), labels = c("No Cumple", "Cumple"))) %>%
  ggplot(aes(x = Cumple_label, y = capacidades, fill = Cumple_label)) +
  geom_boxplot(alpha = 0.8, color = "black", outlier.size = 2, outlier.shape = 16) + 
  facet_wrap(~Indicador, nrow = 1, labeller = as_labeller(c(
    "p70_infancia" = "Derechos Infancia",
    "p70_transparencia" = "Transparencia"
  ))) +
  scale_fill_manual(values = colores_p70) +
  theme_minimal(base_size = 16) +
  labs(title = "Capacidad vs. Estándares: Infancia y Transparencia", x = NULL, y = "Capacidad (cap)") +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14)
  )



# ==============================================================================
#  Print de Graficos
# ==============================================================


#Prioridad

print(CAPyANE)
print(CAPyAG)
print(grafico_biv_acciones_mult)
print(grafico_biv_actores_mult)
print(datos_acad_cap)
print(datos_priv_cap)
print(grafico_p70_1)
print(grafico_p70_2)
print(grafico_p70_3)

#------------------------------------------


print(grafico_box_acciones)
print(grafico_box_actores)
print(grafico_box_capacidades)
