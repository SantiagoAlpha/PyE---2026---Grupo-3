

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













# Algunas medidas resumen 
summary(datos_limpios)
summary(datos_limpios[,c(2,3,4)])

# Otras funciones para obtener medidas

# Posición: tendencia central
mean(datos_limpios$altura) # Media aritmética
median(datos_limpios$altura) # Mediana

attach(datos_limpios) # Puedo fijar los datos por comodidad

# Posición: otras
min(altura) 
max(altura)
quantile(altura) # 5 medidas resumen
quantile(altura, 0.9) # Otros percentiles
sort(table(especie), decreasing = TRUE)[1] # Moda

# Dispersión
range(altura) # Valores mín y max
max(altura) - min(altura) # Rango
sd(altura) # Desvío estándar
var(altura) # Variancia
IQR(altura) # Rango intercuartílico
round(sd(altura)/mean(altura)*100,1) # Coeficiente de variación

# Otras medidas
var(altura,diametro) # Covariancia
cor(altura,diametro) # Correlación lineal

# Medidas por grupos
datos_limpios %>% group_by(especie) %>%
  summarise(Promedio = median(altura),
            Desv.Est. = IQR(altura),
            Mínimo = min(altura),
            Máximo = max(altura))

# Distribuciones condicionales
tabyl(datos_limpios, tiempo, follaje) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", "Origen", "Tipo de follaje")
