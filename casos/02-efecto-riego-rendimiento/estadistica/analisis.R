# TP: Estadistica Descriptiva Agricola
# Dataset: datos_agricultura.csv



# --- Parte 1: Exploracion de datos ---


 # 1. Cargar los datos

#setwd("C:/Users/bever/Desktop/ADAGRO/data-analytics-agro/casos/02-efecto-riego-rendimiento")
#getwd()                 
#list.files("data")      
datos <- read.csv("data/datos_agricultura.csv", fileEncoding = "UTF-8-BOM")
 
 # 2. Mostrar los primeros 6 registros

head(datos, 6)
 
 # 3. Lotes que contiene la base

n_lotes <- nrow(datos)   
cat("La base contiene", n_lotes, "lotes.\n") # 30 lotes

 # 4. Variables

n_vars <- ncol(datos)   # 8 variables
cat("La base contiene", n_vars, "variables:", paste(names(datos), 
                                                    collapse = ", "), "\n") 


 # 5. Categóricas y numéricas

str(datos)
tipos <- data.frame(variable = names(datos),
                    clase_R = sapply(datos, class),
                    tipo = c("identificador", "categórica", "numérica", "numérica",
                             "numérica", "numérica", "categórica", "categórica"),
                    row.names = NULL
                    )
tipos
 

 # 6. Resumen Estadístico de las variables numericas

vars_num <- c("superficie_ha", "rendimiento_t_ha", "lluvia_mm", "fertilizante_kg_ha")
summary(datos[, vars_num])






# --- Parte 2: Medidas de tendencia central ---


# Variable analizada: rendimiento_t_ha

r <- datos$rendimiento_t_ha

 # 7. Media

media <- mean(r)
cat("La media del rendimiento es", round(media, 2), "t/ha\n")
 
 # 8. Mediana

mediana <- median(r)
cat("La mediana del rendimiento es", mediana, "t/ha\n")

 # 9. Moda

# R no trae una función de moda, asi que debo verificar en la tabla de frecuencias

frecuencias <- table(r)
moda <- names(frecuencias)[frecuencias == max(frecuencias)]
cat("Moda(s):", moda, "- cada una aparece", max(frecuencias), "veces\n")

# Hay tres modas (3,0; 3,1 y 4,1), con solo 2 lotes cada una: aporta poco.
# Ademas, todas estan en el grupo de rendimeintos bajos, asi que la tabla de frencuencias
# no detectan el segundo grupo.


 # 10. Comparar media y mediana

sort(r)
tapply(r, datos$cultivo, median)

# La media (4,74 t/ha) es mayor que la mediana (3,95 t/ha), y en sort(r) se ve que los lotes se juntan en dos grupos:
# 20 lotes entre 2,6 y 4,3 y 10 lotes entre 6,5 y 8,1, sin ningun lote entre 4,3 y 6,5.
# La media cae justo en ese hueco, asi que ningun lote rinde cerca del promedio.
# La mediana refleja mejor al grupo mayoritario, pero tampoco describe a los lotes de maíz. Una sola medida no alcanza.
# No puedo afirmar todavia que la diferencia entre grupos se deba solo al cultivo.




# --- Parte 3: Dispersión y posición ---
 
# variable analizada: rendimiento_t_ha

r <- datos$rendimiento_t_ha

 # 11. Mínimo y Máximo

minimo <- min(r)
maximo <- max(r)
cat("Mínimo:", minimo, "t/ha - Máximo:", maximo, "t/ha\n")

 # 12. Rango

rango <- maximo - minimo
cat("Rango:", rango, "t/ha\n")

 # 13. Q1, Q2 y Q3

cuartiles <- quantile(r, probs = c(0.25, 0.50, 0.75))
cuartiles

 # 14. Rango Intercuartílico (IQR)

iqr <- IQR(r)       # Q3 - Q1
cat("IQR:", iqr, "t/ha\n")

 # 15. Valores donde se encuentra el 50% de los rendimientos

cat("El 50 % central va de", cuartiles[1], "a", cuartiles[3], "t/ha\n")

 # El 50% central de los rendimientos va de 3,125 a 6,875 t/ha
# (IQR= 3,75 t/ha), un intervalo muy amplio.
# Ese intervalo mezcla cultivos con niveles de rendimiento distintos y abarca el hueco donde no hay ningun lote.
# Para un productor, un rendimiento típico general no sirve: la expectativa depende del cultivo.


 
# --- Parte 4: Gráficos ---


# Colores fijos por cultivo.

colores <- c(Maiz = "#E69F00", Soja = "#009E73", Trigo = "#0072B2")
par(las = 1)   # los números del eje quedan en horizontal


 # Barras: lotes por cultivo

conteo <- table(datos$cultivo)

barplot(conteo, ylim = c(0, 12), col = NA, border = NA,     
        main = "Cantidad de lotes por cultivo",
        xlab = "Cultivo", ylab = "N° de lotes")
grid(nx = NA, ny = NULL, col = "gray85", lty = 1)            
bp <- barplot(conteo, col = colores[names(conteo)], add = TRUE)
text(bp, as.vector(conteo), labels = as.vector(conteo), pos = 3)   

# Soja 11 lotes, Maíz 10 y Trigo 9
# La base se ve equilibrada entre cultivos, por lo que ninguno pesa de más en las medidas generales.


# Histograma: Distribución del rendimiento
 
cortes <- seq(2.5, 8.5, by = 0.5)

hist(r, breaks = cortes, ylim = c(0, 7), col = NA, border = NA,
     main = "Distribución del rendimiento (t/ha)",
     xlab = "Rendimiento (t/ha)", ylab = "Frecuencia")
grid(nx = NA, ny = NULL, col = "gray85", lty = 1)
hist(r, breaks = cortes, col = "#9DB4C0", border = "white",
     labels = TRUE, add = TRUE)                              
abline(v = media,   lty = 2, lwd = 2, col = "#D55E00")
abline(v = mediana, lty = 2, lwd = 2, col = "#0072B2")
legend("topright", bty = "n", lty = 2, lwd = 2,
       col = c("#D55E00", "#0072B2"),
       legend = c(paste("Media:", round(media, 2)),
                  paste("Mediana:", mediana)))

# Distribución bimodal, con dos grupos separados por un hueco. 
# 20 lotes entre 2,5 y 4,5 t/ha y 10 lotes entre 6 y 8,5 t/ha
# ningun lote entre 4,5 y 6 t/ha.
# La media (4,74) cae en el hueco y la mediana (3,95) queda en el grupo mayoritario.
# Los grupos coinciden con los cultivos.


# Bloxplot: por cultivo
 
boxplot(rendimiento_t_ha ~ cultivo, data = datos, ylim = c(2, 9),
        col = NA, border = NA,
        main = "Rendimiento por cultivo",
        xlab = "Cultivo", ylab = "Rendimiento (t/ha)")
grid(nx = NA, ny = NULL, col = "gray85", lty = 1)
boxplot(rendimiento_t_ha ~ cultivo, data = datos, add = TRUE, outline = FALSE,
        col = adjustcolor(colores, alpha.f = 0.35), border = colores, boxwex = 0.5)
stripchart(rendimiento_t_ha ~ cultivo, data = datos, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.08,
           pch = 19, col = colores)

# Las 3 cajas no se superponen. El maíz (6,5 a 8,1 t/ha) con una mediana de 7,3 tiene una caja mas alta, o sea, mas variación entre sus lotes.
# No se ven puntos atípicos, y los puntos sobre las cajas muestran que cada una resume entre 9 y 11 lotes.



 # Dispersión: lluvia vs rendimiento
 
plot(datos$lluvia_mm, r, type = "n",
     main = "Lluvia vs. rendimiento",
     xlab = "Lluvia (mm)", ylab = "Rendimiento (t/ha)")
grid(col = "gray85", lty = 1)
points(datos$lluvia_mm, r, pch = 19, col = colores[datos$cultivo])

for (cu in names(colores)) {
  lotes_cu <- datos[datos$cultivo == cu, ]
  modelo   <- lm(rendimiento_t_ha ~ lluvia_mm, data = lotes_cu)
  ord      <- order(lotes_cu$lluvia_mm)
  lines(lotes_cu$lluvia_mm[ord], fitted(modelo)[ord], col = colores[cu], lwd = 2)
}
legend("topleft", legend = names(colores), col = colores, pch = 19, bty = "n")

# Dentro de cada cultivo, más lluvia se asocia a más rendimiento: 
# Cada 100mm más acompañan aproximadamente +1,0t/ha en maíz, +0,8 en trigo y +0,45 en soja.
# Con lluvias similares los cultivos rinde distinto: a 720mm el maíz rinde 7,2 y la soja 3,3.
# La mayor parte de la diferencia entre lotes viene del cultivo y no de la lluvia, 
# porque con la misma lluvia, la brecha entre maiz y soja sigue siendo enorme.


 # Torta: tipo de suelo

suelo <- table(datos$tipo_suelo)
pct   <- round(100 * as.vector(suelo) / sum(suelo))
etiquetas <- paste0(names(suelo), "\n", as.vector(suelo), " lotes (", pct, " %)")
colores_suelo <- c(Arcilloso = "#B86F4B", Arenoso = "#E8D5A8", Franco = "#7B5B3A")
par(mar = c(2, 2, 3, 6))   
pie(suelo, labels = etiquetas, col = colores_suelo[names(suelo)],
    border = "white", init.angle = 90,
    main = "Proporción de lotes según tipo de suelo")

# La mitad de los lotes tiene suelo franco, y los otros dos tipos se reparten casi parejo.


# --- Parte 5: Pregunta de Negocio: Efecto del riego ---

par(mar = c(5, 4, 4, 2) + 0.1)
colores_riego <- c(No = "#D55E00", Si = "#56B4E9")

# 16. Boxplot del rendimiento según riego

boxplot(rendimiento_t_ha ~ riego, data =  datos, ylim = c(2,9),
        col = NA,
        border = NA,
        main = "Rendimiento según riego",
        xlab = "Riego",
        ylab = "Rendimiento (t/ha)")
grid(nx = NA, ny = NULL, col = "gray85", lty = 1)
boxplot(rendimiento_t_ha ~ riego, data = datos, add = TRUE, outline = FALSE,
        col = adjustcolor(colores_riego, alpha.f = 0.35),
        border = colores_riego, boxwex = 0.5)
stripchart(rendimiento_t_ha ~ riego, data = datos, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.08,
           pch = 19, col = colores_riego)


# Rendimiento medio

table(datos$riego)
medias_riego <- tapply(r, datos$riego, mean)

 # 17. Con riego
cat("Rendimiento medio con riego:", round(medias_riego["Si"], 2), "t/ha\n")

 # 18. Sin riego
cat("Rendimiento medio sin riego:", round(medias_riego["No"], 2), "t/ha\n")


# 19. Cuál presenta mayor rendimiento promedio?

diferencia <- medias_riego["Si"] - medias_riego["No"]
cat("Diferencia (con riego - sin riego):", round(diferencia, 2), "t/ha\n")
cat("Aumento relativo:", round(100 * diferencia / medias_riego["No"]), "%\n")

mayor <- names(which.max(medias_riego))
cat("Mayor rendimiento promedio:", ifelse(mayor == "Si", "lotes con riego", "lotes sin riego"), "\n")



# 20. Interpretación del efecto del riego

# Está el riego repartido parejo entre cultivos?
table(datos$cultivo, datos$riego)
round(prop.table(table(datos$cultivo, datos$riego), margin = 1) * 100)

# Comparación dentro de cada cultivo (distinto objeto y distinto nombre que medias_riego:
# acá hay una media por cultivo y por grupo de riego)
aggregate(rendimiento_t_ha ~ cultivo + riego, data = datos, FUN = mean)

medias_cr <- tapply(r, list(datos$cultivo, datos$riego), mean)
dif_cr    <- medias_cr[, "Si"] - medias_cr[, "No"]          # diferencia en cada cultivo
n_cult    <- as.vector(table(datos$cultivo))
dif_ajustada <- sum(dif_cr * n_cult) / sum(n_cult)          # promedio ponderado por lotes
cat("Diferencia global:", round(diferencia, 2), "t/ha\n")
cat("Diferencia dentro de cada cultivo (promedio ponderado):", round(dif_ajustada, 2), "t/ha\n")

# Otras variables que también cambian con el riego
aggregate(cbind(lluvia_mm, fertilizante_kg_ha) ~ riego, data = datos, FUN = mean)



# Los lotes con riego rinden en promedio 5,66 t/ha y los lotes sin riego 3,81 t/ha, una diferencia de 1,85 t/ha (alrededor de 48 % más).
# Esa diferencia está inflada por la composición de los grupos. El riego no está repartido parejo entre cultivos: 8 de 10 lotes de maíz
# (80 %) tienen riego, contra 4 de 11 de soja (36 %) y 3 de 9 de trigo (33 %). 
# Como el maíz rinde bastante más que los otros cultivos, el grupo con riego tiene más maíz. 
# Al comparar dentro de cada cultivo, los lotes con riego siguen rindiendo algo más, pero la diferencia es mucho menor:
# +0,66 t/ha en maíz, +0,30 en soja y +0,25 en trigo.


# Conclusión General

# La base tiene 30 lotes y 8 variables, sin datos faltantes. El rendimiento medio es 4,74 t/ha, pero 
# ningú lote rinde cerca de este valor: hay dos grupos (soja y trigo entre 2,6 y 4,3 t/ha; maíz entre
# 6,5 y 8,1 t/ha), y eso explicaria la gran dispersión (IQR de 3,75 t/ha) sin valores atípicos.
# La lluvia acompaña al rendimiento dentro de cada cultivo, pero la mayor parte de la diferencia entre lotes viene del cultivo.
# Los lotes con riego rinden más (1,85 t/ha), pero esa brecha se reduce a unos 0,4 t/ha al comparar dentro de cada cultivo.
# Como el análisis es descriptivo y se basa en 30 lotes, los resultados describen esta base:
# no permiten generalizar ni establecer causas, y sin precios ni costos tampoco permiten decir que cultivo o práctica conviene económicamente.

#---------------------------------------------------------------
# Principios básicos de la Estadística aplicados en este TP

# - Población y muestra: los 30 lotes son una muestra; no sabemos cómo se eligieron, por eso no generalizamos.
# - Tipos de variables:
      # categóricas (cultivo, suelo, riego)
      # numéricas (rendimiento, lluvia, fertilizante, superficie)
      # identificador (lote)
# - Estadística descriptiva vs inferencial: este TP describe los datos; no hace inferencia sobre una población más grande.
# - Medidas resumen: 
      # tendencia central (media, mediana, moda)
      # dispersión (rango, IQR)
      # posición (cuartiles)
 # Ninguna sola alcanza si la distribución tiene dos grupos
# - Variabilidad y forma de la distribución: el promedio solo no describe los datos; hay que mirar como se reparten (histograma, boxplot)
# - Visualización: el gráfico se elige según el tipo de variable.
# - Correlación no implica causalidad, y las variables confusoras (cultivo) pueden inflar una diferencia entre grupos.