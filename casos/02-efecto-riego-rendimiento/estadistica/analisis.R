# TP: Estadistica Descriptiva Agricola
# Dataset: datos_agricultura.csv (30 lotes: soja, maiz, trigo)
 
# --- Parte 1: Exploracion de datos ---
 
datos <- read.csv("datos_agricultura.csv", stringsAsFactors = TRUE)
 
head(datos, 6)
 
nrow(datos)   # 30 lotes
ncol(datos)   # 8 variables
 
names(datos)
 
str(datos)
# Categoricas: lote (identificador), cultivo, tipo_suelo, riego
# Numericas: superficie_ha, rendimiento_t_ha, lluvia_mm, fertilizante_kg_ha
 
summary(datos[, c("superficie_ha", "rendimiento_t_ha",
                   "lluvia_mm", "fertilizante_kg_ha")])
 
# --- Parte 2: Medidas de tendencia central ---
 
r <- datos$rendimiento_t_ha
 
mean(r)
median(r)
 
# Para la moda, mejor mirar la tabla de frecuencias completa en vez de
# quedarme con un solo valor - asi veo si hay empate
table(r)[table(r) == max(table(r))]


# --- Parte 3: Dispersion y posicion ---
 
min(r); max(r)
diff(range(r))
 
quantile(r, probs = c(0.25, 0.50, 0.75))
IQR(r)
 
# --- Parte 4: Graficos ---
 
barplot(table(datos$cultivo),
        main = "Cantidad de lotes por cultivo",
        xlab = "Cultivo", ylab = "N de lotes",
        col = c("gold", "tomato", "skyblue"))
 
hist(r, breaks = 8, col = "lightgreen",
     main = "Distribucion del rendimiento (t/ha)",
     xlab = "Rendimiento (t/ha)", ylab = "Frecuencia")
 
boxplot(rendimiento_t_ha ~ cultivo, data = datos,
        main = "Rendimiento por cultivo",
        xlab = "Cultivo", ylab = "Rendimiento (t/ha)",
        col = c("gold", "tomato", "skyblue"))
 
plot(datos$lluvia_mm, datos$rendimiento_t_ha,
     main = "Lluvia vs Rendimiento",
     xlab = "Lluvia (mm)", ylab = "Rendimiento (t/ha)",
     pch = 19, col = "darkblue")
abline(lm(rendimiento_t_ha ~ lluvia_mm, data = datos),
       col = "red", lwd = 2)
 
pie(table(datos$tipo_suelo),
    main = "Proporcion de lotes segun tipo de suelo",
    col = c("sandybrown", "chocolate", "wheat"))

    