# Caso 1 - Analisis climatico de Fortin Olmos, Santa Fe
# Quiero ver como se relacionan temperatura, humedad, presion y radiacion,
# para sacar alguna conclusion util pensando en decisiones agro

library(readr)
library(dplyr)

datos <- read_delim("casos/01-caracterizacion-climatica-fortin-olmos/data/16_santa_fe_fortin_olmos_03.csv", delim = ";")

# El csv tiene un monton de filas vacias al final (quedaron del excel original),
# asi que me quedo solo con las filas que tienen fecha
datos <- datos %>% filter(!is.na(Fecha))

# Paso Fecha a Date nomas (sin la hora, que siempre es 00:00:00 y no aporta nada)
datos$Fecha <- as.Date(datos$Fecha)

str(datos)

# --- Validacion de los datos, antes de analizar nada ---

# Cuantos datos faltan por columna
colSums(is.na(datos))

# Fechas duplicadas? no deberia haber
sum(duplicated(datos$Fecha))

# Valores fuera de rango fisico posible
datos %>% filter(Humedad_Media < 0 | Humedad_Media > 100)
datos %>% filter(Radiacion_Global < 0)
datos %>% filter(Presion_Media < 950 | Presion_Media > 1050)

# Con esto ya se puede confiar en que las 410 filas que quedaron estan bien

# La serie va de sept 2021 a dic 2022 pero no es continua, hay huecos
rango <- range(datos$Fecha)
dias_totales <- as.numeric(diff(rango)) + 1
cat("Rango de fechas:", format(rango), "\n")
cat("Deberia haber", dias_totales, "dias, pero hay", nrow(datos), "registros\n")
cat("Faltan", dias_totales - nrow(datos), "dias en la serie\n")

# En que meses se concentran los dias faltantes? (para saber si el sensor
# fallaba siempre en la misma epoca o es aleatorio)
secuencia_completa <- seq(rango[1], rango[2], by = "day")
fechas_faltantes <- setdiff(secuencia_completa, datos$Fecha) |> as.Date(origin = "1970-01-01")
table(format(fechas_faltantes, "%Y-%m"))


# --- Estadistica descriptiva ---

summary(datos[, c("Temperatura_Abrigo_150cm", "Humedad_Media",
                  "Presion_Media", "Radiacion_Global")])

# El desvio estandar summary() no lo muestra, lo calculo aparte
sapply(datos[, c("Temperatura_Abrigo_150cm", "Humedad_Media",
                 "Presion_Media", "Radiacion_Global")],
       sd, na.rm = TRUE)


# --- Analisis por mes / estacionalidad ---

datos$mes <- format(datos$Fecha, "%m")

datos %>%
  group_by(mes) %>%
  summarise(temp_media = mean(Temperatura_Abrigo_150cm, na.rm = TRUE),
            humedad_media = mean(Humedad_Media, na.rm = TRUE),
            radiacion_media = mean(Radiacion_Global, na.rm = TRUE))

boxplot(Radiacion_Global ~ mes, data = datos,
        main = "Radiacion Global por mes",
        xlab = "Mes", ylab = "Radiacion Global", col = "lightgreen")

boxplot(Humedad_Media ~ mes, data = datos,
        main = "Humedad Media por mes",
        xlab = "Mes", ylab = "Humedad Media (%)", col = "lightblue")


# --- Correlacion entre las variables ---

correlaciones <- cor(datos[, c("Temperatura_Abrigo_150cm", "Humedad_Media",
                               "Presion_Media", "Radiacion_Global")],
                     use = "complete.obs")
round(correlaciones, 2)


# --- Regresion simple: Radiacion ~ Humedad (la correlacion mas fuerte, -0.80) ---

modelo_rad_hum <- lm(Radiacion_Global ~ Humedad_Media, data = datos)
summary(modelo_rad_hum)

plot(datos$Humedad_Media, datos$Radiacion_Global,
     main = "Radiacion Global vs Humedad Media",
     xlab = "Humedad Media (%)", ylab = "Radiacion Global",
     pch = 19, col = "darkgreen")
abline(modelo_rad_hum, col = "red", lwd = 2)


# --- Segunda regresion simple: Temperatura ~ Presion (la otra correlacion fuerte, -0.75) ---
# la idea es comparar cual de los dos predictores explica mejor su variable

modelo_temp_pres <- lm(Temperatura_Abrigo_150cm ~ Presion_Media, data = datos)
summary(modelo_temp_pres)

plot(datos$Presion_Media, datos$Temperatura_Abrigo_150cm,
     main = "Temperatura vs Presion Media",
     xlab = "Presion Media (hPa)", ylab = "Temperatura (C)",
     pch = 19, col = "steelblue")
abline(modelo_temp_pres, col = "red", lwd = 2)


# Comparo los dos R-cuadrado para ver cual modelo explica mejor
cat("R2 Radiacion ~ Humedad:", summary(modelo_rad_hum)$r.squared, "\n")
cat("R2 Temperatura ~ Presion:", summary(modelo_temp_pres)$r.squared, "\n")