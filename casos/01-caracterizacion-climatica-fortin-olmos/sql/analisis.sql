-- Caso 1 - Consultas SQL sobre datos climaticos de Fortin Olmos
-- Motor: DuckDB, corre directo sobre el csv sin necesidad de importarlo antes

-- Vista base: apunto al csv ya filtrando las filas vacias del final
CREATE OR REPLACE VIEW clima AS
SELECT *
FROM read_csv_auto('../data/16_santa_fe_fortin_olmos_03.csv', delim=';')
WHERE Fecha IS NOT NULL;

-- 1. Exploracion inicial: cuantos registros hay y que rango de fechas cubren
SELECT
    COUNT(*) AS total_registros,
    MIN(Fecha) AS desde,
    MAX(Fecha) AS hasta
FROM clima;

-- 2. Temperatura media, minima y maxima por mes
SELECT
    strftime(Fecha, '%m') AS mes,
    ROUND(AVG(Temperatura_Abrigo_150cm), 2) AS temp_media,
    MIN(Temperatura_Abrigo_150cm) AS temp_min,
    MAX(Temperatura_Abrigo_150cm) AS temp_max
FROM clima
GROUP BY mes
ORDER BY mes;

-- 3. Dias con humedad muy alta (relevante por riesgo de enfermedades fungicas)
SELECT Fecha, Humedad_Media, Radiacion_Global
FROM clima
WHERE Humedad_Media > 90
ORDER BY Fecha;

-- 4. Comparacion: radiacion promedio en dias humedos vs dias secos
SELECT
    CASE WHEN Humedad_Media > 70 THEN 'humedo' ELSE 'seco' END AS condicion,
    ROUND(AVG(Radiacion_Global), 2) AS radiacion_media,
    COUNT(*) AS dias
FROM clima
GROUP BY condicion;
