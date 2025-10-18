#####################
## 1) Librerías #####
#####################

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(biscale)

############################
## 2) Configuración BD #####
############################

db_host = "localhost"
db_port = 5433
db_name = "censo_rm_2017"
db_user = "postgres"
db_password = "postgres"

# Conexión
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

########################
## 3) Consulta SQL #####
########################

sql_indicadores = "

WITH agg AS 
(
SELECT 

z.geocodigo::double precision AS geocodigo,
c.nom_comuna,

-- INDICADOR 1: Porcentaje de Población Migrante (Adultos).
-- Fórmula: (Total de migrantes > 18 años / Total de personas > 18 años) * 100
ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99) AND p.p09 > 18) * 100.0 / 
    NULLIF(COUNT(*) FILTER (WHERE p.p09 > 18), 0), 2
    ) AS ptje_migrantes_adultos,
    
-- INDICADOR 2: Porcentaje de Migrantes ADULTOS con Escolaridad Incompleta
-- Fórmula: (Total de migrantes > 18 con escolaridad < 8 / Total de migrantes > 18) * 100
ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99) AND p.p15 < 8 AND p.p09 > 18) * 100.0 / 
    NULLIF(COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99) AND p.p09 > 18), 0), 2
    ) AS ptje_esc_inc_mig_adultos

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id
WHERE (pr.nom_provincia = 'SANTIAGO' OR c.nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))
GROUP BY z.geocodigo, c.nom_comuna
)
SELECT a.geocodigo,
    shp.geom,
    a.nom_comuna,
    a.ptje_migrantes_adultos,
    a.ptje_esc_inc_mig_adultos
FROM agg AS a
JOIN dpa.zonas_censales_rm AS shp ON shp.geocodigo = a.geocodigo
WHERE shp.urbano = 1;"
# Almacenar DF

df_indicadores = st_read(con, query = sql_indicadores)


########################
### 4) Marco comunal ###
########################

sql_comunas = "

SELECT
geocodigo::double precision AS geocodigo,
geom
FROM dpa.zonas_censales_rm
WHERE urbano = 1
AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'));
"

sf_comunas = st_read(con, query = sql_comunas)


#######################
## 5) Pequeño EDA #####
#######################

# Grafico de barras % migrantes adultos
ggplot(df_indicadores, aes(x = ptje_migrantes_adultos)) +
  geom_histogram(bins = 30, fill = '#226e6e', color = 'white') +
  labs(title = "Distribución de % Migrantes Adultos", 
       x = "% Migrantes Adultos",                  
       y = "Frecuencia")

# Grafico de dispersion migracion y escolaridad incompleta en adultos
grafico_dispersion <- ggplot(df_indicadores, aes(x = ptje_migrantes_adultos, y = ptje_esc_inc_mig_adultos)) +
  geom_point(alpha = 0.6, color = "#0072B2") + 
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linetype = "dashed") +
  labs(
    title = "Relación entre Migración y Escolaridad Incompleta en Adultos", # <-- Título actualizado
    subtitle = "Datos por zona censal en la Provincia de Santiago (Censo 2017)",
    x = "% de Población Migrante Adulta",                                 # <-- Eje X actualizado
    y = "% de Migrantes Adultos con Escolaridad Incompleta",              # <-- Eje Y actualizado
    caption = "Fuente: Elaboración propia con datos del Censo 2017."
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"))
print(grafico_dispersion)

##############################
## 6) Mapas univariados #####
##############################

mapa_migrantes <- ggplot() +
  geom_sf(data = df_indicadores, aes(fill = ptje_migrantes_adultos), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(name = "% Migrantes \nAdultos") +
  labs(title = "Población Migrante Adulta") +
  theme_void() +
  theme(legend.position = "bottom")

# Mapa 2: Escolaridad Incompleta en Migrantes Adultos
mapa_escolaridad <- ggplot() +
  geom_sf(data = df_indicadores, aes(fill = ptje_esc_inc_mig_adultos), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(name = "% Esc. Incompleta \nen Migrantes Adultos") +
  labs(title = "Escolaridad Incompleta en Migrantes Adultos") +
  theme_void() +
  theme(legend.position = "bottom") 

# Visualización de ambos mapas
plot_grid(mapa_migrantes, mapa_escolaridad, align = "h")


#############################
## 7) Mapa Bivariado ########
#############################

df_bivariado <- bi_class(df_indicadores, x = ptje_migrantes_adultos, y = ptje_esc_inc_mig_adultos, style = "jenks", dim = 3)

# Crear el mapa
mapa_biv <- ggplot() +
  geom_sf(data = df_bivariado, aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_sf(data = sf_comunas, fill = NA, color = "white", linewidth = 0.5) +
  labs(
    title = "Mapa Bivariado: Migración y Escolaridad en Adultos",
    subtitle = "Provincia de Santiago, por zona censal",
    caption = "Fuente: Elaboración propia con datos del Censo 2017"
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
# Crear la leyenda bivariada
leyenda_biv <- bi_legend(pal = "DkBlue",
                         dim = 3,
                         xlab = "% Migrantes Adultos ", 
                         ylab = "% Esc. Incompleta ",
                         size = 8) 
# Combinar mapa y leyenda en una sola visualización final
final_plot <- ggdraw() +
  draw_plot(mapa_biv, 0, 0, 1, 1) +
  draw_plot(leyenda_biv, 0.05, 0.05, 0.25, 0.25) 
#Visualizar mapa
print(final_plot)


