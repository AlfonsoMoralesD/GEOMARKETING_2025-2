##-------------------------------------------##
##------ TRABAJO 2 + 3: MICROSIMULACIÓN -----##
##--- Clustering Fonasa/Isapre + Ingreso ----##
##------ Autor: Alfonso Morales (2025) ------##
##-------------------------------------------##

######################################
# 1. LIBRERÍAS
######################################
library(rakeR)
library(sf)
library(ggplot2)
library(data.table)
library(dplyr)
library(DBI)
library(RPostgres)
library(factoextra)
library(cluster)
library(vegan)
library(RColorBrewer)
library(GGally)
library(corrplot)
library(tidyr)
######################################
# 2. CARGA DE DATOS
######################################
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw     <- readRDS("data/casen_rm.rds")

######################################
# 3. PRE-PROCESAMIENTO CASEN
######################################
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "s13", "ypc")
casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna  <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

casen$esc  <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$e6a  <- as.numeric(unclass(casen$e6a))
casen$ypc  <- as.numeric(unclass(casen$ypc))
casen$s13  <- as.integer(unclass(casen$s13))

idx_na <- which(is.na(casen$esc))
fit  <- lm(esc ~ e6a, data = casen[-idx_na,])
pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))

casen$ID <- as.character(seq_len(nrow(casen)))

######################################
# 4. VARIABLES CENSO PARA RESTRICCIONES DE LA MICROSIMULACIÓN
######################################
col_cons   <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
edad_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels   <- grep("^esco", col_cons, value = TRUE)
sexo_levels  <- grep("^sexo", col_cons, value = TRUE)

casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = edad_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3], esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

######################################
# 5. MICROSIMULACIÓN
######################################
cons_censo_df$COMUNA <- substr(as.character(cons_censo_df$COMUNA), 1, 5)
casen$Comuna <- substr(as.character(casen$Comuna), 1, 5)

cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i <- cons_censo_comunas[[zona]]
  tmp    <- inds_list[[zona]]
  if (is.null(tmp) || nrow(tmp) == 0) return(NULL)
  
  col_order <- sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  inds_i <- tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID","Edad","Escolaridad","Sexo")
  
  w_frac <- weight(cons = cons_i, inds = inds_i, vars = c("Edad","Escolaridad","Sexo"))
  sim_i  <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(sim_i, tmp[, c("ID", "s13", "ypc")], by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")

######################################
# 6. AGREGAR VARIABLES A NIVEL ZONA
######################################
sim_df$fonasa <- ifelse(sim_df$s13 == 1, 1, 0)
sim_df$isapre <- ifelse(sim_df$s13 == 2, 1, 0)

# Ingreso microsimulado por zona
zonas_ingreso <- aggregate(ypc ~ zone, data = sim_df, FUN = median)
names(zonas_ingreso) <- c("geocodigo", "mediana_ingreso")
zonas_ingreso$geocodigo <- as.character(zonas_ingreso$geocodigo)

# % Fonasa / Isapre por zona
salud_promedio <- sim_df %>%
  group_by(zone) %>%
  summarise(
    ptje_fonasa = 100 * mean(fonasa, na.rm = TRUE),
    ptje_isapre = 100 * mean(isapre, na.rm = TRUE)
  ) %>%
  rename(geocodigo = zone)

######################################
# 7. CONEXIÓN BD PARA OBTENER CENSO
######################################
con <- dbConnect(
  Postgres(),
  dbname   = "censo_rm_2017",
  host     = "localhost",
  port     = 5433,
  user     = "postgres",
  password = "postgres"
)

sql_censo <- "
WITH agg AS (
  SELECT z.geocodigo::text AS geocodigo, c.nom_comuna,
    ROUND(AVG(v.cant_per), 2) AS cant_per
  FROM public.viviendas AS v
  JOIN public.zonas AS z 
      ON v.zonaloc_ref_id = z.zonaloc_ref_id
  JOIN public.comunas AS c 
      ON z.codigo_comuna = c.codigo_comuna
  JOIN public.provincias AS pr 
      ON pr.provincia_ref_id = c.provincia_ref_id
  WHERE (pr.nom_provincia = 'SANTIAGO' 
         OR c.nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))
  GROUP BY z.geocodigo, c.nom_comuna
)
SELECT 
  a.geocodigo,
  a.nom_comuna,
  a.cant_per,
  shp.geom
FROM agg AS a
JOIN dpa.zonas_censales_rm AS shp 
    ON shp.geocodigo::text = a.geocodigo
WHERE shp.urbano = 1;
"

censo_sf <- st_read(con, query = sql_censo)

######################################
# 8. UNIR VARIABLES
######################################
zonas_cluster <- censo_sf %>%
  left_join(salud_promedio, by = "geocodigo") %>%
  left_join(zonas_ingreso,   by = "geocodigo")

zn <- zonas_cluster

######################################
# 9. PREPARAR PARA CLUSTERING
######################################
datos_cluster <- zn %>%
  st_drop_geometry() %>%
  select(ptje_fonasa, ptje_isapre, cant_per, mediana_ingreso) %>%
  na.omit() %>%
  scale()

fviz_nbclust(datos_cluster, kmeans, method = "wss") +
  labs(title="Método del codo - Número óptimo de clusters")

k_opt <- 3

set.seed(123)
modelo <- kmeans(datos_cluster, centers = k_opt, nstart=30)

zonas_cluster$cluster <- as.factor(modelo$cluster)

######################################
# 10. MAPA FINAL
######################################
## Corro esta sección del codigo previo al indice para poder 
## mejorar el plot de los clusters
tabla_shannon <- zonas_cluster %>%
  st_drop_geometry() %>%
  group_by(nom_comuna, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nom_comuna) %>%
  summarise(H = diversity(n, index = "shannon"))

comunas_geom <- zonas_cluster %>%
  select(nom_comuna, geom) %>%
  group_by(nom_comuna) %>%
  summarise(geometry = st_union(geom), .groups = "drop") %>%
  st_as_sf()

ggplot() +
  geom_sf(data = zonas_cluster, aes(fill = as.factor(cluster)), color = NA) +
  geom_sf(data = comunas_geom, fill = NA, color = "grey30", size = 0.35) +
  geom_sf_text(data = comunas_geom, aes(label = nom_comuna), 
               size = 2, color = "black", check_overlap = TRUE) +
  scale_fill_brewer(palette = "Set3", name = "Cluster") +
  labs(
    title = "Distribución de Clusters en el Gran Santiago",
    subtitle = "Microsimulación Casen + Censo (Ingreso, Sistema de Salud y Tamaño del Hogar)",
    caption = "Elaboración propia"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

#RESUMEN DE CADA CLUSTER
zonas_cluster %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    ingreso = mean(mediana_ingreso,na.rm=TRUE),
    fonasa = mean(ptje_fonasa),
    isapre = mean(ptje_isapre),
    hogar  = mean(cant_per),
    n = n()
  )
######################################
# 11. Índice de Shannon, Matriz de correlación y Gráfico de dispersión de variables
######################################
shannon_comuna <- left_join(comunas_geom, tabla_shannon, by = "nom_comuna")

ggplot(shannon_comuna) +
  geom_sf(aes(fill = H), color = "grey30", size = 0.1) +
  geom_sf(data = comunas_geom, fill = NA, color = "grey30", size = 0.35) +
  geom_sf_text(data = comunas_geom, aes(label = nom_comuna), 
               size = 2, color = "black", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(
    title = "Índice de Shannon - Variabilidad Intra Comunal",
    subtitle = "Diversidad de clusters según las variables",
    caption = "Elaboración propia",
    fill = "Shannon H"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5)
  )
### Matriz de correlación.
vars_cluster <- c("mediana_ingreso", "ptje_fonasa", "ptje_isapre", "cant_per")

df_cluster <- zonas_cluster %>%
  st_drop_geometry() %>%
  select(all_of(vars_cluster)) %>%
  na.omit()

df_cluster <- df_cluster %>%
  rename(
    "Ingreso medio" = mediana_ingreso,
    "% Fonasa" = ptje_fonasa,
    "% Isapre" = ptje_isapre,
    "Tamaño del hogar" = cant_per
  )

cor_matrix <- cor(df_cluster)

corrplot(
  cor_matrix, 
  method = "color",
  col = RColorBrewer::brewer.pal(8, "PuOr"),  
  addCoef.col = "black", 
  tl.col = "black",
  tl.srt = 35,            
  number.cex = 0.9,       
  mar = c(0, 0, 1, 0)     
) 

### Gráfico de dispersión
vars_cluster1 <- c("mediana_ingreso", "ptje_fonasa", "ptje_isapre", "cluster")

df_cluster <- zonas_cluster %>%
  st_drop_geometry() %>%
  select(all_of(vars_cluster1)) %>%
  na.omit()

df_long <- df_cluster %>%
  pivot_longer(cols = c(ptje_fonasa, ptje_isapre),
               names_to = "tipo_salud",
               values_to = "porcentaje")

df_long$tipo_salud <- factor(df_long$tipo_salud,
                             levels = c("ptje_fonasa", "ptje_isapre"),
                             labels = c("% Fonasa", "% Isapre"))

plot_disp <- ggplot(df_long, aes(x = mediana_ingreso, y = porcentaje, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~ tipo_salud, ncol = 2, scales = "free_y") +
  labs(
    title = "Relación entre ingreso medio y sistema de salud",
    subtitle = "Comparación visual entre zonas con % Fonasa e % Isapre",
    x = "Ingreso medio",
    y = "Porcentaje",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13)

plot_disp
