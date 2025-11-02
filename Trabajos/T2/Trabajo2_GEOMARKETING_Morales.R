######################################
# 1. Librerías necesarias
######################################
library(rakeR)
library(sf)
library(ggplot2)
library(data.table)
library(cowplot)
library(dplyr)
library(rprojroot)
######################################
# 2. Carga de datos
######################################

cons_censo_df <- readRDS("data/cons_censo_df.rds")      
casen_raw <- readRDS("data/casen_rm.rds")        
zonas_gs <- sf::st_read("data/zonas_gs_ingreso.geojson")

######################################
# 3. Preprocesamiento CASEN
######################################
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "s13")
casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# Extraer código de comuna
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

# Conversión de etiquetas a numérico
casen$esc  <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$e6a  <- as.numeric(unclass(casen$e6a))
casen$s13  <- as.integer(unclass(casen$s13))

# Filtrar sistemas de salud válidos
casen <- casen[casen$s13 %in% c(1, 2), ]  # Solo FONASA e ISAPRE

# Imputar NA en escolaridad
idx_na <- which(is.na(casen$esc))
if (length(idx_na) > 0) {
  fit <- lm(esc ~ e6a, data = casen[-idx_na,])
  pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
  casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))
}

# ID único
casen$ID <- as.character(seq_len(nrow(casen)))

######################################
# 4. Clasificaciones demográficas
######################################
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
edad_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo", col_cons, value = TRUE)

# edad
casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = edad_levels,
  right = FALSE, include.lowest = TRUE
)

# Escolaridad
casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

# Sexo
casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)), 
  levels = sexo_levels
)

######################################
# 5. Microsimulación con rakeR
######################################
cons_censo_df$COMUNA <- substr(as.character(cons_censo_df$COMUNA), 1, 5)
casen$Comuna <- substr(as.character(casen$Comuna), 1, 5)
comunas_validas <- unique(cons_censo_df$COMUNA)
casen <- casen %>% filter(Comuna %in% comunas_validas)

cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i <- cons_censo_comunas[[zona]]
  tmp <- inds_list[[zona]]
  if(is.null(tmp) || nrow(tmp) == 0) return(NULL)
  
  col_order <- sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  inds_i <- tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID","Edad","Escolaridad","Sexo")
  
  w_frac <- weight(cons = cons_i, inds = inds_i, vars = c("Edad","Escolaridad","Sexo"))
  sim_i  <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  tmp_sub <- tmp[, c("ID","s13","edad")]
  sim_i$ID <- as.character(sim_i$ID)
  tmp_sub$ID <- as.character(tmp_sub$ID)
  out <- merge(sim_i, tmp_sub, by = "ID", all.x = TRUE)
  
  if(!("zone" %in% names(out))) out$zone <- as.character(cons_i$GEOCODIGO[1])
  out$COMUNA <- zona
  out
})

sim_list <- Filter(Negate(is.null), sim_list)
sim_df <- data.table::rbindlist(sim_list, fill = TRUE)

######################################
# 6. Clasificación etaria general
######################################
sim_df$grupo_edad <- cut(
  sim_df$edad,
  breaks = c(18, 29, 59, Inf),
  labels = c("Adultos jóvenes (18–29)", "Adultos (30–59)", "Mayores (60+)"),
  right = TRUE
)

######################################
# 7. Calcular proporciones por grupo etario
######################################
sim_df$fonasa <- ifelse(sim_df$s13 == 1, 1, 0)
sim_df$isapre <- ifelse(sim_df$s13 == 2, 1, 0)
sim_df$geocodigo <- as.character(sim_df$zone)

salud_edad <- sim_df %>%
  group_by(geocodigo, grupo_edad) %>%
  summarise(
    n_total = n(),
    n_fonasa = sum(fonasa, na.rm = TRUE),
    n_isapre = sum(isapre, na.rm = TRUE)
  ) %>%
  mutate(
    ptje_fonasa = 100 * n_fonasa / pmax(1, n_total),
    ptje_isapre = 100 * n_isapre / pmax(1, n_total)
  )

zonas_salud_edad <- merge(zonas_gs, salud_edad, by = "geocodigo", all.x = TRUE)

######################################
# 8. Mapas
######################################
col_comuna <- grep("comuna", names(zonas_gs), ignore.case = TRUE, value = TRUE)[1]
comunas_sf <- zonas_gs %>%
  group_by(.data[[col_comuna]]) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  rename(COMUNA = !!sym(col_comuna))
centroides_comunas <- st_centroid(comunas_sf)
centroides_comunas <- cbind(centroides_comunas, st_coordinates(centroides_comunas))
colores_gradiente <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")

# FONASA - Adultos jóvenes
mapa_fonasa_jovenes <- ggplot() +
  geom_sf(data = filter(zonas_salud_edad, grupo_edad == "Adultos jóvenes (18–29)"),
          aes(fill = ptje_fonasa), color = NA) +
  geom_sf(data = comunas_sf, fill = NA, color = "grey30", size = 0.1) +
  geom_text(data = centroides_comunas, aes(X, Y, label = COMUNA), size = 1.5, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = colores_gradiente, name = "% FONASA", limits = c(0,100), na.value = "grey90") +
  labs(title = "FONASA - Adultos Jóvenes", subtitle = "Microsimulación CASEN–CENSO") +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# FONASA - Adultos y Mayores
mapa_fonasa_adultos <- ggplot() +
  geom_sf(data = filter(zonas_salud_edad, grupo_edad %in% c("Adultos (30–59)", "Mayores (60+)")),
          aes(fill = ptje_fonasa), color = NA) +
  geom_sf(data = comunas_sf, fill = NA, color = "grey30", size = 0.1) +
  geom_text(data = centroides_comunas, aes(X, Y, label = COMUNA), size = 1.5, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = colores_gradiente, name = "% FONASA", limits = c(0,100), na.value = "grey90") +
  labs(title = "FONASA - Adultos y Mayores", subtitle = "Microsimulación CASEN–CENSO") +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# ISAPRE - Adultos jóvenes
mapa_isapre_jovenes <- ggplot() +
  geom_sf(data = filter(zonas_salud_edad, grupo_edad == "Adultos jóvenes (18–29)"),
          aes(fill = ptje_isapre), color = NA) +
  geom_sf(data = comunas_sf, fill = NA, color = "grey30", size = 0.1) +
  geom_text(data = centroides_comunas, aes(X, Y, label = COMUNA), size = 1.5, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = colores_gradiente, name = "% ISAPRE", limits = c(0,100), na.value = "grey90") +
  labs(title = "ISAPRE - Adultos Jóvenes", subtitle = "Microsimulación CASEN–CENSO") +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# ISAPRE - Adultos y Mayores
mapa_isapre_adultos <- ggplot() +
  geom_sf(data = filter(zonas_salud_edad, grupo_edad %in% c("Adultos (30–59)", "Mayores (60+)")),
          aes(fill = ptje_isapre), color = NA) +
  geom_sf(data = comunas_sf, fill = NA, color = "grey30", size = 0.1) +
  geom_text(data = centroides_comunas, aes(X, Y, label = COMUNA), size = 1.5, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = colores_gradiente, name = "% ISAPRE", limits = c(0,100), na.value = "grey90") +
  labs(title = "ISAPRE - Adultos y Mayores", subtitle = "Microsimulación CASEN–CENSO") +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

######################################
# 9. Mostrar los 4 mapas juntos
######################################
plot_grid(mapa_fonasa_jovenes)
plot_grid(mapa_fonasa_adultos)
plot_grid(mapa_isapre_jovenes)
plot_grid(mapa_isapre_adultos)

plot_grid(mapa_fonasa_jovenes,mapa_fonasa_adultos, ncol = 2 )
plot_grid(mapa_isapre_jovenes,mapa_isapre_adultos, ncol = 2 )


