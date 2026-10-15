### 1. Cargar paquetes necesarios
library(sf)
library(sp)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(KnowBR)
library(vegan)
library(rnaturalearth)
library(bivariatemaps)
library(patchwork)

### 2. Definir directorio de trabajo
setwd("D:/proyectos_sigs/arlette_cetaceas_caribbean")

### 3. Cargar shapefile del área de estudio
area <- st_read("MarineRealmsShapeFile/cuadricula_fin.shp")
area_clean <- area[!st_is_empty(area), ]
area_clean <- st_make_valid(area_clean)
st_crs(area_clean) <- 4326

area_sp <- as_Spatial(area_clean)
plot(area_clean)


### 4. Cargar datos de ocurrencias
datos_raw <- read.csv("datos_total2.csv")
pts <- st_as_sf(datos_raw, coords = c("lon", "lat"), crs = 4326)

datos_filtrados_sf <- st_filter(pts, area_clean)
coords <- st_coordinates(datos_filtrados_sf)

#seleccionar datos con geometría adecuada y confirmando que si tengan los datos de coordenadas dentro del area de estudio
datos_filtrados <- datos_filtrados_sf %>%
  st_drop_geometry() %>%
  mutate(
    Longitude = coords[, 1],
    Latitude  = coords[, 2])

##filtrar los datos para sacar la estadistica por especie no importa no tener años
resumen_especies <- datos_filtrados %>%
  filter(!is.na(species), !is.na(year)) %>%
  group_by(species) %>%
  summarise(n_records = n(),
            first_year = min(year),
            last_year  = max(year),
            .groups = "drop")

##filtrar los datos para sacar la estadistica por especie no importa que si tengan años
resumen_especies2 <- datos_filtrados %>%
  filter(!is.na(species)) %>%
  group_by(species) %>%
  summarise(n_records = n(),
            first_year = min(year),
            last_year  = max(year),
            .groups = "drop")




### 5. Crear Formato A para KnowBR
datos_KBR <- datos_filtrados %>%
  select(Species = species,
         Longitude,
         Latitude) %>%  mutate(Count = 1)

write.csv(datos_KBR, "datos_formato_KnowBR2.csv", row.names = FALSE)


### 6. Análisis temporal
registros_por_anio <- datos_filtrados %>%
  group_by(year) %>% summarise(n_registros = n()) %>%
  mutate(acum_registros = cumsum(n_registros))

primer_registro <- datos_filtrados %>%
  group_by(species) %>% summarise(first_year = min(year))

especies_nuevas_por_anio <- primer_registro %>%
  group_by(first_year) %>% summarise(n_especies_nuevas = n()) %>%
  mutate(acum_especies = cumsum(n_especies_nuevas))


#generar la grafica de acumulacion historica de registros para el periodo de estudio
windows()
p_time <- ggplot(registros_por_anio, aes(x = year, y = n_registros)) +
  geom_col(fill = "steelblue", alpha = 1) +
  geom_line(aes(y = acum_registros / 10), color = "red", size = 2) +
  scale_y_continuous(
    name = "Records per year",
    sec.axis = sec_axis(~.*10, name = "Cumulative records")
  ) +
  labs(x = "Year", y = "Records per year") +
  theme_minimal(base_size = 20, base_family = "Times New Roman")
p_time



### 7. Ejecutar KnowBR con shapefile propio (grid)
data(adworld)

KnowBPolygon(data = datos_KBR,format = "A",  shape = area_sp,  shapenames = "id",
             curve = "Rational",  estimator = 1,  cutoff = 1,  cutoffCompleteness = 0,
             cutoffSlope = 1,  save = "CSV",  jpg = FALSE)


### 8. Leer archivo Estimators.csv (para richness/completeness/etc.)
estim <- read_delim("Estimators.csv", delim = ";",
                    col_names = TRUE, trim_ws = TRUE,
                    locale = locale(decimal_mark = ","))

grid_sf <- area_clean %>%
  left_join(estim, by = c("id" = "Area"))


### 9. Mapas base
continente <- st_read("MarineRealmsShapeFile/continent_lims.shp")
paises     <- st_read("MarineRealmsShapeFile/paises_latino.shp")
st_crs(continente) <- 4326
st_crs(paises) <- 4326

area_boundary <- st_union(area_clean) %>% st_cast("MULTILINESTRING")


### 10A.Records por celda con st_join (misma lógica que edad)esto garantiza mismas celdas que el mapa de edad
pts_sf <- st_as_sf(datos_filtrados, coords = c("Longitude", "Latitude"), crs = 4326)
pts_join <- st_join(pts_sf, area_clean["id"])# Asignar ID de celda a cada registro (igual que edad)

# Contar registros por celda (Records reales por celda)
records_per_cell <- pts_join %>%
  st_drop_geometry() %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  summarise(Records_cell = n(), .groups = "drop")

grid_records <- area_clean %>%  left_join(records_per_cell, by = "id")# Unir al grid (nuevo objeto para mapa de Records)
grid_records_data <- grid_records %>% filter(!is.na(Records_cell) & Records_cell > 0)
grid_records_zero <- grid_records %>% filter(is.na(Records_cell) | Records_cell == 0)


###10B. Mapa final de "Number of records" (por celda, consistente con edad)
p_records <- ggplot() +
  geom_sf(data = continente, fill = "grey85", color = "grey60", size = 0.3) +
  geom_sf(data = paises, fill = "grey98", color = "grey70", size = 0.2) +
  geom_sf(data = area_boundary, color = "black", size = 0.6, fill = NA) +
  geom_sf(data = grid_records_zero, fill = NA, color = NA, alpha = 0) +
  geom_sf(data = grid_records_data, aes(fill = Records_cell), color = NA) +
  geom_sf(data = grid_records_data, fill = NA, color = "black", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#C8FFFFFF","#64FFFFFF","#00FFFFFF","#64FF64FF",
                "#C8FF00FF","#FFFF00FF","#FFC800FF","#FF6400FF","#FF0000FF"),
    name = "Number of records"
  ) +
  coord_sf(xlim = c(-100, -45), ylim = c(4, 30), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    plot.title  = element_text(face = "bold", family = "Times New Roman"),
    axis.title  = element_text(face = "bold", family = "Times New Roman"),
    axis.text   = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman")
  )

#graficar el mapa de registros por celdas
windows()
p_records + plot_layout(widths = c(4, 1))



### 11. Análisis Complementariedad (de KnowBR, opcional)
grid_data <- grid_sf %>% filter(Records > 0)
p50_records <- median(grid_data$Records, na.rm = TRUE)
p50_richness <- median(grid_data$Observed.richness, na.rm = TRUE)

mean_completeness <- mean(grid_data$Completeness, na.rm = TRUE)

summary(grid_data$Records)
sd(grid_data$Records)

summary(grid_data$Observed.richness)
sd(grid_data$Observed.richness)

summary(grid_data$Completeness)
summary(grid_data$Slope)

well_sampled <- grid_data %>%
  filter(Completeness >= 80,  Slope <= 0.02,Records >= 20)

nrow(well_sampled)
nrow(grid_data)
(nrow(well_sampled) / nrow(grid_data)) * 100


### 12. Mapa de edad (igual al tuyo, usando el MISMO pts_join)
datos_filtrados <- datos_filtrados %>%
  mutate(Age = 2025 - year)

# Rehacer pts_sf con Age incluido (o reutilizar pts_join pero más claro así)
pts_sf_age <- st_as_sf(datos_filtrados, coords = c("Longitude", "Latitude"), crs = 4326)
pts_join_age <- st_join(pts_sf_age, area_clean["id"])

age_per_cell <- pts_join_age %>%
  st_drop_geometry() %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  summarise(median_age = median(Age, na.rm = TRUE), n_records  = n(),  .groups = "drop")

grid_age <- area_clean %>%  left_join(age_per_cell, by = "id")
grid_age_data <- grid_age %>% filter(!is.na(median_age))
grid_age_zero <- grid_age %>% filter(is.na(median_age))

summary(grid_age$median_age)
prop_mayor_30 <- mean(grid_age$median_age > 26, na.rm = TRUE)
prop_mayor_30

p_age <- ggplot() +
  geom_sf(data = continente, fill = "grey85", color = "grey60", size = 0.3) +
  geom_sf(data = paises, fill = "grey98", color = "grey70", size = 0.2) +
  geom_sf(data = area_boundary, color = "black", size = 0.5, fill = NA) +
  geom_sf(data = grid_age_zero, fill = NA, color = NA, alpha = 0) +
  geom_sf(data = grid_age_data, aes(fill = median_age), color = NA) +
  geom_sf(data = grid_age_data, fill = NA, color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    name = "Median age\n(years)"
  ) +
  coord_sf(xlim = c(-100, -45), ylim = c(4, 30), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    plot.title  = element_text(face = "bold", family = "Times New Roman"),
    axis.title  = element_text(face = "bold", family = "Times New Roman"),
    axis.text   = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"))

#graficar el mapa resulante de muestreos (edades)
windows()
p_age + plot_layout(widths = c(4, 1))


### 13. Chequeo rápido: mismas celdas coloreadas?
cat("Celdas con records (st_join):", nrow(grid_records_data), "\n")
cat("Celdas con edad:", nrow(grid_age_data), "\n")



### 14. MAPA BIVARIADO (Records_cell vs median_age)
###4 categorías por eje (cuartiles) + paleta 4x4 como tu escala
# Unir ambas métricas en una sola capa (por id)
grid_bivar <- area_clean %>%
  left_join(records_per_cell, by = "id") %>%   # Records_cell
  left_join(age_per_cell,      by = "id")      # median_age

#Misma máscara: solo celdas con datos en ambas variables
grid_bivar_data <- grid_bivar %>%
  filter(!is.na(Records_cell), Records_cell > 0,
         !is.na(median_age))

# Clasificación en 4 categorías por eje (cuartiles): x = Research effort (Records_cell) y = Sampling age (median_age)
grid_bivar_data <- grid_bivar_data %>%
  mutate(rec_q = ntile(Records_cell, 4),  age_q = 5 - ntile(median_age, 4))

#Clase bivariada "age-rec" (4x4)
grid_bivar_data <- grid_bivar_data %>%
  mutate(bivar_class = factor(paste0(age_q, "-", rec_q),
      levels = as.vector(outer(1:4, 1:4, FUN = function(a, r) paste0(a, "-", r)))))

#Paleta bivariada 4x4 (mezcla bilineal de 4 esquinas): Ajustada a la escala de amarillo–naranja–rojo y rosa–morado
bivar_pal_4x4 <- function(
    bl = "#F3E9D5",  # bajo age, bajo effort (crema claro)
    br = "#F2E41B",  # bajo age, alto effort (amarillo)
    tl = "#F4B0C9",  # alto age, bajo effort (rosa)
    tr = "#6B0015"   # alto age, alto effort (morado/rojo oscuro)
){
  ramp2 <- function(c1, c2, n) grDevices::colorRampPalette(c(c1, c2))(n)
  
  left_col  <- ramp2(bl, tl, 4)  # x=1, y 1..4
  right_col <- ramp2(br, tr, 4)  # x=4, y 1..4
  
  mat <- matrix(NA_character_, nrow = 4, ncol = 4)
  for (y in 1:4) {
    mat[y, ] <- ramp2(left_col[y], right_col[y], 4)
  }
  
  labs <- as.vector(outer(1:4, 1:4, FUN = function(a, r) paste0(a, "-", r)))
  cols <- as.vector(t(mat))
  names(cols) <- labs
  cols
}

pal_4x4 <- bivar_pal_4x4()


#Leyenda bivariada tipo “cuadro” (4x4) con flechas (como tu ejemplo)
legend_df <- expand.grid(rec_q = 1:4, age_q = 1:4) %>%
  mutate(bivar_class = factor(paste0(age_q, "-", rec_q),
      levels = levels(grid_bivar_data$bivar_class)))

p_legend <- ggplot(legend_df, aes(x = rec_q, y = age_q, fill = bivar_class)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_manual(values = pal_4x4, guide = "none") +
  coord_equal(expand = FALSE) +
  # marco
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 4.5),
            fill = NA, color = "black", linewidth = 0.9, inherit.aes = FALSE) +
  # flechas
  annotate("segment", x = 0.5, xend = 4.6, y = 0.35, yend = 0.35,
           arrow = arrow(length = unit(0.20, "cm")), linewidth = 0.9) +
  annotate("segment", x = 0.35, xend = 0.35, y = 0.5, yend = 4.6,
           arrow = arrow(length = unit(0.20, "cm")), linewidth = 0.9) +
  # etiquetas (puedes cambiarlas a español si quieres)
  annotate("text", x = 2.5, y = 0.05, label = "Research effort (records)",
           family = "Times New Roman", fontface = "bold", size = 3.3, vjust = 0) +
  annotate("text", x = 0.05, y = 2.5, label = "Sampling age (years)",
           family = "Times New Roman", fontface = "bold", size = 3.3, angle = 90, hjust = 0) +
  theme_void(base_family = "Times New Roman") +
  theme(plot.margin = margin(5, 5, 5, 5))


#Mapa bivariado
p_bivar <- ggplot() +
  geom_sf(data = continente, fill = "grey85", color = "grey60", size = 0.3) +
  geom_sf(data = paises, fill = "grey98", color = "grey70", size = 0.2) +
  geom_sf(data = area_boundary, color = "black", size = 0.5, fill = NA) +
  geom_sf(data = grid_bivar_data, aes(fill = bivar_class), color = NA) +
  geom_sf(data = grid_bivar_data, fill = NA, color = "black", size = 0.1) +
  scale_fill_manual(values = pal_4x4) +
  coord_sf(xlim = c(-100, -45), ylim = c(4, 30), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "none",
    axis.title  = element_text(face = "bold", family = "Times New Roman"),
    axis.text   = element_text(family = "Times New Roman"))

#Combinar mapa + leyenda (patchwork)
windows()
p_bivar + p_legend + plot_layout(widths = c(4, 1))





