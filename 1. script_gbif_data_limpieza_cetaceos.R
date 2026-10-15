#####################################################################
############### MANUAL DE BIOGEOGRAF?A ##############################
####### Practica de adquision de datos biologicos de especies #######
#####Elaborada por: David A. Prieto-Torres (FES-Iztacala, UNAM)######

#############################################################################################
###########PASO I: INTALACION Y ACTIVACION DE PAQUETES NECESARIOS############################
#############################################################################################
##esto solo es necesario instalarlo la primera vez ya luego NO, puede irse a la siguiente seccion.
install.packages ("rgbif")
install.packages ("TeachingDemos")
install.packages ("dismo")
install.packages ("biomod2")
install.packages ("sp")
install.packages ("raster")
install.packages ("usdm")
install.packages ("ENMeval")
install.packages ("foreign")
install.packages ("spocc")
install.packages ("corrplot")
install.packages ("XML")
install.packages ("dplyr")
install.packages ("reshape")
install.packages ("CoordinateCleaner")
install.packages ("sf")
install.packages ("tidyr")
install.packages ("terra")
install.packages("maps")
install.packages("rnaturalearth")
install.packages("robis")


getOption("timeout")
options(timeout = 120)  # Cambia el tiempo de espera a 120 segundos

#########################
##Activar los paquetes###
#########################
library(rgbif)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(raster)
library(usdm)
library(foreign)
library(spocc)
library(corrplot)
library(XML)
library(dplyr)
library(reshape)
library(CoordinateCleaner)
library(sf)
library(terra)
library(maps)
library(rnaturalearth)
library(robis)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


#############################################################################################
###########DESCARGA DE DATOS ESPECIE DE INTERES##############################################
#############################################################################################
rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo

#1.Descarga de datos de la especie "GBIF"
species1_gbif <- gbif("Balaenoptera", "ricei*", geo=FALSE)##permite vincularse a la pagina GBIF y descargar datos, para ello es necesario escribir el nombre de la especie separando el g?nero del epiteto (el asterisco es para no descartar datos por las "subspecies").
data1_gbif <- subset(species1_gbif, !is.na(lon) & !is.na(lat))#quita todos los datos que no tienen coordenadas geograficas
data2_gbif <- subset(data1_gbif, !is.na(year))#quita todos los datos que no tienen informacion del ano (fecha)
data3_gbif <- subset(data2_gbif, !is.na(adm1))#quita todos los datos que no tienen informacion del estado/localidad de colecta

##1.1 Carga de datos descargados manualmente desde GBIF
# Reemplaza 'D:/Arlette/Balaenoptera_ricei/Balaenoptera_ricei.csv' con la ruta real de tu archivo
species1_gbif <- read.csv("D:/Arlette/Balaenoptera_ricei/Balaenoptera_ricei.csv", header = TRUE, sep = ",")
data1_gbif <- subset(species1_gbif, !is.na(lon) & !is.na(lat))#quita todos los datos que no tienen coordenadas geograficas
data2_gbif <- subset(data1_gbif, !is.na(year))#quita todos los datos que no tienen informacion del ano (fecha)
data3_gbif <- subset(data2_gbif, !is.na(catalogNumber))#quita todos los datos que no tienen informacion del estado/localidad de colecta

##OBIS##
species1_obis <- occurrence(scientificname = "Balaenoptera ricei")
data1_obis <- subset(species1_obis, !is.na(decimalLongitude) & !is.na(decimalLatitude))#quita todos los datos que no tienen coordenadas geograficas
data2_obis <- subset(data1_obis, !is.na(year))#quita todos los datos que no tienen informacion del ano (fecha)
data3_obis <- subset(data2_obis, !is.na(catalogNumber))#quita todos los datos que no tienen informacion del estado/localidad de colecta


#2.Crear un archivo en formato ".CSV" con la informacion basica necesaria para cada registro, incluyendo: "codigo GBIF", "institucion de procedencia", "numero de catalogo/coleccion", "taxonomia" y "coordenadas geograficas", asi como "ano", "pais" y "region" de colecta. NOTA: para este ejercicio solo se estan seleccionando 15 columnas de las 154 disponibles en GBIF, no obstante esto puede modificarse de acuerdo a la necesidades y preferencias de cada investigador
gbif_obis_ID <- c(data3_gbif$gbifID, data3_obis$occurrenceID)###seleciona la informaci?n sobre el "c?digo GBIF" que corresponde a cada dato.
institutionCode <- c(data3_gbif$institutionCode, data3_obis$institutionCode)###seleciona la informaci?n sobre el "instituci?n de procedencia" para cada dato.
catalogNumber <- c(data3_gbif$catalogNumber, data3_obis$catalogNumber)###seleciona la informaci?n sobre el "n?mero de cat?logo" de cada dato.
kingdom <- c(data3_gbif$kingdom, data3_obis$kingdom)###seleciona la informaci?n tax?nomica del "Reino" de mi especie de inter?s.
phylum <- c(data3_gbif$phylum, data3_obis$phylum)###seleciona la informaci?n tax?nomica del "Phylum" de mi especie de inter?s.
class <- c(data3_gbif$class, data3_obis$class)###seleciona la informaci?n tax?nomica del "Clase" de mi especie de inter?s.
order <- c(data3_gbif$order, data3_obis$order)###seleciona la informaci?n tax?nomica del "Orden" de mi especie de inter?s.
family <- c(data3_gbif$family, data3_obis$family)###seleciona la informaci?n tax?nomica del "Familia" de mi especie de inter?s.
genus <- c(data3_gbif$genus, data3_obis$genus)###seleciona la informaci?n tax?nomica del "G?nero" de mi especie de inter?s.
species <- c(data3_gbif$species, data3_obis$species)###seleciona la informaci?n tax?nomica del "Especie" de mi especie de inter?s.
lat <- c(data3_gbif$lat, data3_obis$decimalLatitude)###seleciona la informaci?n sobre la "Latitud" que corresponde a la localidad de muestreo de cada dato.
lon <- c(data3_gbif$lon, data3_obis$decimalLongitude)###seleciona la informaci?n sobre la "Longitud" que corresponde a la localidad de muestreo de cada dato.
year <- c(data3_gbif$year, data3_obis$year)###seleciona la informaci?n sobre el "A?O" de colecta o muestreo de cada dato.
country <- c(data3_gbif$country, data3_obis$country)###seleciona la informaci?n sobre el "pa?s" de la localidad de muestreo de cada dato.

##Unir toda la informacion en un solo archivo: Aqui se unen los datos de GBIF Y OBIS
data_cleaned1 <-data.frame(gbif_obis_ID,institutionCode,catalogNumber,kingdom,phylum,class,order,family,genus,species,lon,lat,year,country)

View(data_cleaned1)##permite visualizar la tabla de datos descargada, conformada por 4006 observaciones con 15 variables de informacion para cada registro segun el estandar de datos de Darwin Core.

##pero podemos seguir limpiando la base de datos, por ejemplo:
data_cleaned2 <- subset(data_cleaned1, !is.na(institutionCode))#quita todos los datos que no tienen informacion de la institucion de procedencia
data_cleaned1 <- subset(data_cleaned2, !is.na(data_cleaned2$catalogNumber))#quita todos los datos que no tienen informacion del numero de catalogo o registro
View(data_cleaned1)##permite visualizar la tabla de datos descargada. 

# Quitar filas duplicadas considerando todas las columnas
datos_sin_dup <- data_cleaned1 %>% distinct(catalogNumber, lat, lon, .keep_all = TRUE)

##Guardar el archivo creado para los datos datos descargados y filtrados
setwd("D:/Arlette/archivos_csv_originales_descarga/")##permite indicar el directorio dentro del computador donde queremos guardar el archivo ".csv" contentivo de la informacion
write.csv(data_cleaned1, file = "Balaenoptera_ricei_original.csv", row.names = F) ###es el comando utilizado para guardar el archivo de nombre "Amazona_xantholora" con extensi?n ".csv"

rm(list = setdiff(ls(), "data_cleaned1"))
###Tabla concatenada de GBIF mas otros museos####
data_cleaned2 <- read.csv2("D:/Arlette/archivos_csv_originales_descarga/Balaenoptera_ricei_original.csv", sep = ",", header = TRUE) 
data_cleaned2$lat <- as.numeric(data_cleaned2$lat)##vuelve los datos de latitud en numeros
data_cleaned2$lon <- as.numeric(data_cleaned2$lon)##vuelve los datos de longitud en numeros

########################################################################################
########### LIMPIEZA ESPACIO-TEMPORAL DE LOS DATOS #####################################
########################################################################################
#1.Separaci?n de los datos de acuerdo a la informaci?n temporal
data4 <- subset(data_cleaned2, year < 2000)###selecciona los datos del pasado (antes del 2000)
data5 <- subset(data_cleaned2, year > 2000 & year < 2010)###selecciona los datos del 2001 al 2009
data6 <- subset(data_cleaned2, year > 2009)###selecciona los datos del 2010 al presente

#############################################################
#Limpieza espacial de los datos: eliminiacion de duplicados##
####las siguientes 4 lineas es una funcion que establece como usar los datos para hacer la limpieza (no modificar)###
clean_dup <- function(data,longitude,latitude,threshold=0.0){  data <- data[!is.na(data[,longitude]),]
dat_sp <- SpatialPointsDataFrame(data[,c(longitude ,latitude)],data)
dat_sp1 <- remove.duplicates(dat_sp, zero = threshold)
return(dat_sp1@data)}

###limpiar los dos set de datos creados: "datos6 = 1970-2000" y "data5 = 2001-2020"
data7 <- clean_dup(data4,longitude = "lon",latitude = "lat",threshold = 0.0083333)###datos antes del 2000 (61 datos)
data8 <- clean_dup(data5,longitude = "lon",latitude = "lat",threshold = 0.0083333)###datos 2000-2009  (n = 47 datos)
data9 <- clean_dup(data6,longitude = "lon",latitude = "lat",threshold = 0.0083333)###datos despues del 2009 (n = 915 datos)


####################################################################################
#4. Calcular la distancia buffer para seleccionar datos del 2001-2020###############
## es decir la distancia minima entre datos para saber cuales si son buenos o no####
####################################################################################
###convertir los archivos de datos en formato vectorial de puntos espaciales para el calculo de distancias geograficas
points_occ1999 <- SpatialPointsDataFrame(data7[,11:12],data7)#convertir a un archivo shp de puntos 1970-2000 (es decir los viejos)
points_occ2000 <- SpatialPointsDataFrame(data8[,11:12],data8)##convertir a un archivo shp de puntos 2001-2020 (es decir los nuevos)
points_occ2010 <- SpatialPointsDataFrame(data9[,11:12],data9)##convertir a un archivo shp de puntos 2001-2020 (es decir los nuevos)

points_occ1999_2 <- st_as_sf (points_occ1999)##convierte el archivo de puntos en el formato que se necesita.
points_occ2000_2 <- st_as_sf (points_occ2000)##convierte el archivo de puntos en el formato que se necesita.
points_occ2010_2 <- st_as_sf (points_occ2010)##convierte el archivo de puntos en el formato que se necesita.

###Calcular distancia promedio y el filtro buffer a aplicar entre los puntos
DistanciaPuntos <- st_distance(points_occ2010_2, points_occ2010_2)##calcula la distancia promedio de los puntos de 2010-presente
buffer = (mean(DistanciaPuntos)*2)/100 #me da el valor m?ximo observado entre los datos para calcular la distancia que se usara como area BUFFER para seleccionar cuales datos entraran en nuestra matriz.

###aplicar la distancia buffer entre los puntos de 1970-2000 para seleccionar los datos del 2001-2020
buffer_points <- st_buffer(points_occ2010_2, dist = buffer)###aplica la distancia buffer a los puntos del 2010-presente

####visualizaci?n de los datos por separado
windows()##crea una ventana nueva para guardar mapas con mejor resolucion
world <- vect(ne_countries(scale = "medium", returnclass = "sf"))##crea un modelo de mapas para el mundo


install.packages("rnaturalearthdata")

plot(world, xlim = c(-99, 152), ylim = c(-20,65), col = "lightblue", border = "gray40")###dibujar un mapa estableciendo un acercamiento al area geografica de distribucion de los datos obtenidos para la especie. 
points(data4$lon, data4$lat, col = "red", pch=20, cex= 2)###colocar los puntos de ocurrencia de mi especie en el mapa
points(data5$lon, data5$lat, col = "blue", pch=20, cex= 2)###colocar los puntos de ocurrencia de mi especie en el mapa
points(data6$lon, data6$lat, col = "green", pch=20, cex= 2)###colocar los puntos de ocurrencia de mi especie en el mapa
plot(buffer_points, add = TRUE, border = "blue", lwd = 2, col = NA)###grafica el area buffer que fue definida para los datos.

#Seleccionar los puntos antes del 2000 que entran en el filtro buffer aplicado
data_poly_all_1999 <- st_intersects(points_occ1999_2,buffer_points, fn = NULL)###selecciona los puntos que si entran en el analisis
en_poligono_index_1999 <- which(!is.na(data_poly_all_1999))
p_en_poligono_1999 <- points_occ1999_2[buffer_points,]

#Seleccionar los puntos del 2000-2009 que entran en el filtro buffer aplicado
data_poly_all_2000 <- st_intersects(points_occ2000_2,buffer_points, fn = NULL)###selecciona los puntos que si entran en el analisis
en_poligono_index_2000 <- which(!is.na(data_poly_all_2000))
p_en_poligono_2000 <- points_occ2000_2[buffer_points,]

selected_data_1999 <- data.frame(p_en_poligono_1999)### para guardar el archivo de puntos
selected_data_2000 <- data.frame(p_en_poligono_2000)### para guardar el archivo de puntos

##################################################################
############# LIMPIEZA ECOLOGICA DE LOS DATOS ####################
##################################################################
#1.Verificar informacion ambiental en mis localidades de ocurrencia
##2000-2010
setwd("D:/Arlette/capas_climaticas_limpieza/presente_2000-2010/")## directorio donde estan las capas climaticas del presente 
pca_path_2000 <- list.files(".",pattern = "*.tif$",full.names = T)###crea el stack de las variables ambientales
capas_2000 <- stack(pca_path_2000)
windows()##crea una ventana nueva para guardar mapas con mejor resolucion
plot(capas_2000$temperatura)

##2010-2020
setwd("D:/Arlette/capas_climaticas_limpieza/presente_2010-2020/")## directorio donde estan las capas climaticas del presente 
pca_path_2020 <- list.files(".",pattern = "*.tif$",full.names = T)###crea el stack de las variables ambientales
capas_2020 <- stack(pca_path_2020)
windows()##crea una ventana nueva para guardar mapas con mejor resolucion
plot(capas_2020$temperatura)


###Para los siguientes pasos, trabajaremos solo con las 3 columnas que nos interesan: nombre de la especie, longitud y latitud 
##datos antes del 2000
species<-points_occ1999_2$species ## seleccionar la columna del nombre de la especie para los datos 1970-2000
lat<-as.numeric(points_occ1999_2$lat)## seleccionar la columna del nombre de la latitud para los datos 1970-2000
lon<-as.numeric(points_occ1999_2$lon)## seleccionar la columna del nombre de la longitud para los datos 1970-2000
datos_1999<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos 1970-2000

##datos antes del 2000-2009
species<-points_occ2000_2$species ## seleccionar la columna del nombre de la especie para los datos 1970-2000
lat<-as.numeric(points_occ2000_2$lat)## seleccionar la columna del nombre de la latitud para los datos 1970-2000
lon<-as.numeric(points_occ2000_2$lon)## seleccionar la columna del nombre de la longitud para los datos 1970-2000
datos_2000<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos 1970-2000

##datos antes del 2010-presente
species<-points_occ2010_2$species ## seleccionar la columna del nombre de la especie para los datos 1970-2000
lat<-as.numeric(points_occ2010_2$lat)## seleccionar la columna del nombre de la latitud para los datos 1970-2000
lon<-as.numeric(points_occ2010_2$lon)## seleccionar la columna del nombre de la longitud para los datos 1970-2000
datos_2010<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos 1970-2000


###convertir los archivos de datos en formato vectorial de puntos espaciales para el calculo de distancias geogr?ficas
datos_1999_1 <- SpatialPointsDataFrame(datos_1999[,2:3],datos_1999)#convertir a un archivo shp de puntos 1970-2000
datos_2000_1 <- SpatialPointsDataFrame(datos_2000[,2:3],datos_2000)#convertir a un archivo shp de puntos 1970-2000
datos_2010_1 <- SpatialPointsDataFrame(datos_2010[,2:3],datos_2010)#convertir a un archivo shp de puntos 1970-2000


#2. extraemos los valores ambientales para esas localidades que si coinciden (2010-2020)
presencias_clima2010 <- data.frame(raster::extract(capas_2020,datos_2010_1[,2:3]))###extrae los valores climaticos para cada uno de los puntos de presencia
presencias_clima2010_2 <-data.frame(datos_2010_1,presencias_clima2010)##crear una tabla de datos con los valores climaticos para mis registros del 2000-2010
presencias_clima2010_3 <- na.omit(presencias_clima2010_2)## omite mis datos de presencia sin valores ambientales.
View(presencias_clima2010_3)

#3. Calcular los intervalos de referencias para los valores ambientales de las variables m?s importantes en la distribuci?n de mi especie. Aqu? proponemos realizar este calculo a trav?s del valor promedio +/- 2 veces la desviaci?n estandar encontrada en los datos de 1970-2001. Estos valores ser?n utilizados para realizar un filtrado ambiental de los datos del 2001-2020.
windows()##crea una ventana nueva para guardar mapas con mejor resolucion
hist(presencias_clima2010_3$batimetria)
bat_min= (mean(presencias_clima2010_3$batimetria)) - ((sd(presencias_clima2010_3$batimetria))*2)#valor minimo definido
bat_max= (mean(presencias_clima2010_3$batimetria)) + ((sd(presencias_clima2010_3$batimetria))*2)#valor m?ximo definido

tem_min= (mean(presencias_clima2010_3$temperatura)) - ((sd(presencias_clima2010_3$temperatura))*2)#valor m?nimo definido 
tem_max= (mean(presencias_clima2010_3$temperatura)) + ((sd(presencias_clima2010_3$temperatura))*2)#valor m?ximo definido

O2_min= (mean(presencias_clima2010_3$O2)) - ((sd(presencias_clima2010_3$O2))*2)#valor m?nimo definido
O2_max= (mean(presencias_clima2010_3$O2)) + ((sd(presencias_clima2010_3$O2))*2)#valor m?ximo definido

chl_mar_min= (mean(presencias_clima2010_3$chl_mar)) - ((sd(presencias_clima2010_3$chl_mar))*2)#valor m?nimo definido para bio15
chl_mar_max= (mean(presencias_clima2010_3$chl_mar)) + ((sd(presencias_clima2010_3$chl_mar))*2)#valor m?ximo definido para bio15


#2. extraemos los valores ambientales para esas localidades que NO coinciden (antes del 2010)
##2000-2010
presencias_clima2000 <- data.frame(raster::extract(capas_2000,datos_2000_1[,2:3]))###extrae los valores climaticos para cada uno de los puntos de presencia
presencias_clima2000_2 <-data.frame(datos_2000_1,presencias_clima2000)##crear una tabla de datos con los valores climaticos para mis registros del 1970-2000
presencias_clima2000_3 <- na.omit(presencias_clima2000_2)## omite mis datos de presencia sin valores ambientales.
View(presencias_clima2000_3)

##antes del 2000
presencias_clima1999 <- data.frame(raster::extract(capas_2000,datos_1999_1[,2:3]))###extrae los valores climaticos para cada uno de los puntos de presencia
presencias_clima1999_2 <-data.frame(datos_1999_1,presencias_clima1999)##crear una tabla de datos con los valores climaticos para mis registros del 1970-2000
presencias_clima1999_3 <- na.omit(presencias_clima1999_2)## omite mis datos de presencia sin valores ambientales.
View(presencias_clima1999_3)

##Limpiar los datos que no coinciden en tiempo:
#2000-2010
data2000_tem <- subset(presencias_clima2000_3, temperatura > tem_min & temperatura < tem_max)#elimina datos con temperatura 
data2000_O2 <- subset(data2000_tem, O2 > O2_min & O2 < O2_max)#elimina datos con oxigeno
data2000_chl_mar <- subset(data2000_O2, chl_mar > chl_mar_min & chl_mar < chl_mar_max)#elimina datos con clorofila

#menos del 2000
data1999_tem <- subset(presencias_clima1999_3, temperatura > tem_min & temperatura < tem_max)#elimina datos con temperatura 
data1999_O2 <- subset(data1999_tem, O2 > O2_min & O2 < O2_max)#elimina datos con oxigeno
data1999_chl_mar <- subset(data1999_O2, chl_mar > chl_mar_min & chl_mar < chl_mar_max)#elimina datos con clorofila

#####################################
###individualizar los set de datos###
#####################################
data_1999_fin <- data1999_chl_mar###corresponde al archivo final de datos para antes del 2000
data_2000_fin <- data1999_chl_mar###corresponde al archivo final de datos del 2000-2010
data_2010_fin <- presencias_clima2010_3###corresponde al archivo final de datos del 2010-present
data_buffer1999 <- selected_data_1999###corresponde al archivo final de datos para los a?os entre 2001 y el presente
data_buffer2000 <- selected_data_2000###corresponde al archivo final de datos para los a?os entre 2001 y el presente

#5. Armar el ?nico archivo .CSV con los registros validados y depurados de la especie 
species<- c(data_1999_fin$species, data_2000_fin$species, data_2010_fin$species, data_buffer1999$species, data_buffer2000$species)##selecciona y une las columnas "species" de ambos archivos
lat<-as.numeric(c(data_1999_fin$lat, data_2000_fin$lat, data_2010_fin$lat, data_buffer1999$lat, data_buffer2000$lat))##selecciona y une las columnas "latitud" de ambos archivos
lon<-as.numeric(c(data_1999_fin$lon, data_2000_fin$lon, data_2010_fin$lon, data_buffer1999$lon, data_buffer2000$lon))##selecciona y une las columnas "longitud" de ambos archivos

##Unir toda la informacion en un solo archivo
data_cleaned2 <-data.frame(species,lat,lon)###crea el archivo dataframe concatenado.
data_cleaned2$species_name <- "Balaenoptera_ricei"#crea una columna/variable colocando el nombre de la especie
data_cleaned3<-select(data_cleaned2, -(species))#elimina la columna "species" ya que solo contiene valores "1"
data_cleaned4 <- data_cleaned3%>%select(species_name,lon,lat)##ordena las columnas en species_name, lon y lat

View(data_cleaned4)##permite visualizar la tabla de datos concatenados: 248 observaciones con 3 variables de informaci?n

#6. Quitar datos duplicados (mismas coordenadas geograficas)
data10 <- clean_dup(data_cleaned4,longitude = "lon",latitude = "lat",threshold = 0.05)### datos espacialmente limpios

#7.####visualizaci?n de los datos finales
windows()
plot(world, xlim = c(-99, 152), ylim = c(-20,65), col = "lightblue", border = "gray40")###me permite ver un mapa con una escala mas cercana a la distribucion de mis datos
points(data10$lon, data10$lat, col = "red", pch=20, cex= 2)###colocar los puntos en el mapa

##Guardar el archivo en el PC: datos limpios
setwd("D:/Arlette/datos_descargados_limpios/")##carpeta donde guardar el archivo final de los datos
write.csv(data10, file = "Balaenoptera_ricei_limpios.csv", row.names = F)###es el comando utilizado para guardar el archivo de nombre "Balaenoptera_ricei" con extension ".csv" En total quedaron 915 datos

#guardemos el archivo en formato shapefile
shape_sf <- st_as_sf(data10, coords = c("lon", "lat"), crs = 4326)#convierte el archivo en formato shapefile de puntos
setwd("D:/Arlette/formatos_shapefile_points/")#directorio donde guardare el archivo
st_write(shape_sf, "Balaenoptera_ricei.shp", driver = "ESRI Shapefile")#guarda el archivo en formato shapefile de puntos con el nombre entre comillas

##Fin




