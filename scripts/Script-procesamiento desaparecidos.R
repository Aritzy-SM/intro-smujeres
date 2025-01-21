rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven, BAMMtools, plyr)

#para carto

p_load (foreign, rgdal, sp, leaflet, htmltools, cleangeo, leaflet.extras, RColorBrewer, mapview, webshot, colorRamps, )

#para el html

p_load(htmlwidgets)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

# abrir base agregada sesnsp y población
setwd("/Users/HP/Documents/Rizika/IPGC/Desaparecidos")

datos <- read_csv("cenapi.csv")

#### Filtrar datos por fecha de interés 2000-2020 ####

datos_2006 <- datos %>% 
  filter(year(`FECHA EVENTO`)>=2006)

datos_2006 <- datos_2006 %>% 
  select(2, 4, 6:12, 14, 23:24, 36, 38:44)

colnames(datos_2006) <- c("fecha_reporte", "fecha_evento", "estado", "clave_estado", "municipio", "clave_municipio", "nacionalidad", "estatus_migratorio", "sexo", "edad", "ocupacion", "rel_grupos_delictivos", "fecha_localizacion", "vivo_muerto", "pos_causa_desaparicion", "condicion_encontrado", "estado_loc", "clave_estado_loc", "municipio_loc", "clave_municipio_loc")

datos_2006 <- datos_2006 %>% 
  mutate(clave_estado = str_pad(datos_2006$clave_estado, 2, pad = 0))

datos_2006 <- datos_2006 %>% 
  mutate(clave_municipio = str_pad(datos_2006$clave_municipio, 3, pad = 0))

datos_2006 <- datos_2006 %>% 
  filter(clave_estado > "00")

datos_2006 <- datos_2006 %>% 
  filter(clave_municipio > "000")

datos_2006 <- datos_2006 %>% 
  mutate(cve_mun = paste0(clave_estado, clave_municipio))

datos_2006 <- datos_2006 %>% 
  select(1:5, 21, 7:20)

### Descriptivos####

datos_tipo <- datos_2006 %>%
  group_by(vivo_muerto) %>% 
  count()

datos_anual <- datos_2006 %>%
  group_by(year(fecha_evento)) %>% 
  count()

datos_estado <- datos_2006 %>%
  group_by(estado) %>% 
  count()

datos_estado_anual <- datos_2006 %>%
  group_by(estado, year(fecha_evento)) %>% 
  count()

##### Análisis de modalidades de violencia sobre cuerpos de desaparecidos #####

datos_violencia <- datos_2006 %>% 
  filter(vivo_muerto== "MUERTO")

lista_metodos <- as.data.frame(unique(datos_violencia$condicion_encontrado))

metodos <- datos_violencia %>% 
  mutate(metodo = ifelse(condicion_encontrado == "nan", "No especificado", 
                         ifelse(condicion_encontrado == "SIN DATO", "No especificado",
                                condicion_encontrado)))

metodos$metodo <- tolower(metodos$metodo)

lista_metodos_1 <- as.data.frame(unique(metodos$metodo))

write.csv(lista_metodos_1, file="lista_metodos_violencia.csv", fileEncoding ="UTF-8")

metodos <- metodos %>% 
  select(3:6, 21) %>% 
  group_by(estado, metodo) %>% 
  count()

metodos <- metodos %>% 
  spread(metodo, freq) %>% 
  group_by(estado, municipio)

metodos[is.na(metodos)]<-0

correlacion <- metodos %>% 
  ungroup() %>% 
  select(5:34)

correlacion$cve_mun <- as.numeric(correlacion$cve_mun)

correlacion <- cor(correlacion)

write.xlsx (correlacion, file="corr_metodos_violencia.xlsx")

#### Filtrar por variables de interés ####

datos_sub <- datos_2006 %>% 
  select(2:6, 14)

datos_sub$anual <- year(datos_sub$fecha_evento)

datos_sub <- datos_sub %>% 
  select(2:7)

##### Encontrados vivos #####
datos_vivos <- datos_sub %>% 
  subset(vivo_muerto=="VIVO")

datos_vivos <- datos_vivos %>% 
  group_by(anual, estado, clave_estado, municipio, cve_mun) %>% 
  count()

names(datos_vivos)[7] <- "total_vivos_anual"

### spread
datos_vivos_tot <- datos_vivos %>%
  spread(key = anual, value = total_vivos_anual, fill = 0) %>% 
  group_by(cve_mun)

datos_vivos_tot <- datos_vivos_tot %>% 
  select(1:4, 6:18)

colnames(datos_vivos_tot)[5:17] <- paste0("total_", colnames(datos_vivos_tot)[5:17])

## Sacar datos mínimos y máximos por año ##

datos_vivos <- datos_vivos %>% 
  select(6, 1:4, 7)

max_vivos <- datos_vivos %>% 
  group_by(anual) %>% 
  slice(which.max(total_vivos_anual)) 

names(max_vivos)[6] <- "max_vivos_anual"

min_vivos <- datos_vivos %>% 
  group_by(anual) %>% 
  slice(which.min(total_vivos_anual)) 

names(min_vivos)[6] <- "min_vivos_anual"

max_comp_vivos <- max_vivos %>% 
  select(1, 6)

min_comp_vivos <- min_vivos %>% 
  select (1,6)

min_max_comp_vivos <- left_join(min_comp_vivos, max_comp_vivos, by = "anual")

datos_vivos <- left_join(datos_vivos, min_max_comp_vivos, by = "anual")

datos_vivos$indicador_concentracion <- round(datos_vivos$total_vivos_anual/datos_vivos$max_vivos_anual, 2)

#### Encontrados muertos ####

datos_muertos <- datos_sub %>% 
  subset(vivo_muerto=="MUERTO")

datos_muertos <- datos_muertos %>% 
  group_by(anual, estado, clave_estado, municipio, cve_mun) %>% 
  count()

names(datos_muertos)[7] <- "total_muertos_anual"

### spread
datos_muertos_tot <- datos_muertos %>%
  spread(key = anual, value = total_muertos_anual, fill = 0) %>% 
  group_by(cve_mun)

datos_muertos_tot <- datos_muertos_tot %>% 
  select(1:4, 6:18)

colnames(datos_muertos_tot)[5:17] <- paste0("total_", colnames(datos_muertos_tot)[5:17])

#### datos 2006-2012 

datos_muertos_pres_1 <- datos_muertos %>% 
  select(1:6) %>% 
  filter(anual<2013)

datos_muertos_pres_1 <- datos_muertos_pres_1 %>% 
  select(2:6) %>% 
  group_by(estado, clave_estado, municipio, cve_mun) %>% 
  summarise_all(list(sum))

names(datos_muertos_pres_1)[5] <- "total_muertos_2006_2012"

#### datos 2013-2018

datos_muertos_pres_2 <- datos_muertos %>% 
  select(1:6) %>% 
  filter(anual>=2013)

datos_muertos_pres_2 <- datos_muertos_pres_2 %>% 
  select(2:6) %>% 
  group_by(estado, clave_estado, municipio, cve_mun) %>% 
  summarise_all(list(sum))

names(datos_muertos_pres_2)[5] <- "total_muertos_2013_2018"

datos_muertos_pres <- left_join(datos_muertos_pres_1, datos_muertos_pres_2, by = c("estado", "clave_estado", "municipio", "cve_mun"))

datos_muertos_pres$total_muertos_2006_2012[is.na(datos_muertos_pres$total_muertos_2006_2012)] <- 0
datos_muertos_pres$total_muertos_2013_2018[is.na(datos_muertos_pres$total_muertos_2013_2018)] <- 0

datos_muertos_pres <- datos_muertos_pres %>% 
  mutate(tasa_cambio = round(ifelse(total_muertos_2006_2012==0 & total_muertos_2013_2018!=0, 100,
                                    ((total_muertos_2013_2018 - total_muertos_2006_2012)/total_muertos_2006_2012)*100),2))

## Sacar datos mínimos y máximos por año ##

datos_muertos <- datos_muertos %>% 
  select(6, 1:4, 7)

max_muertos <- datos_muertos %>% 
  group_by(anual) %>% 
  slice(which.max(total_muertos_anual))

names(max_muertos)[6] <- "max_muertos_anual"

min_muertos <- datos_muertos %>% 
  group_by(anual) %>% 
  slice(which.min(total_muertos_anual))

names(min_muertos)[6] <- "min_muertos_anual"

max_comp_muertos <- max_muertos %>% 
  select(1, 6)

min_comp_muertos <- min_muertos %>% 
  select (1,6)

min_max_comp_muertos <- left_join(min_comp_muertos, max_comp_muertos, by = "anual")

datos_muertos <- left_join(datos_muertos, min_max_comp_muertos, by = "anual")

datos_muertos$indicador_concentracion <- round(datos_muertos$total_muertos_anual/datos_muertos$max_muertos_anual, 2)

#### datos totales 

datos_muertos_total <- datos_muertos %>% 
  select(2:6) %>% 
  group_by(estado, clave_estado, municipio, cve_mun) %>% 
  summarise_all(list(sum))

names(datos_muertos_total)[5] <- "total_desaparecidos_muertos"

## Sacar datos mínimos y máximos por total 2006-2018 ##

max_muertos_total <- datos_muertos_total %>% 
  slice(which.max(total_desaparecidos_muertos))

names(max_muertos_total)[5] <- "max_muertos_total"

min_muertos_total <- datos_muertos_total %>% 
  slice(which.min(total_desaparecidos_muertos))

names(min_muertos_total)[5] <- "min_muertos_total_anual"

datos_muertos_total$min <- 1
datos_muertos_total$max <- 353

datos_muertos_total$indicador_concentracion <- round(datos_muertos_total$total_desaparecidos_muertos/datos_muertos_total$max,2)

##### Sin encontrar #####

datos_sin_e <- datos_sub %>% 
  subset(vivo_muerto =="AUN SIN LOCALIZAR")

datos_sin_e <- datos_sin_e %>% 
  group_by(anual, estado, clave_estado, municipio, cve_mun) %>% 
  count() 

names(datos_sin_e)[7] <- "total_desaparecidos_anual"

## spread

datos_desaparecidos_tot <- datos_sin_e %>%
  spread(key = anual, value = total_desaparecidos_anual, fill = 0) %>% 
  group_by(cve_mun)

datos_desaparecidos_tot <- datos_desaparecidos_tot %>% 
  select(1:4, 6:18)

colnames(datos_desaparecidos_tot)[5:17] <- paste0("total_", colnames(datos_desaparecidos_tot)[5:17])

#### datos 2006-2012 

datos_sin_e_pres_1 <- datos_sin_e %>% 
  select(1:6) %>% 
  filter(anual<2013)

datos_sin_e_pres_1 <- datos_sin_e_pres_1 %>% 
  select(2:6) %>% 
  group_by(estado, clave_estado, municipio, cve_mun) %>% 
  summarise_all(list(sum))

names(datos_sin_e_pres_1)[5] <- "total_sin_e_2006_2012"

#### datos 2013-2018

datos_sin_e_pres_2 <- datos_sin_e %>% 
  select(1:6) %>% 
  filter(anual>=2013)

datos_sin_e_pres_2 <- datos_sin_e_pres_2 %>% 
  select(2:6) %>% 
  group_by(estado, clave_estado, municipio, cve_mun) %>% 
  summarise_all(list(sum))

names(datos_sin_e_pres_2)[5] <- "total_sin_e_2013_2018"

datos_sin_e_pres <- left_join(datos_sin_e_pres_1, datos_sin_e_pres_2, by = c("estado", "clave_estado", "municipio", "cve_mun"))

datos_sin_e_pres$total_sin_e_2006_2012[is.na(datos_sin_e_pres$total_sin_e_2006_2012)] <- 0
datos_sin_e_pres$total_sin_e_2013_2018[is.na(datos_sin_e_pres$total_sin_e_2013_2018)] <- 0

## Sacar datos mínimos y máximos por año ##

datos_sin_e <- datos_sin_e %>% 
  select(6, 1:4, 7)

max_sin_e <- datos_sin_e %>% 
  group_by(anual) %>% 
  slice(which.max(total_desaparecidos_anual))

names(max_sin_e)[6] <- "max_sin_e_anual"

min_sin_e <- datos_sin_e %>% 
  group_by(anual) %>% 
  slice(which.min(total_desaparecidos_anual))

names(min_sin_e)[6] <- "min_sin_e_anual"

max_comp_sin_e <- max_sin_e %>% 
  select(1, 6)

min_comp_sin_e <- min_sin_e %>% 
  select (1,6)

min_max_comp_sin_e <- left_join(min_comp_sin_e, max_comp_sin_e, by = "anual")

datos_sin_e <- left_join(datos_sin_e, min_max_comp_sin_e, by = "anual")

datos_sin_e$indicador_concentracion <- round(datos_sin_e$total_desaparecidos_anual/datos_sin_e$max_sin_e_anual,2)

#### datos totales 

datos_sin_e_total <- datos_sin_e %>% 
  select(2:6) %>% 
  group_by(estado, clave_estado, municipio, cve_mun) %>% 
  summarise_all(list(sum))

names(datos_sin_e_total)[5] <- "total_desaparecidos"

## Sacar datos mínimos y máximos por total 2006-2018 ##

max_sin_e_total <- datos_sin_e_total %>% 
  slice(which.max(total_desaparecidos))

names(max_sin_e_total)[5] <- "max_sin_e_total"

min_sin_e_total <- datos_sin_e_total %>% 
  slice(which.min(total_desaparecidos))

names(min_sin_e_total)[5] <- "min_sin_e_total_anual"

datos_sin_e_total$min <- 1
datos_sin_e_total$max <- 1437

datos_sin_e_total$indicador_concentracion <- round(datos_sin_e_total$total_desaparecidos/datos_sin_e_total$max,2)

#### Categorización ####

### Jenks

jenks_vivos <- ddply(datos_vivos, .(anual), function(x) getJenksBreaks(x$indicador_concentracion, 5))

jenks_muertos <- ddply(datos_muertos, .(anual), function(x) getJenksBreaks(x$indicador_concentracion, 3))

jenks_sin_e <- ddply(datos_sin_e, .(anual), function(x) getJenksBreaks(x$indicador_concentracion, 4))


#### jenks para categoria presidencia

jenks_muertos_pres_2006 <- getJenksBreaks(datos_muertos_pres$total_muertos_2006_2012, 5)

jenks_muertos_pres_2006 <- as.data.frame(jenks_muertos_pres)

jenks_muertos_pres_2013 <- getJenksBreaks(datos_muertos_pres$total_muertos_2013_2018, 5)

jenks_muertos_pres_2013 <- as.data.frame(jenks_muertos_pres_2013)

#### jenks para categoria 2006-2018

jenks_muertos_totales <- getJenksBreaks(datos_muertos_total$total_desaparecidos_muertos, 6)

jenks_muertos_totales <- as.data.frame(jenks_muertos_totales)

jenks_sin_e_totales <- getJenksBreaks(datos_sin_e_total$total_desaparecidos, 6)

jenks_sin_e_totales <- as.data.frame(jenks_sin_e_totales)

##### jenks totales para carto ###

j_vivos <- ddply(datos_vivos, .(anual), function(x) getJenksBreaks(x$total_vivos_anual, 5))

j_muertos <- ddply(datos_muertos, .(anual), function(x) getJenksBreaks(x$total_muertos_anual, 3))

j_sin_e <- ddply(datos_sin_e, .(anual), function(x) getJenksBreaks(x$total_desaparecidos_anual, 4))

### unimos bases para categorizar por año

datos_vivos <- left_join(datos_vivos, jenks_vivos, by = "anual")

datos_muertos <- left_join(datos_muertos, jenks_muertos, by = "anual")

datos_sin_e <- left_join(datos_sin_e, jenks_sin_e, by = "anual")

### unimos bases para categorizar por totales

datos_muertos_total <- left_join(datos_muertos_total, jenks_muertos_totales)

datos_sin_e <- left_join(datos_sin_e, jenks_sin_e, by = "anual")

### Ahora hay que colocar categoría por indicador

#### para vivos ####

datos_vivos$categoria <- case_when(datos_vivos$indicador_concentracion<=datos_vivos$V5 & datos_vivos$indicador_concentracion>datos_vivos$V4 ~ "Concentración alta de desaparecidos vivos", 
                                               datos_vivos$indicador_concentracion<=datos_vivos$V4 & datos_vivos$indicador_concentracion>datos_vivos$V3 ~ "Concentración media de desaparecidos vivos", 
                                               datos_vivos$indicador_concentracion<=datos_vivos$V3 & datos_vivos$indicador_concentracion>=datos_vivos$V2 ~ "Concentración baja de desaparecidos vivos", datos_vivos$indicador_concentracion<datos_vivos$V2 ~ "Concentración muy baja de desaparecidos vivos" )

datos_vivos <- datos_vivos %>% 
  select(1:6, 9, 15)

## spread para indicador

datos_vivos_i <- datos_vivos %>% 
  select(1:5, 7)

datos_vivos_i <- datos_vivos_i %>% 
  spread(key = anual, value = indicador_concentracion, fill = 0) %>% 
  group_by(cve_mun)

colnames(datos_vivos_i)[5:17] <- paste0("indicador_", colnames(datos_vivos_i)[5:17])

## spread para categoría

datos_vivos_c <- datos_vivos %>% 
  select(1:5, 8)

datos_vivos_c <- datos_vivos_c %>% 
  spread(key = anual, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(datos_vivos_c)[5:17] <- paste0("categoria_", colnames(datos_vivos_c)[5:17])

## unir tablas

datos_vivos_fin <- Reduce(function(x,y) merge(x=x, y=y, by = c("estado", "clave_estado", "municipio", "cve_mun")), list( datos_vivos_tot, datos_vivos_i, datos_vivos_c))

#### para muertos ####

datos_muertos$categoria <- case_when(datos_muertos$indicador_concentracion<=datos_muertos$V3 & datos_muertos$indicador_concentracion>datos_muertos$V2 ~ "Concentración alta de desaparecidos muertos", 
                                   datos_muertos$indicador_concentracion<=datos_muertos$V2 & datos_muertos$indicador_concentracion>=datos_muertos$V1 ~ "Concentración baja de desaparecidos muertos")

datos_muertos <- datos_muertos %>% 
  select(1:6, 9, 13)

## spread para indicador

datos_muertos_i <- datos_muertos %>% 
  select(1:5, 7)

datos_muertos_i <- datos_muertos_i %>% 
  spread(key = anual, value = indicador_concentracion, fill = 0) %>% 
  group_by(cve_mun)

colnames(datos_muertos_i)[5:17] <- paste0("indicador_", colnames(datos_muertos_i)[5:17])

## spread para categoría

datos_muertos_c <- datos_muertos %>% 
  select(1:5, 8)

datos_muertos_c <- datos_muertos_c %>% 
  spread(key = anual, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(datos_muertos_c)[5:17] <- paste0("categoria_", colnames(datos_muertos_c)[5:17])

## unir tablas

datos_muertos_fin <- Reduce(function(x,y) merge(x=x, y=y, by = c("estado", "clave_estado", "municipio", "cve_mun")), list( datos_muertos_tot, datos_muertos_i, datos_muertos_c))

#### para desaparecidos ####

datos_sin_e$categoria <- case_when(datos_sin_e$indicador_concentracion<=datos_sin_e$V4 & datos_sin_e$indicador_concentracion>datos_sin_e$V3 ~ "Concentración alta de desaparecidos sin encontrar", 
                                   datos_sin_e$indicador_concentracion<=datos_sin_e$V3 & datos_sin_e$indicador_concentracion>datos_sin_e$V2 ~ "Concentración media de desaparecidos sin encontrar", 
                                   datos_sin_e$indicador_concentracion<=datos_sin_e$V2 ~ "Concentración baja de desaparecidos sin encontrar")

datos_sin_e <- datos_sin_e %>% 
  select(1:6, 9, 14)

## spread para indicador

datos_sin_e_i <- datos_sin_e %>% 
  select(1:5, 7)

datos_sin_e_i <- datos_sin_e_i %>% 
  spread(key = anual, value = indicador_concentracion, fill = 0) %>% 
  group_by(cve_mun)

colnames(datos_sin_e_i)[5:17] <- paste0("indicador_", colnames(datos_sin_e_i)[5:17])

## spread para categoría

datos_sin_e_c <- datos_sin_e %>% 
  select(1:5, 8)

datos_sin_e_c <- datos_sin_e_c %>% 
  spread(key = anual, value = categoria, fill = 0) %>% 
  group_by(cve_mun)

colnames(datos_sin_e_c)[5:17] <- paste0("categoria_", colnames(datos_sin_e_c)[5:17])

## unir tablas

datos_sin_e_fin <- Reduce(function(x,y) merge(x=x, y=y, by = c("estado", "clave_estado", "municipio", "cve_mun")), list( datos_desaparecidos_tot, datos_sin_e_i, datos_sin_e_c))

#### Carto ####

cartografia_estatal <- readOGR(dsn="/Users/HP/Documents/Rizika/pob-geo/estados_mex", layer= "destdv250k_2gw")
estados <- spTransform(cartografia_estatal, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cartografia_municipal <- readOGR(dsn="/Users/HP/Documents/Rizika/pob-geo/mun_mex2018", layer= "mun_mex2018", encoding = "UTF-8")
municipios <- spTransform(cartografia_municipal, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#carto municipal con tabla de datos anual de desaparecidos muertos

municipios_p <- merge(municipios, datos_muertos_fin, by.x="CVEGEO", by.y="cve_mun")

municipios_p@data$total_2006[is.na(municipios_p@data$total_2006)] <- 0
municipios_p@data$total_2007[is.na(municipios_p@data$total_2007)] <- 0
municipios_p@data$total_2008[is.na(municipios_p@data$total_2008)] <- 0
municipios_p@data$total_2009[is.na(municipios_p@data$total_2009)] <- 0
municipios_p@data$total_2010[is.na(municipios_p@data$total_2010)] <- 0
municipios_p@data$total_2011[is.na(municipios_p@data$total_2011)] <- 0
municipios_p@data$total_2012[is.na(municipios_p@data$total_2012)] <- 0
municipios_p@data$total_2013[is.na(municipios_p@data$total_2013)] <- 0
municipios_p@data$total_2014[is.na(municipios_p@data$total_2014)] <- 0
municipios_p@data$total_2015[is.na(municipios_p@data$total_2015)] <- 0
municipios_p@data$total_2016[is.na(municipios_p@data$total_2016)] <- 0
municipios_p@data$total_2017[is.na(municipios_p@data$total_2017)] <- 0
municipios_p@data$total_2018[is.na(municipios_p@data$total_2018)] <- 0

#carto municipal con tabla de datos dado los años presidenciales de desaparecidos muertos

municipios_pres <- merge(municipios, datos_muertos_pres, by.x="CVEGEO", by.y="cve_mun")

municipios_pres@data$total_muertos_2006_2012[is.na(municipios_pres@data$total_muertos_2006_2012)] <- 0
municipios_pres@data$total_muertos_2013_2018[is.na(municipios_pres@data$total_muertos_2013_2018)] <- 0

#carto municipal con tabla de datos de datos totales de muertos y desaparecidos 2006-2018

municipios_muertos_tot <- merge(municipios, datos_muertos_total, by.x="CVEGEO", by.y="cve_mun")

municipios_muertos_tot@data$total_desaparecidos_muertos[is.na(municipios_muertos_tot@data$total_desaparecidos_muertos)] <- 0

municipios_sin_e_tot <- merge(municipios, datos_sin_e_total, by.x="CVEGEO", by.y="cve_mun")

municipios_sin_e_tot@data$total_desaparecidos[is.na(municipios_sin_e_tot@data$total_desaparecidos)] <- 0


##### mapa estatal base

mapa_mex <- leaflet(cartografia_estatal) %>% 
  addTiles(urlTemplate = "http://{s}.tile.osm.org/{z}/{x}/{y}.png") %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = cartografia_estatal,
              color = "grey",
              fillColor = "",
              group = "estados")

#### Para 2018 ####

bins_muertos_2018 <- c(0, 1, 3, 6, 9, 13)

pal_muertos_2018 <- colorBin("OrRd", domain = municipios_p$total_2018, 5, bins = bins_muertos_2018)


labels_2018 <- sprintf(
  "<strong>%s</strong><br/> Total de desaparecidos %d </sup>",
  municipios_p$NOMGEO, municipios_p$total_2018
) %>% lapply(htmltools::HTML)


mapa_2018 <- mapa_mex %>%
  addPolygons( data = municipios_p,
  fillColor = ~pal_muertos_2018(municipios_p$total_2018),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
   weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 1,
    bringToFront = TRUE),
  label = labels_2018,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto",
    group = "2018"))

mapa_2018 <- mapa_2018 %>% 
  addLegend(pal = pal_muertos_2018, values = ~datos_muertos_fin$total_muertos_2018, opacity = 0.7, title = "Total de personas desaparecidas \n reportadas muertas",
          position = "bottomright")
  
saveWidget(mapa_2018, "mapa_desaparecidos_muertos_2018.html", title = "Desaparecidos_2018", selfcontained = FALSE)

#### Para presidencia de calderon 2006-2012 ####

bins_muertos_calderon <- c(0, 1, 10, 43, 98, 160)

pal_muertos_calderon <- colorBin("OrRd", domain = municipios_pres$total_muertos_2006_2012, 5, bins = bins_muertos_calderon)

labels_calderon <- sprintf(
  "<strong>%s</strong><br/> Total de desaparecidos %d </sup>",
  municipios_pres$NOMGEO, municipios_pres$total_muertos_2006_2012
) %>% lapply(htmltools::HTML)


mapa_calderon <- mapa_mex %>%
  addPolygons( data = municipios_pres,
               fillColor = ~pal_muertos_calderon(municipios_pres$total_muertos_2006_2012),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 1,
                 bringToFront = TRUE),
               label = labels_calderon,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto",
                 group = "Presidencia de Felipe Calderón"))

mapa_calderon <- mapa_calderon %>% 
  addLegend(pal = pal_muertos_calderon, values = ~datos_muertos_pres$total_muertos_2006_2012, opacity = 0.7, title = "Total de personas desaparecidas \n reportadas muertas",
            position = "bottomright")

saveWidget(mapa_calderon, "mapa_desaparecidos_muertos_calderon.html", title = "Desaparecidos_calderon", selfcontained = FALSE)

#### Para presidencia de peña nieto 2013-2018 ####

bins_muertos_epn <- c(0, 1, 17, 46, 122, 193)

pal_muertos_epn <- colorBin("OrRd", domain = municipios_pres$total_muertos_2013_2018, 5, bins = bins_muertos_epn)

labels_epn <- sprintf(
  "<strong>%s</strong><br/> Total de desaparecidos %d </sup>",
  municipios_pres$NOMGEO, municipios_pres$total_muertos_2013_2018
) %>% lapply(htmltools::HTML)


mapa_epn <- mapa_mex %>%
  addPolygons( data = municipios_pres,
               fillColor = ~pal_muertos_epn(municipios_pres$total_muertos_2013_2018),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 1,
                 bringToFront = TRUE),
               label = labels_epn,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto",
                 group = "Presidencia de Enrique Peña Nieto"))

mapa_epn <- mapa_epn %>% 
  addLegend(pal = pal_muertos_epn, values = ~datos_muertos_pres$total_muertos_2013_2018, opacity = 0.7, title = "Total de personas desaparecidas \n reportadas muertas",
            position = "bottomright")

saveWidget(mapa_epn, "mapa_desaparecidos_muertos_epn.html", title = "Desaparecidos_epn", selfcontained = FALSE)

#### Para total de muertos ####

bins_muertos_tot <- c(0, 1, 12, 35, 86, 185, 353)

pal_muertos_tot <- colorBin("OrRd", domain = municipios_muertos_tot, 6, bins = bins_muertos_tot)

labels_muertos_tot <- sprintf(
  "<strong>%s</strong><br/> Total de desaparecidos %d </sup>",
  municipios_muertos_tot$NOMGEO, municipios_muertos_tot$total_desaparecidos_muertos
) %>% lapply(htmltools::HTML)


mapa_muertos_tot <- mapa_mex %>%
  addPolygons( data = municipios_muertos_tot,
               fillColor = ~pal_muertos_tot(municipios_muertos_tot$total_desaparecidos_muertos),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 1,
                 bringToFront = TRUE),
               label = labels_muertos_tot,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto",
                 group = "Total muertos"))

mapa_muertos_tot <- mapa_muertos_tot %>% 
  addLegend(pal = pal_muertos_tot, values = ~datos_muertos_total$total_desaparecidos_muertos, opacity = 0.7, title = "Total de personas desaparecidas \n reportadas muertas de 2006 a 2018",
            position = "bottomright")

saveWidget(mapa_muertos_tot, "mapa_desaparecidos_muertos_total.html", title = "Desaparecidos_muertos_total", selfcontained = FALSE)

#### Para total de desaparecidos ####

bins_sin_e_tot <- c(0, 1, 61, 204, 391, 616, 1437)

pal_sin_e_tot <- colorBin("OrRd", domain = municipios_sin_e_tot, 6, bins = bins_sin_e_tot)

labels_sin_e_tot <- sprintf(
  "<strong>%s</strong><br/> Total de desaparecidos %d </sup>",
  municipios_sin_e_tot$NOMGEO, municipios_sin_e_tot$total_desaparecidos
) %>% lapply(htmltools::HTML)


mapa_sin_e_tot <- mapa_mex %>%
  addPolygons( data = municipios_sin_e_tot,
               fillColor = ~pal_sin_e_tot(municipios_sin_e_tot$total_desaparecidos),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 1,
                 bringToFront = TRUE),
               label = labels_sin_e_tot,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto",
                 group = "Total desaparecidos"))

mapa_sin_e_tot <- mapa_sin_e_tot %>% 
  addLegend(pal = pal_sin_e_tot, values = ~datos_sin_e_total$total_desaparecidos, opacity = 0.7, title = "Total de personas desaparecidas de 2006 a 2018",
            position = "bottomright")

saveWidget(mapa_sin_e_tot, "mapa_desaparecidos_sin_e_total.html", title = "Desaparecidos_total", selfcontained = FALSE)


