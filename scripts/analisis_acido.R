rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, wordcloud, tm, ggrepel, treemapify, BAMMtools)

#### Se fijan parámetros del área de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

#######################################################################################################

setwd("/Users/asanchezm/Documents/ataques/datos/")


##### importar archivos  ##


base<- read_excel("acido/AMENAZAS O ATAQUES CON ÁCIDO HISTÓRICO AL 31 DE DIC DE 2024.xlsx")

######## limpiar base #######------
#conocer base: variables
colnames(base)
summary(base)

##### ---------limpieza - limpiar colnames -------- ########

names(base)

colnames(base) <-  c(
  "EUV", "narracion", "fecha_recepcion","anual_registro" , "edo_usuario", "dependencia_recepcion", "edo_registro", "fecha_hechos", "nacionalidad", "pais_nacimiento", "edo_nacimiento", "vivienda", "edo_domicilio", "mun_domicilio", "dom_localidad", "edad", "estado_civil","embarazada", "escolaridad", "agresion_en_domicilio", "lugar", "descripcion_lugar", "conocimiento_de_autoridad", "v_economica", "v_fisica","v_patrimonial", "v_psicologica", "v_sexual", "v_otro", "modalidad_violencia"  , "num_agresores", "vinculo_con_victima", "detalle_vinculo_con_victima", "aux0" 
)


base <-  as_tibble(base)

#verificar unicidad de categorías en variable
table <- unique(base$edo_domicilio)
table <- as.data.frame(table)

tableviv <- unique(base$vivienda)
tableviv <- as.data.frame(tableviv)

tableesc <- unique(base$escolaridad)
tableesc <- as.data.frame(tableesc)

tabledad <- unique(base$edad)
tabledad <- as.data.frame(tabledad)

#corregir edad***** consultar 
base$edad <-  as.numeric(base$edad)

base$edad[is.na(base$edad)]<-0

base$anual_registro <-  as.numeric(base$anual_registro)
base$anual_registro[is.na(base$anual_registro)]<-0

tablevio <- unique(base$modalidad_violencia)
tablevio <- as.data.frame(tablevio)

###### NOTA: se necesita unificar variable de Dependencia de recepcion********* #######
tabledep <- unique(base$dependencia_recepcion)
tabledep <- as.data.frame(tabledep)

######## --- 0. acercamiento - análisis exploratorio ----- #####
# descriptivos basicos
base_est_mod <- base %>% 
  select(edo_domicilio, modalidad_violencia) %>% 
  group_by(edo_domicilio, modalidad_violencia) %>% 
  count()


base_est_esc <- base %>% 
  select(escolaridad, modalidad_violencia) %>% 
  group_by(escolaridad, modalidad_violencia) %>% 
  count()


# sumar por tipo de violencia
# 
names(base)
mod_vio <- colnames(base)[24:29]

base_a <- base %>% 
  group_by(anual_registro) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)

base_esc <- base %>% 
  group_by(anual_registro,modalidad_violencia, escolaridad) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)

base_edo <- base %>% 
  group_by(anual_registro, edo_domicilio) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)


base_mod <- base %>% 
  group_by(anual_registro, modalidad_violencia) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)


base_auh <- base %>% 
  group_by(anual_registro, modalidad_violencia, conocimiento_de_autoridad) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)

base_au <- base %>% 
  group_by(modalidad_violencia, conocimiento_de_autoridad) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)

base_vinc <- base %>% 
  group_by(modalidad_violencia, detalle_vinculo_con_victima) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)



##### ---------- 1.1. importar poblacion para sacar tasas de cambio--------------- ######


pob_conapo <- read_xlsx("/Users/asanchezm/Documents/pob/conapo/0_Pob_Inicio_1950_2070.xlsx")

names(pob_conapo)
names(pob_conapo)[2] <- "anual"
names(pob_conapo)[3] <- "Entidad"
names(pob_conapo)[4] <- "Clave_Ent"
names(pob_conapo)[7] <- "pobtot"

## agregar totales y por sexo
base_pob <- aggregate(pobtot ~ anual + Clave_Ent + Entidad, data = pob_conapo, sum)

base_pob_sex <- aggregate(pobtot ~ anual + SEXO + Clave_Ent + Entidad, data = pob_conapo, sum)

base_pob_sex <- base_pob_sex %>%
  pivot_wider(names_from = SEXO, values_from = pobtot)

names(base_pob_sex)

base_pob <- left_join(base_pob, base_pob_sex, by = c("Clave_Ent", "anual", "Entidad"))

base_pob_24 <- base_pob %>% 
  filter(anual == 2025)

##### renombrar estados ######
base <- base %>% 
  mutate(Entidad = ifelse(edo_domicilio == "Estado de México", "México",
                          ifelse(edo_domicilio == "Coahuila de Zaragoza", "Coahuila",
                                 ifelse(edo_domicilio == "Veracruz Ignacio de la Llave", "Veracruz", 
                                        ifelse(edo_domicilio == "Michoacán de Ocampo", "Michoacán", edo_domicilio)))))

base <- left_join(base, base_pob_24, by = c("Entidad"))

#check de na en match

prueba_na <- base %>%
  filter(is.na(pobtot))

base <- base %>% 
  mutate(EUV_F = map(strsplit(EUV, split = "-"), 1),
         reincidencia = map(strsplit(EUV, split = "-"), 2))


tot_vu <- n_distinct(base$EUV_F)

base$EUV_F <- as.character(base$EUV_F)

base$aux0 <- 1

lista_euv <- unique(base$EUV_F)

############################### VISUALIZACIONES REPORTE ##########################

##### ---------- 1.1. vars territoriales: distribucion por tiempo ---------------- ######
base <- base %>% 
  mutate(dif_tiempo = difftime(fecha_recepcion, fecha_hechos, units='days'))

# total de casos por año 
tot_hist <- aggregate(aux0 ~ ymd(fecha_hechos) , data = base, sum)
names(tot_hist)[1] <- "fecha_hechos_day"

# total de casos por año 
tot_anual <- aggregate(aux0 ~ year(fecha_hechos) , data = base, sum)
names(tot_anual)[1] <- "fecha_hechos_anual"

# total de casos por año y estado
tot_anual_edo <- aggregate(aux0 ~ year(fecha_hechos) + edo_domicilio , data = base, sum)
names(tot_anual_edo)[1] <- "fecha_hechos_anual"

# promedio de tiempo de denuncia entre hecho y recepcion en banco
prom_anual_edo <- aggregate(dif_tiempo ~ year(fecha_hechos) + edo_domicilio , data = base, mean)
names(prom_anual_edo)[1] <- "fecha_hechos_anual"
prom_anual_edo$dif_tiempo <- as.numeric(round(prom_anual_edo$dif_tiempo, 2))


prom_anual_edo$fecha_hechos_anual <- as.factor(prom_anual_edo$fecha_hechos_anual)


#### grafs edo


ggnac <- ggplot(data=tot_anual,aes(x=fecha_hechos_anual,y=aux0)) +
  geom_line() +
  xlab("Año")+ylab("Total de casos ~ histórico") +
  ggtitle("Total de casos - histórico ")

tot_anual$fecha_hechos_anual <- as.factor(tot_anual$fecha_hechos_anual)


## 1.1 graph 1 de tendencia histórica
ggnac_hist <- ggplot(tot_anual, aes(x=fecha_hechos_anual, y=aux0, label = aux0, size = aux0,  group=1)) +
  geom_line(colour='brown4', size =.4)+
  geom_point(colour='brown4')+
  xlab("")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(check_overlap = TRUE, vjust = -1, nudge_x = 0.05, size=3)+
  xlab("Total de casos por año ~ histórico")+ylab("Número de casos") +
  ggtitle("Total de casos por año ")

ggnac_hist

## grapg de diferencia entre tiempo de hecho y recepción
gg_edo_prom <- 
  ggplot(prom_anual_edo, aes(x = fecha_hechos_anual, y = dif_tiempo, size=dif_tiempo, label = paste(edo_domicilio, dif_tiempo, sep = "\n"))) +
  geom_point(aes( col = dif_tiempo), alpha = 0.5) +
  scale_size(range = c(5, 10)) + 
  scale_colour_gradientn(colours=c("yellow","red")) + theme_bw()+
  xlab("Años")+ylab("Diferencia de tiempo (fecha del hecho - fecha recepción) ~ en días") +
  ggtitle("Promedio de tiempo por estado ")

gg_edo_prom

##### ---------- 1.2. vars territoriales: distribucion por estado ---------------- ######

######## total de victimas unicas por estado - historico###################
vu_edo <- aggregate(aux0 ~ EUV_F + edo_domicilio , data = base, sum)

vu_edo$aux1 <- 1

vu_edo_filter <- aggregate(aux1 ~ edo_domicilio , data = vu_edo, sum)

names(vu_edo_filter)[2] <- "victimas"


#total de casos por estado
tot_edo <- aggregate(aux0 ~  edo_domicilio , data = base, sum)

names(tot_edo)[2] <- "casos"

totales_edo <- left_join(tot_edo, vu_edo_filter, by = "edo_domicilio")

totales_edo$reincidencia <- (totales_edo$casos - totales_edo$victimas)


totales_edo <- totales_edo %>% 
  pivot_longer(!edo_domicilio, names_to = "tipo", values_to = "totales")

##### graph 1.2. comparativo entre total victimas, casos e reincidencia historico

## graph de casos 
ggcomphist <-  qplot(
  x = totales,
  y = edo_domicilio,
  data = totales_edo,
  color = tipo ,
  xlab = "Total de casos vs víctimas ~ histórico ",
  ylab = "Estados"
) + geom_text_repel(label = totales_edo$totales)


ggcomphist

##### total de casos y victimas por año###############

vu_edo_a <- aggregate(aux0 ~ EUV_F + anual_registro + edo_domicilio , data = base, sum)

vu_edo_a$aux1 <- 1

#total de casos por estado por año

tot_edo_filter_a <- aggregate(aux0 ~  anual_registro + edo_domicilio, data = vu_edo_a, sum)
names(tot_edo_filter_a)[3] <- "casos"

#total de victimas por estado por año

vu_edo_filter_a <- aggregate(aux1 ~  anual_registro + edo_domicilio, data = vu_edo_a, sum)

names(vu_edo_filter_a)[3] <- "victimas"

totales_edo_anual <- left_join(tot_edo_filter_a, vu_edo_filter_a, by = c("edo_domicilio", "anual_registro"))



totales_edo_anual$prueba <- (totales_edo_anual$victimas== totales_edo_anual$casos)


totales_edo_anual <- totales_edo_anual %>% 
  pivot_longer(!c(edo_domicilio, anual_registro), names_to = "tipo", values_to = "totales")

ggcomp_2 <- ggplot(totales_edo_anual, aes(x = Year, y = GDP, colour =Region, group = Region)) +  geom_line() + geom_point()


ggcomp <- totales_edo %>% 
  ggplot(aes(x = totales, y = edo_domicilio, fill = tipo))+
  geom_col(position = "dodge") + 
  geom_label(aes(label = totales),
             nudge_x =-2.5,
             size = 2.5)  + theme_bw()+
  xlab("Total de casos vs víctimas ~ histórico")+ylab("Estados") +
  ggtitle("Comparativa entre total de casos y total de víctimas por estado ")


ggcomp


#generar ranking pra categorizacion de total de casos
jenks_edo <- getJenksBreaks(totales_edo_anual$casos, 5)

jenks_edo <-  as.data.frame(jenks_edo)

jenks_edo <- tibble::rowid_to_column(jenks_edo, "ID")

jenks_edo <- jenks_edo %>% 
  mutate(ID = paste0("v_", ID))

jenks_edo <- jenks_edo %>% 
  pivot_wider(names_from = ID, values_from = jenks_edo)

# 
# tot_edo <- totales_edo %>% 
#   mutate(categoria = ifelse(total_casos_edo<=jenks_edo$v_1, "Muy baja",
#                             ifelse(jenks_edo$v_1<total_casos_edo & total_casos_edo<=jenks_edo$v_2, "Baja",
#                             ifelse(jenks_edo$v_2<total_casos_edo & total_casos_edo<=jenks_edo$v_3, "Media",
#                             ifelse(jenks_edo$v_3<total_casos_edo & total_casos_edo<=jenks_edo$v_4, "Alta",
#                             ifelse(jenks_edo$v_4<total_casos_edo & total_casos_edo<=jenks_edo$v_5, "Muy alta", "Sin def"))))))


tot_edo <- totales_edo %>% 
  mutate(categoria = ifelse(total_casos_edo<=jenks_edo$v_1, "1",
                            ifelse(jenks_edo$v_1<total_casos_edo & total_casos_edo<=jenks_edo$v_2, "2",
                                   ifelse(jenks_edo$v_2<total_casos_edo & total_casos_edo<=jenks_edo$v_3, "3",
                                          ifelse(jenks_edo$v_3<total_casos_edo & total_casos_edo<=jenks_edo$v_4, "4",
                                                 ifelse(jenks_edo$v_4<total_casos_edo & total_casos_edo<=jenks_edo$v_5, "5", "Sin def"))))))


totales_edo$categoria <- factor(totales_edo$categoria)

# total de victimas unicas por municipio
vu_mun <- aggregate(aux0 ~ EUV_F + edo_domicilio + mun_domicilio, data = base, sum)

#total de casos por municipio
tot_mun <- aggregate(aux0 ~  edo_domicilio + mun_domicilio, data = base, sum)

names(tot_mun)[3] <- "total_casos_mun"

tot_edo_mun <- left_join(totales_edo, tot_mun, by = c("edo_domicilio"))



## graf mun

gg_mun <- 
  ggplot(tot_edo_mun, aes(x = total_casos_mun, y = edo_domicilio, size = total_casos_mun)) +
  geom_point(aes( col = total_casos_edo), alpha = 0.5) +
  
  geom_label(aes(label = mun_domicilio),
             data = tot_edo_mun %>% filter(total_casos_mun > 10),
             nudge_x =-1.5,
             nudge_y =1.2,
             size = 2) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Total de casos por municipio y estado ")

gg_mun


#### grafs edo

gg_edo <- 
  ggplot(tot_edo, aes(x = total_casos_edo, y = edo_domicilio, size=total_casos_edo, label = total_casos_edo)) +
  geom_point(aes( col = total_casos_edo), alpha = 0.5) +
  scale_size(range = c(5, 10)) + 
  scale_colour_gradientn(colours=c("yellow","red")) + theme_bw()+
  xlab("Total de casos ~ histórico")+ylab("Estados") +
  ggtitle("Total de casos por estado ")

gg_edo

graph_edos <-  ggplot(tot_edo, aes(area = porcentaje, fill = categoria, label = paste(edo_domicilio, total_casos_edo, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "OrRd")

graph_edos




##### ---------- 1.3. vars territoriales: distribucion por tipo de violencia ---------------- ######

base_vinc <- base %>% 
  group_by(modalidad_violencia, detalle_vinculo_con_victima) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)



base_edo_v <- base %>% 
  group_by(modalidad_violencia, edo_domicilio) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)



base_anual_v <- base %>% 
  group_by(modalidad_violencia, year(fecha_hechos)) %>% 
  summarise_at(mod_vio[1:6], sum, na.rm = TRUE)

names(base_anual_v)[2] <- "fecha_hechos_anual"


base_anual_v <- base_anual_v %>% 
  pivot_longer(!c(modalidad_violencia, fecha_hechos_anual), names_to = "tipo_violencia", values_to = "count")


#Tipo de violencia: familiar

base_vf <- base_anual_v %>% 
  filter(modalidad_violencia == "1 - Familiar")
