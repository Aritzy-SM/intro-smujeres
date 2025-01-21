rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven, reshape2)

#### Se fijan parÃÂ¡metros del ÃÂ¡rea de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

setwd("/Users/patatas/Documents/aritzy/mujeres/")


#### 1. importar bases ####

incidencia  <- read_excel("incidencia delictiva/datos/Estatal-Delitos-2015-2024_dic2024.xlsx")

victimas <- read_xlsx("incidencia delictiva/datos/Estatal-V¡ctimas-2015-2024_dic2024.xlsx")

##### ---------- 1.1. importar poblacion para sacar tasas de cambio--------------- ######


pob_conapo <- read_xlsx("/Users/patatas/Documents/aritzy/mujeres/pob/conapo/0_Pob_Inicio_1950_2070.xlsx")

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

base_pob_sex <- base_pob_sex %>% 
  select(1:2, 4:5)

base_pob <- left_join(base_pob, base_pob_sex, by = c("Clave_Ent", "anual"))


#### 2.. procesar incidencia delictiva a nivel estatal ####
#### 
incidencia_objetivo <-  as_tibble(incidencia)

names(incidencia_objetivo)

delitos_todos <- as.data.frame(unique(incidencia_objetivo$`Tipo de delito`)) 


# seleccionar delitos de interes
#### plataforma riesgos###
# seleccionar delitos de interes
subtipos <- c("Homicidio doloso", 
              "Feminicidio",
              "Lesiones dolosas",
              "Aborto",
              "Secuestro",
              "Tráfico de menores",                                                           
              "Rapto" ,
              "Abuso sexual" ,
              "Acoso sexual" ,
              "Hostigamiento sexual" ,
              "Violación simple" ,
              "Violación equiparada" ,
              "Incesto",
              "Otros delitos que atentan contra la libertad y la seguridad sexual" ,
              "Violencia familiar", 
              "Violencia de género en todas sus modalidades distinta a la violencia familiar", 
              "Incumplimiento de obligaciones de asistencia familiar"
)
incidencia_objetivo <- incidencia_objetivo %>% 
  filter(`Tipo de delito` %in% subtipos)


delitos_subset <- as.data.frame(unique(incidencia_objetivo$`Tipo de delito`)) 

incidencia_objetivo <- incidencia_objetivo %>% 
  mutate(delito_ab = ifelse(`Tipo de delito`=="Homicidio doloso", "homicidio_dol",
                            ifelse(`Tipo de delito`=="Feminicidio", "feminicidio",
                                   ifelse(`Tipo de delito`=="Lesiones dolosas", "lesiones_dol",
                                          ifelse(`Tipo de delito`=="Aborto", "aborto",
                                                 ifelse(`Tipo de delito`=="Secuestro", "secuestro",
                                                        ifelse(`Tipo de delito`=="Tráfico de menores", "trafico_menores",
                                                               ifelse(`Tipo de delito`=="Rapto", "rapto",
                                                                      ifelse(`Tipo de delito`=="Abuso sexual", "abuso_sexual",
                                                                             ifelse(`Tipo de delito`=="Acoso sexual", "acoso_sexual",
                                                                                    ifelse(`Tipo de delito`=="Hostigamiento sexual", "hostigamiento_sexual",
                                                                                           ifelse(`Tipo de delito`=="Violación simple", "violacion_simple",
                                                                                                  ifelse(`Tipo de delito`=="Violación equiparada", "violacion_equiparada",
                                                                                                  ifelse(`Tipo de delito`=="Incesto", "incesto",
                                                                                                         ifelse(`Tipo de delito`=="Otros delitos que atentan contra la libertad y la seguridad sexual", "otros_seg_sex",
                                                                                                                ifelse(`Tipo de delito`=="Violencia familiar", "viol_familiar",
                                                                                                                       ifelse(`Tipo de delito`=="Violencia de género en todas sus modalidades distinta a la violencia familiar", "viol_genero",
                                                                                                                              ifelse(`Tipo de delito`=="Incumplimiento de obligaciones de asistencia familiar", "asistencia_familiar", "ERROR:sin definir"))))))))))))))))))


# sumar por delito estatal
names(incidencia_objetivo)
meses <- colnames(incidencia_objetivo)[8:19]
incidencia_objetivo <- incidencia_objetivo %>% 
  group_by(Año, Clave_Ent, Entidad, `Bien jurídico afectado`, 
           `Tipo de delito`, `Subtipo de delito`, delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


# sumar por delito nacional

delitos_indicadores_nac <- incidencia_objetivo %>% 
  group_by(Año, `Tipo de delito`) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


########  prueba para transformar a var de tiempo - BD tiempo ######
names(incidencia_objetivo)
incidencia_objetivo_t <-incidencia_objetivo  %>%
  pivot_longer(!c(Año, Clave_Ent, Entidad, `Bien jurídico afectado`, 
                  `Tipo de delito`, `Subtipo de delito`, delito_ab), names_to = "mes", values_to = "incidencia_mes")



incidencia_objetivo_t$day <- 1

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(mes=ifelse(mes== "Enero", 1,
                    ifelse(mes== "Febrero", 2,
                           ifelse(mes=="Marzo", 3,
                                  ifelse(mes =="Abril", 4,
                                         ifelse(mes == "Mayo", 5,
                                                ifelse(mes =="Junio", 6,
                                                       ifelse(mes=="Julio", 7,
                                                              ifelse(mes=="Agosto", 8, 
                                                                     ifelse(mes=="Septiembre", 9,
                                                                            ifelse(mes=="Octubre", 10,
                                                                                   ifelse(mes=="Noviembre", 11, 12))))))))))))



incidencia_objetivo_t <-  incidencia_objetivo_t %>%  
  mutate(datetime = make_datetime(Año, mes, day)) %>% 
  arrange(datetime)


####### generar una base para hacer comparativo de tasas de cambio anuales 

# agregar cols de mes y año as date
incidencia_objetivo_t <- incidencia_objetivo_t %>%
  mutate(mes = month(datetime),
         anual = year(datetime))

incidencia_objetivo_t$mes_anual <- format(as.Date(incidencia_objetivo_t$datetime), "%Y-%m")

#sumatoria por mes
incidencia_objetivo_t <- incidencia_objetivo_t %>%
  group_by(datetime, anual, mes_anual, Entidad,Clave_Ent, delito_ab ) %>%
  summarise(total_mensual = sum(incidencia_mes), .groups = 'drop')


# Calcular tasa de cambio de mes a mes
incidencia_objetivo_t <- incidencia_objetivo_t %>%
  arrange(datetime, anual, mes_anual) %>%
  group_by(Entidad,Clave_Ent, delito_ab) %>%
  mutate(prev_mes_total = lag(total_mensual),
         tasa_cambio_prev_mes = (total_mensual - prev_mes_total) / total_mensual * 100)

# Calculate percentage difference with the same month in the previous year
incidencia_objetivo_t <- incidencia_objetivo_t %>%
  mutate(prev_mes_anual = lag(total_mensual, 12),
         tasa_cambio_prev_year = (total_mensual - prev_mes_anual) / prev_mes_anual * 100)

## crear variables de tiempo

# mes<- format(as.Date(hoy), "%Y-%m")

mes_actual <- floor_date(Sys.Date() )

mes_pasado <- floor_date(Sys.Date() - months(1), "month")

yearBefore <- floor_date(Sys.Date() - years(1), "year")

biyearBefore <- floor_date(Sys.Date() - years(2), "year")


#### agregar por sumatorias anuales a 12 meses
#### 


df <- incidencia_objetivo_t %>%
  mutate( periodo = ifelse (as_date(incidencia_objetivo_t$datetime) >= as_date(yearBefore), "a1",
                               ifelse (as_date(yearBefore) < as_date(datetime) < as_date(biyearBefore), "a2", "sin comp" )))


a2 <- interval(yearBefore, biyearBefore)


df <- incidencia_objetivo_t %>%
  filter(periodo = ifelse (datetime %in% a1, "a1", "falta"))


df <- incidencia_objetivo_t %>%
mutate(period = case_when(incidencia_objetivo_t$datetime > yearBefore~ "a1",
                          datetime %in% a2 ~ "a2"))
       



prueba <- incidencia_objetivo_t %>%
  rowwise() %>% 
  replace(is.na(.), 0) %>%
  mutate(a1 = ifelse(Año==2019, sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre), 0),
         a2 = ifelse(Año==2020, sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre), 0))

#### agregar por es









# View the results
print(incidencia_objetivo_t)


###### unir datos de poblacion para sacar tasas#######
###### 
delitos_indicadores <- left_join(incidencia_objetivo_t, base_pob, by = c("Clave_Ent", "Entidad", "anual"))

names(delitos_indicadores)

delitos_indicadores <- delitos_indicadores %>% 
  mutate(m1_t_pob = round((a1/pobtot)*100000, 2),
         m2_t_pob = round((a2/pobtot)*100000,2),
         a1_t_pob = round((a1/pobtot)*100000, 2),
         a2_t_pob = round((a2/pobtot)*100000,2))

# indicador cambio porcentual

delitos_indicadores <- delitos_indicadores %>% 
  mutate(t_crec_anual = round(ifelse(a1_t_pob==0 & a2_t_pob!=0, 100,
                                     ((a2_t_pob - a1_t_pob)/a1_t_pob)*100),2))
is.na(delitos_indicadores)<-sapply(delitos_indicadores, is.infinite)
delitos_indicadores[is.na(delitos_indicadores)]<-0

####Categorización por jenks###

p_load(BAMMtools)
p_load(plyr)

#Para la incidencia
jenks_a2 <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$a2_t_pob, 5))

# write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")

jenks_crecimiento <- ddply(delitos_indicadores, .(delito_ab), function(x) getJenksBreaks(x$t_crec_anual, 5))

##### tasa anual ###

indicador_anual <- left_join(delitos_indicadores, jenks_a2, by = "delito_ab")

indicador_anual <- as.data.frame(indicador_anual)

indicador_anual$eva_incidencia_a2 <- case_when(indicador_anual$a2_t_pob<=indicador_anual$V5 & indicador_anual$a2_t_pob>indicador_anual$V4 ~ 1, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V4 & indicador_anual$a2_t_pob>indicador_anual$V3 ~ .75, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V3 & indicador_anual$a2_t_pob>indicador_anual$V2 ~ .5, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V2 & indicador_anual$a2_t_pob>0 ~ .25, 
                                               indicador_anual$a2_t_pob==0 ~ 0)

indicador_anual <- indicador_anual %>% 
  select(1:12, 19)

######## seguridad ejecutiva#######
incidencia_seg_ejecutiva <- indicador_anual %>% 
  filter(delito_ab == "homicidio_dol" | delito_ab == "secuestro" | delito_ab == "robo_veh_cv")

incidencia_seg_ejecutiva <- incidencia_seg_ejecutiva %>% 
  select(2, 3, 7, 13)

incidencia_seg_ejecutiva <- incidencia_seg_ejecutiva %>% 
  spread(delito_ab, eva_incidencia_a2) %>% 
  group_by(Entidad)

### multiplicar por ponderación

incidencia_seg_ejecutiva <- incidencia_seg_ejecutiva %>% 
  mutate(eva_homicidio = homicidio_dol*.4, 
         eva_secuestro = secuestro*.35,
         eva_robo_veh_cv = robo_veh_cv*.25)

incidencia_seg_ejecutiva <- incidencia_seg_ejecutiva %>% 
  mutate(riesgo_incidencia = eva_homicidio + eva_secuestro + eva_robo_veh_cv )

incidencia_seg_ejecutiva_sub <- incidencia_seg_ejecutiva %>% 
  select(1, 2, 9)

names(incidencia_seg_ejecutiva_sub)[1] <- "Clave_ent"

###### seguridad patrimonial #####

incidencia_seg_patrimonial <- indicador_anual %>% 
  filter(delito_ab == "robo_negocio" | delito_ab == "robo_transportista_sv" | delito_ab == "robo_transportista_cv" | delito_ab == "robo_casa"| delito_ab == "robo_maquinaria" | delito_ab == "robo_veh_cv" | delito_ab == "robo_veh_sv" | delito_ab == "d_propiedad")

incidencia_seg_patrimonial <- incidencia_seg_patrimonial %>% 
  select(2, 3, 7, 13)

incidencia_seg_patrimonial <- incidencia_seg_patrimonial %>% 
  spread(delito_ab, eva_incidencia_a2) %>% 
  group_by(Entidad)

### multiplicar por ponderación

incidencia_seg_patrimonial <- incidencia_seg_patrimonial %>% 
  mutate(eva_d_propiedad = d_propiedad*.1,
         eva_robo_maquinaria = robo_maquinaria*.1,
         eva_robo_negocio = robo_negocio*.2,
         eva_robo_transportista_cv = robo_transportista_cv*.2,
         eva_robo_transportista_sv = robo_transportista_sv*.15,
         eva_robo_veh_cv = robo_veh_cv*.15,
         eva_robo_veh_sv = robo_veh_sv*.1)

incidencia_seg_patrimonial <- incidencia_seg_patrimonial %>% 
  mutate(riesgo_incidencia = eva_d_propiedad + eva_robo_maquinaria + eva_robo_negocio + eva_robo_transportista_cv + eva_robo_transportista_sv + eva_robo_veh_cv + eva_robo_veh_sv  )

incidencia_seg_patrimonial_sub <- incidencia_seg_patrimonial %>% 
  select(1, 2, 18)

####### presencia de grupos criminales #####

presencia_terr <- read_excel("201229-bd-grupos_civiles_armados_ene_2021.xlsx")

presencia_terr$...13 <- NULL

presencia_terr <- presencia_terr %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", 
                          ifelse(Entidad == "Michoacán", "Michoacán de Ocampo", 
                                 ifelse(Entidad == "Veracruz", "Veracruz de Ignacio de la Llave",
                                        ifelse(Entidad == "Coahuila", "Coahuila de Zaragoza",
                                               Entidad)))))


presencia_terr <- presencia_terr %>% 
  mutate(Clave_entidad = ifelse(Entidad == "Aguascalientes", "01",
                                ifelse(Entidad == "Baja California", "02", 
                                       ifelse(Entidad == "Baja California Sur", "03",
                                              ifelse(Entidad == "Campeche", "04",
                                                     ifelse( Entidad == "Chiapas", "07", 
                                                             ifelse(Entidad == "Chihuahua", "08",
                                                                    ifelse(Entidad == "Coahuila de Zaragoza", "05",
                                                                           ifelse(Entidad == "Colima", "06",
                                                                                  ifelse(Entidad == "Ciudad de México", "09",
                                                                                         ifelse(Entidad == "Durango", "10",
                                                                                                ifelse(Entidad == "Guanajuato", "11", 
                                                                                                       ifelse(Entidad == "Guerrero", "12", 
                                                                                                              ifelse(Entidad == "Hidalgo", "13", 
                                                                                                                     ifelse(Entidad == "Jalisco", "14",
                                                                                                                            ifelse(Entidad == "México", "15",
                                                                                                                                   ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                          ifelse(Entidad == "Morelos", "17",
                                                                                                                                                 ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                        ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                               ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                      ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                             ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                                    ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                           ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                                  ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                         ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                                ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                       ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                              ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                                     ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                            ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                                  ))))))))))


#### indicador de pulverización 
pulverizacion <- presencia_terr %>% 
  select(1: 6)

pulverizacion <- pulverizacion %>% 
  select(1:2) %>% 
  group_by(Clave_entidad, Entidad) %>% 
  count()

names(pulverizacion)[3] <- "Compuesto"

### categorización por jenks

jenks_compuesto <-getJenksBreaks(pulverizacion$Compuesto, 5)

jenks_compuesto <- as.data.frame(jenks_compuesto)

jenks_compuesto <- rownames_to_column(jenks_compuesto, var = "rowname")

jenks_compuesto <- jenks_compuesto %>%
  spread(key = rowname, value = jenks_compuesto, fill = 0)

pulverizacion <- cbind(pulverizacion, jenks_compuesto)

colnames(pulverizacion)[4:8] <- paste0("V", colnames(pulverizacion)[4:8])

pulverizacion$eva_compuesto <- case_when(pulverizacion$Compuesto<=pulverizacion$V5 & pulverizacion$Compuesto>pulverizacion$V4 ~ 1, 
                                         pulverizacion$Compuesto<=pulverizacion$V4 & pulverizacion$Compuesto>pulverizacion$V3 ~ .75, 
                                         pulverizacion$Compuesto<=pulverizacion$V3 & pulverizacion$Compuesto>pulverizacion$V2 ~ .5, 
                                         pulverizacion$Compuesto<= pulverizacion$V2 & pulverizacion$Compuesto>= pulverizacion$V1 ~ .25,
                                         pulverizacion$Compuesto==0 ~ 0)

pulverizacion <- pulverizacion %>% 
  select(1:3, 9)

i_seg_ejecutiva_p <- pulverizacion %>% 
  select(1, 2, 4)

##### Eventos violentos ####

eva <- read_csv("datos_acled.csv")

eva <- eva %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", 
                          ifelse(Entidad == "Michoacán", "Michoacán de Ocampo", 
                                 ifelse(Entidad == "Veracruz", "Veracruz de Ignacio de la Llave",
                                        ifelse(Entidad == "Coahuila", "Coahuila de Zaragoza",
                                               Entidad)))))


eva <- eva[eva$EVENT_TYPE == "Battles" | eva$EVENT_TYPE == "Explosions/Remote violence" | eva$EVENT_TYPE == "Violence against civilians",] 

eva <- eva %>% 
  mutate(ACTOR1 = ifelse(ACTOR1 == "Unidentified Gang (Mexico)", "Unidentified Armed Group (Mexico)", ACTOR1))

### seleccionar los últimos doce meses

hoy <- "2020-12-15"

hoy <- strftime(hoy, format = "%Y-%m-%d")

mes <- as.Date(hoy, format="%Y-%m-%d")-365

mes <- strftime(mes, format = "%Y-%m-%d")

eva <- eva[eva$EVENT_DATE >= mes & eva$EVENT_DATE <= hoy,]

##### por número de civiles víctimas de violencia, grupo y tipo de evento ###

victimas_viol_civ <- eva %>% 
  filter(EVENT_TYPE == "Violence against civilians")

victimas_viol_civ_tot <- aggregate( FATALITIES ~ Clave_ent + Entidad, data = victimas_viol_civ, sum )

names(victimas_viol_civ_tot)[3] <- "total_victimas_civiles"

victimas_viol_civ_count <- victimas_viol_civ %>%
  select(19, 31) %>% 
  group_by(Clave_ent, Entidad) %>% 
  count()

names(victimas_viol_civ_count)[3] <- "total_eventos_civiles"

violencia_civil <- left_join(victimas_viol_civ_count, victimas_viol_civ_tot, by = c("Clave_ent", "Entidad"))

## categorización por número de eventos

jenks_eventos_civ <-getJenksBreaks(violencia_civil$total_eventos_civiles, 5)

jenks_eventos_civ <- as.data.frame(jenks_eventos_civ)

jenks_eventos_civ <- rownames_to_column(jenks_eventos_civ, var = "rowname")

jenks_eventos_civ <- jenks_eventos_civ %>%
  spread(key = rowname, value = jenks_eventos_civ, fill = 0)

violencia_civil <- cbind(violencia_civil, jenks_eventos_civ)

colnames(violencia_civil)[5:9] <- paste0("V", colnames(violencia_civil)[5:9])

violencia_civil$eva_eventos_civ <- case_when(violencia_civil$total_eventos_civiles<=violencia_civil$V5 & violencia_civil$total_eventos_civiles>violencia_civil$V4 ~ 1, 
                                             violencia_civil$total_eventos_civiles<=violencia_civil$V4 & violencia_civil$total_eventos_civiles>violencia_civil$V3 ~ .75, 
                                             violencia_civil$total_eventos_civiles<=violencia_civil$V3 & violencia_civil$total_eventos_civiles>violencia_civil$V2 ~ .5, 
                                             violencia_civil$total_eventos_civiles<= violencia_civil$V2 & violencia_civil$total_eventos_civiles>= violencia_civil$V1 ~ .25,
                                             violencia_civil$total_eventos_civiles==0 ~ 0)

violencia_civil <- violencia_civil %>% 
  select(1:4, 10)

## categorización por número de víctimas civiles

jenks_victimas_civ <-getJenksBreaks(violencia_civil$total_victimas_civiles, 5)

jenks_victimas_civ <- as.data.frame(jenks_victimas_civ)

jenks_victimas_civ <- rownames_to_column(jenks_victimas_civ, var = "rowname")

jenks_victimas_civ <- jenks_victimas_civ %>%
  spread(key = rowname, value = jenks_victimas_civ, fill = 0)

violencia_civil <- cbind(violencia_civil, jenks_victimas_civ)

colnames(violencia_civil)[6:10] <- paste0("V", colnames(violencia_civil)[6:10])

violencia_civil$eva_victimas_civ <- case_when(violencia_civil$total_victimas_civiles<=violencia_civil$V5 & violencia_civil$total_victimas_civiles>violencia_civil$V4 ~ 1, 
                                              violencia_civil$total_victimas_civiles<=violencia_civil$V4 & violencia_civil$total_victimas_civiles>violencia_civil$V3 ~ .75, 
                                              violencia_civil$total_victimas_civiles<=violencia_civil$V3 & violencia_civil$total_victimas_civiles>violencia_civil$V2 ~ .5, 
                                              violencia_civil$total_victimas_civiles<= violencia_civil$V2 & violencia_civil$total_victimas_civiles>= violencia_civil$V1 ~ .25,
                                              violencia_civil$total_victimas_civiles==0 ~ 0)

violencia_civil <- violencia_civil %>% 
  select(1:5, 11)

violencia_civil$Clave_ent <- str_pad(violencia_civil$Clave_ent, 2, pad = 0)

violencia_civil_sub <- violencia_civil %>% 
  select(1, 2, 5, 6)

##### por número de enfrentamientos ###

enfrentamientos <- eva %>% 
  filter(EVENT_TYPE == "Battles" |EVENT_TYPE == "Explosions/Remote violence")

enfrentamientos_tot <- aggregate( FATALITIES ~ Clave_ent + Entidad, data = enfrentamientos, sum )

names(enfrentamientos_tot)[3] <- "total_victimas_enfrentamientos"

enfrentamientos_count <- enfrentamientos %>%
  select(19, 31) %>% 
  group_by(Clave_ent, Entidad) %>% 
  count()

names(enfrentamientos_count)[3] <- "total_eventos_enfrentamientos"

enfrentamientos <- left_join(enfrentamientos_count, enfrentamientos_tot, by = c("Clave_ent", "Entidad"))

## categorización por número de eventos

jenks_eventos_enfren <-getJenksBreaks(enfrentamientos$total_eventos_enfrentamientos, 5)

jenks_eventos_enfren <- as.data.frame(jenks_eventos_enfren)

jenks_eventos_enfren <- rownames_to_column(jenks_eventos_enfren, var = "rowname")

jenks_eventos_enfren <- jenks_eventos_enfren %>%
  spread(key = rowname, value = jenks_eventos_enfren, fill = 0)

enfrentamientos <- cbind(enfrentamientos, jenks_eventos_enfren)

colnames(enfrentamientos)[5:9] <- paste0("V", colnames(enfrentamientos)[5:9])

enfrentamientos$eva_eventos_enfrenta <- case_when(enfrentamientos$total_eventos_enfrentamientos<=enfrentamientos$V5 & enfrentamientos$total_eventos_enfrentamientos>enfrentamientos$V4 ~ 1, 
                                                  enfrentamientos$total_eventos_enfrentamientos<=enfrentamientos$V4 & enfrentamientos$total_eventos_enfrentamientos>enfrentamientos$V3 ~ .75, 
                                                  enfrentamientos$total_eventos_enfrentamientos<=enfrentamientos$V3 & enfrentamientos$total_eventos_enfrentamientos>enfrentamientos$V2 ~ .5, 
                                                  enfrentamientos$total_eventos_enfrentamientos<= enfrentamientos$V2 & enfrentamientos$total_eventos_enfrentamientos>= enfrentamientos$V1 ~ .25,
                                                  enfrentamientos$total_eventos_enfrentamientos==0 ~ 0)

enfrentamientos <- enfrentamientos %>% 
  select(1:4, 10)

## categorización por número de víctimas enfrentamientos

jenks_victimas_enfren <-getJenksBreaks(enfrentamientos$total_victimas_enfrentamientos, 5)

jenks_victimas_enfren <- as.data.frame(jenks_victimas_enfren)

jenks_victimas_enfren <- rownames_to_column(jenks_victimas_enfren, var = "rowname")

jenks_victimas_enfren <- jenks_victimas_enfren %>%
  spread(key = rowname, value = jenks_victimas_enfren, fill = 0)

enfrentamientos <- cbind(enfrentamientos, jenks_victimas_enfren)

colnames(enfrentamientos)[6:10] <- paste0("V", colnames(enfrentamientos)[6:10])

enfrentamientos$eva_victimas_enfren <- case_when(enfrentamientos$total_victimas_enfrentamientos<=enfrentamientos$V5 & enfrentamientos$total_victimas_enfrentamientos>enfrentamientos$V4 ~ 1, 
                                                 enfrentamientos$total_victimas_enfrentamientos<=enfrentamientos$V4 & enfrentamientos$total_victimas_enfrentamientos>enfrentamientos$V3 ~ .75, 
                                                 enfrentamientos$total_victimas_enfrentamientos<=enfrentamientos$V3 & enfrentamientos$total_victimas_enfrentamientos>enfrentamientos$V2 ~ .5, 
                                                 enfrentamientos$total_victimas_enfrentamientos<= enfrentamientos$V2 & enfrentamientos$total_victimas_enfrentamientos>= enfrentamientos$V1 ~ .25,
                                                 enfrentamientos$total_victimas_enfrentamientos==0 ~ 0)

enfrentamientos <- enfrentamientos %>% 
  select(1:5, 11)

enfrentamientos$Clave_ent <- str_pad(enfrentamientos$Clave_ent, 2, pad = 0)

estados <- incidencia_seg_ejecutiva %>% 
  select(1,2)

enfrentamientos <- left_join(estados, enfrentamientos, by = "Entidad")

enfrentamientos <- enfrentamientos %>% 
  replace(is.na(.), 0) 

enfrentamientos$Clave_ent <- NULL

# names(enfrentamientos)[1] <- "Clave_ent"

enfrentamientos_sub <- enfrentamientos %>% 
  select(1, 2, 5, 6)

names(enfrentamientos_sub)[1] <- "Clave_ent"

#### unir bases para indicador de victimización durante eva's####

indicador_victimizacion_eva <- left_join(violencia_civil_sub, enfrentamientos_sub, by = c("Clave_ent", "Entidad"))

### multiplicar por ponderación

indicador_victimizacion_eva <- indicador_victimizacion_eva %>% 
  mutate(p_eva_eventos_civ = eva_eventos_civ*.4,
         p_eva_victimas_civ = eva_victimas_civ*.2,
         p_eva_eventos_enfrenta = eva_eventos_enfrenta*.3,
         p_eva_victimas_enfren = eva_victimas_enfren*.1)

indicador_victimizacion_eva <- indicador_victimizacion_eva %>% 
  mutate(riesgo_victimizacion_eva = p_eva_eventos_civ + p_eva_victimas_civ + p_eva_eventos_enfrenta + p_eva_victimas_enfren)

indicador_victimizacion_eva <- indicador_victimizacion_eva %>% 
  select(1, 2, 11)

#### unificamos la base

names(incidencia_seg_ejecutiva)[1] <- "Clave_ent"
names(i_seg_ejecutiva_p)[1] <- "Clave_ent" 

######## ENVIPE #####

percepcion_seg <- readxl::read_xlsx("envipe_percepcion_seguridad.xlsx")

percepcion_seg <- percepcion_seg %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", Entidad))

percepcion_seg <- percepcion_seg %>% 
  mutate(Clave_ent = ifelse(Entidad == "Aguascalientes", "01",
                            ifelse(Entidad == "Baja California", "02", 
                                   ifelse(Entidad == "Baja California Sur", "03",
                                          ifelse(Entidad == "Campeche", "04",
                                                 ifelse( Entidad == "Chiapas", "07", 
                                                         ifelse(Entidad == "Chihuahua", "08",
                                                                ifelse(Entidad == "Coahuila de Zaragoza", "05",
                                                                       ifelse(Entidad == "Colima", "06",
                                                                              ifelse(Entidad == "Ciudad de México", "09",
                                                                                     ifelse(Entidad == "Durango", "10",
                                                                                            ifelse(Entidad == "Guanajuato", "11", 
                                                                                                   ifelse(Entidad == "Guerrero", "12", 
                                                                                                          ifelse(Entidad == "Hidalgo", "13", 
                                                                                                                 ifelse(Entidad == "Jalisco", "14",
                                                                                                                        ifelse(Entidad == "México", "15",
                                                                                                                               ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                      ifelse(Entidad == "Morelos", "17",
                                                                                                                                             ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                    ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                           ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                  ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                         ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                                ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                       ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                              ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                     ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                            ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                   ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                          ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                                 ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                        ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                              ))))))))))

### categorizacion con jenks 

jenks_per_seg <-getJenksBreaks(percepcion_seg$porc_inseguros, 5)

jenks_per_seg <- as.data.frame(jenks_per_seg)

jenks_per_seg <- rownames_to_column(jenks_per_seg, var = "rowname")

jenks_per_seg <- jenks_per_seg %>%
  spread(key = rowname, value = jenks_per_seg, fill = 0)

percepcion_seg <- cbind(percepcion_seg, jenks_per_seg)

colnames(percepcion_seg)[5:9] <- paste0("V", colnames(percepcion_seg)[5:9])

percepcion_seg$eva_perc_seg <- case_when(percepcion_seg$porc_inseguros<=percepcion_seg$V5 & percepcion_seg$porc_inseguros>percepcion_seg$V4 ~ 1, 
                                         percepcion_seg$porc_inseguros<=percepcion_seg$V4 & percepcion_seg$porc_inseguros>percepcion_seg$V3 ~ .75, 
                                         percepcion_seg$porc_inseguros<=percepcion_seg$V3 & percepcion_seg$porc_inseguros>percepcion_seg$V2 ~ .5, 
                                         percepcion_seg$porc_inseguros<= percepcion_seg$V2 & percepcion_seg$porc_inseguros>= percepcion_seg$V1 ~ .25,
                                         percepcion_seg$porc_inseguros==0 ~ 0)

percepcion_seg <- percepcion_seg %>% 
  select(4, 1, 2, 3, 10)

i_seg_ejecutiva_per_seg <- percepcion_seg %>% 
  select(1:2, 5)

### tasa victimización

tasa_victimizacion <- readxl::read_xlsx("envipe_victimizacion.xlsx")

tasa_victimizacion <- tasa_victimizacion %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", Entidad))

tasa_victimizacion <- tasa_victimizacion %>% 
  mutate(Clave_ent = ifelse(Entidad == "Aguascalientes", "01",
                            ifelse(Entidad == "Baja California", "02", 
                                   ifelse(Entidad == "Baja California Sur", "03",
                                          ifelse(Entidad == "Campeche", "04",
                                                 ifelse( Entidad == "Chiapas", "07", 
                                                         ifelse(Entidad == "Chihuahua", "08",
                                                                ifelse(Entidad == "Coahuila de Zaragoza", "05",
                                                                       ifelse(Entidad == "Colima", "06",
                                                                              ifelse(Entidad == "Ciudad de México", "09",
                                                                                     ifelse(Entidad == "Durango", "10",
                                                                                            ifelse(Entidad == "Guanajuato", "11", 
                                                                                                   ifelse(Entidad == "Guerrero", "12", 
                                                                                                          ifelse(Entidad == "Hidalgo", "13", 
                                                                                                                 ifelse(Entidad == "Jalisco", "14",
                                                                                                                        ifelse(Entidad == "México", "15",
                                                                                                                               ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                      ifelse(Entidad == "Morelos", "17",
                                                                                                                                             ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                    ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                           ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                  ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                         ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                                ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                       ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                              ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                     ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                            ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                   ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                          ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                                 ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                        ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                              ))))))))))


### categorizacion con jenks 

jenks_victimizacion <-getJenksBreaks(tasa_victimizacion$tasa_victimizacion, 5)

jenks_victimizacion <- as.data.frame(jenks_victimizacion)

jenks_victimizacion <- rownames_to_column(jenks_victimizacion, var = "rowname")

jenks_victimizacion <- jenks_victimizacion %>%
  spread(key = rowname, value = jenks_victimizacion, fill = 0)

tasa_victimizacion <- cbind(tasa_victimizacion, jenks_victimizacion)

colnames(tasa_victimizacion)[4:8] <- paste0("V", colnames(tasa_victimizacion)[4:8])

tasa_victimizacion$eva_victimizacion <- case_when(tasa_victimizacion$tasa_victimizacion<=tasa_victimizacion$V5 & tasa_victimizacion$tasa_victimizacion>tasa_victimizacion$V4 ~ 1, 
                                                  tasa_victimizacion$tasa_victimizacion<=tasa_victimizacion$V4 & tasa_victimizacion$tasa_victimizacion>tasa_victimizacion$V3 ~ .75, 
                                                  tasa_victimizacion$tasa_victimizacion<=tasa_victimizacion$V3 & tasa_victimizacion$tasa_victimizacion>tasa_victimizacion$V2 ~ .5, 
                                                  tasa_victimizacion$tasa_victimizacion<= tasa_victimizacion$V2 & tasa_victimizacion$tasa_victimizacion>= tasa_victimizacion$V1 ~ .25,
                                                  tasa_victimizacion$tasa_victimizacion==0 ~ 0)

tasa_victimizacion <- tasa_victimizacion %>% 
  select(3, 1, 2, 9)

i_seg_ejecutiva_victimizacion <- tasa_victimizacion %>% 
  select(1:2, 4)

##### conflictividad social #####

conflictividad <- read_csv("/Users/aritzy/Documents/Rizika/Conflictividad social/201216-BD-conflictividad_social_nov_2020.csv")

conflictividad <- conflictividad %>% 
  filter(Año == "2020" | Año == "2019" & Mes == "Diciembre")

mecanismo <- unique(conflictividad$Mecanismo)

conflictividad <- conflictividad %>% 
  filter(Mecanismo == "Linchamiento" | Mecanismo == "Intento de linchamiento")

conflictividad <- conflictividad %>% 
  select(5, 6, 13)

linchamiento <- conflictividad %>% 
  select(1, 2) %>% 
  group_by(Estado) %>% 
  count

linchamiento <- linchamiento %>% 
  mutate(Entidad = ifelse(Estado == "Distrito Federal", "Ciudad de México", Estado))

estados <- i_seg_ejecutiva %>% 
  select(1,2)

linchamiento <- left_join(estados, linchamiento, by = "Entidad")

linchamiento <- linchamiento %>% 
  select(1, 2, 5)

names(linchamiento)[1] <- "Clave_ent"
names(linchamiento)[2] <- "Entidad"
names(linchamiento)[3] <- "tot_linchamiento"

linchamiento <- linchamiento %>% 
  replace(is.na(.), 0) 


### categorizacion con jenks 

jenks_linchamiento <-getJenksBreaks(linchamiento$tot_linchamiento, 5)

jenks_linchamiento <- as.data.frame(jenks_linchamiento)

jenks_linchamiento <- rownames_to_column(jenks_linchamiento, var = "rowname")

jenks_linchamiento <- jenks_linchamiento %>%
  spread(key = rowname, value = jenks_linchamiento, fill = 0)

colnames(jenks_linchamiento)[1:5] <- paste0("V", colnames(jenks_linchamiento)[1:5])

linchamiento$eva_linchamiento <- case_when(linchamiento$tot_linchamiento<=jenks_linchamiento$V5 & linchamiento$tot_linchamiento>jenks_linchamiento$V4 ~ 1, 
                                           linchamiento$tot_linchamiento<=jenks_linchamiento$V4 & linchamiento$tot_linchamiento>jenks_linchamiento$V3 ~ .75, 
                                           linchamiento$tot_linchamiento<=jenks_linchamiento$V3 & linchamiento$tot_linchamiento>jenks_linchamiento$V2 ~ .5, 
                                           linchamiento$tot_linchamiento<= jenks_linchamiento$V2 & linchamiento$tot_linchamiento> 0 ~ .25,
                                           linchamiento$tot_linchamiento==0 ~ 0)

i_seg_ejecutiva_linchamiento <- linchamiento %>% 
  select(1:2, 4)


#### unificamos base
indicador_seg_ejecutiva <- Reduce(function(x,y) merge(x=x, y=y, by = c( "Clave_ent", "Entidad")), list( incidencia_seg_ejecutiva_sub, i_seg_ejecutiva_p, indicador_victimizacion_eva, i_seg_ejecutiva_per_seg, i_seg_ejecutiva_victimizacion, i_seg_ejecutiva_linchamiento))


indicador_seg_ejecutiva <- indicador_seg_ejecutiva %>% 
  mutate(p_riesgo_incidencia = riesgo_incidencia*.25,
         p_eva_compuesto = eva_compuesto*.25,
         p_riesgo_victimizacion_eva = riesgo_victimizacion_eva*.25,
         p_eva_perc_seg = eva_perc_seg*.05,
         p_eva_victimizacion = eva_victimizacion*.1,
         p_eva_linchamiento = eva_linchamiento*.1)


indicador_seg_ejecutiva <- indicador_seg_ejecutiva %>% 
  mutate(evaluacion = p_riesgo_incidencia + p_eva_compuesto + p_riesgo_victimizacion_eva + p_eva_perc_seg + p_eva_victimizacion + p_eva_linchamiento)

colnames(indicador_seg_ejecutiva)[3:6] <- paste0("eva_", colnames(indicador_seg_ejecutiva)[3:6])

indicador_seg_ejecutiva <- indicador_seg_ejecutiva %>% 
  mutate(factor_homicidio = eva_homicidio_dol*.15*3*4, 
         factor_secuestro = eva_secuestro*.15*3*4,
         factor_robo_veh_cv = eva_robo_veh_cv*.05*3*3,
         factor_narcomenudeo = eva_narcomenudeo*.05*2*2,
         factor_compuesto = eva_compuesto*.05*3*5,
         factor_eventos_civ = eva_eventos_civ*.05*3*3,
         factor_victimas_civ = eva_victimas_civ*.1*3*3,
         factor_eventos_enfrenta = eva_eventos_enfrenta*.05*3*3,
         factor_victimas_enfren = eva_victimas_enfren*.1*3*3,
         factor_perc_seg = eva_perc_seg*.075*1*2,
         factor_victimizacion = eva_victimizacion*.1*3*2,
         factor_linchamiento = eva_linchamiento*.075*3*2)

indicador_seg_ejecutiva <- indicador_seg_ejecutiva %>% 
  mutate(evaluacion_riesgo = factor_homicidio + factor_secuestro + factor_robo_veh_cv + factor_narcomenudeo + factor_compuesto + factor_eventos_civ + factor_victimas_civ + factor_eventos_enfrenta + factor_victimas_enfren + factor_perc_seg +  factor_victimizacion + factor_linchamiento)

### categorizacion con jenks 

jenks_indicador <-getJenksBreaks(indicador_seg_ejecutiva$evaluacion_riesgo, 5)

jenks_indicador <- as.data.frame(jenks_indicador)

jenks_indicador <- rownames_to_column(jenks_indicador, var = "rowname")

jenks_indicador <- jenks_indicador %>%
  spread(key = rowname, value = jenks_indicador, fill = 0)

colnames(jenks_indicador)[1:5] <- paste0("V", colnames(jenks_indicador)[1:5])

indicador_seg_ejecutiva$nivel_riesgo <- case_when(indicador_seg_ejecutiva$evaluacion_riesgo<=jenks_indicador$V5 & indicador_seg_ejecutiva$evaluacion_riesgo>jenks_indicador$V4 ~ "Riesgo muy alto", 
                                                  indicador_seg_ejecutiva$evaluacion_riesgo<=jenks_indicador$V4 & indicador_seg_ejecutiva$evaluacion_riesgo>jenks_indicador$V3 ~ "Riesgo alto", 
                                                  indicador_seg_ejecutiva$evaluacion_riesgo<=jenks_indicador$V3 & indicador_seg_ejecutiva$evaluacion_riesgo>jenks_indicador$V2 ~ "Riesgo medio", 
                                                  indicador_seg_ejecutiva$evaluacion_riesgo<= jenks_indicador$V2 & indicador_seg_ejecutiva$evaluacion_riesgo>=jenks_indicador$V1 ~ "Riesgo bajo")


indicador_seg_ejecutiva_long <- indicador_seg_ejecutiva %>% 
  select(1, 2, 15:26)

indicador_seg_ejecutiva_long <- gather(indicador_seg_ejecutiva_long, factor_riesgo, evaluacion, factor_homicidio:factor_linchamiento, factor_key=TRUE)

jenks_factores_ej <- ddply(indicador_seg_ejecutiva_long, .(factor_riesgo), function(x) getJenksBreaks(x$evaluacion, 5))


############################# seguridad patrimonial ###############


#seguridad patrimonial

i_seg_patrimonial <- indicador_anual %>% 
  filter(delito_ab == "robo_negocio" | delito_ab == "robo_transportista_sv" | delito_ab == "robo_transportista_cv" | delito_ab == "robo_casa"| delito_ab == "robo_maquinaria" | delito_ab == "robo_veh_cv" | delito_ab == "robo_veh_sv" | delito_ab == "d_propiedad")

i_seg_patrimonial <- i_seg_patrimonial %>% 
  select(2, 3, 7, 12)

i_seg_patrimonial <- i_seg_patrimonial %>% 
  spread(delito_ab, eva_incidencia_a2) %>% 
  group_by(Entidad)

colnames(i_seg_patrimonial)[3:10] <- paste0("eva_", colnames(i_seg_patrimonial)[3:10])

names(incidencia_seg_patrimonial_sub)[1] <- "Clave_ent"

##### enve - victimización empresarial ######

victimizacion_empr <- readxl::read_xlsx("enve_victimizacion.xlsx")

victimizacion_empr <- victimizacion_empr %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", Entidad))

victimizacion_empr <- victimizacion_empr %>% 
  mutate(Clave_ent = ifelse(Entidad == "Aguascalientes", "01",
                            ifelse(Entidad == "Baja California", "02", 
                                   ifelse(Entidad == "Baja California Sur", "03",
                                          ifelse(Entidad == "Campeche", "04",
                                                 ifelse( Entidad == "Chiapas", "07", 
                                                         ifelse(Entidad == "Chihuahua", "08",
                                                                ifelse(Entidad == "Coahuila de Zaragoza", "05",
                                                                       ifelse(Entidad == "Colima", "06",
                                                                              ifelse(Entidad == "Ciudad de México", "09",
                                                                                     ifelse(Entidad == "Durango", "10",
                                                                                            ifelse(Entidad == "Guanajuato", "11", 
                                                                                                   ifelse(Entidad == "Guerrero", "12", 
                                                                                                          ifelse(Entidad == "Hidalgo", "13", 
                                                                                                                 ifelse(Entidad == "Jalisco", "14",
                                                                                                                        ifelse(Entidad == "México", "15",
                                                                                                                               ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                      ifelse(Entidad == "Morelos", "17",
                                                                                                                                             ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                    ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                           ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                  ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                         ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                                ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                       ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                              ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                     ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                            ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                   ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                          ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                                 ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                        ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                              ))))))))))


### categorizacion con jenks 

jenks_victimizacion_empr <-getJenksBreaks(victimizacion_empr$tasa_victimizacion_em, 5)

jenks_victimizacion_empr <- as.data.frame(jenks_victimizacion_empr)

jenks_victimizacion_empr <- rownames_to_column(jenks_victimizacion_empr, var = "rowname")

jenks_victimizacion_empr <- jenks_victimizacion_empr %>%
  spread(key = rowname, value = jenks_victimizacion_empr, fill = 0)

tasa_victimizacion_empr <- cbind(victimizacion_empr, jenks_victimizacion_empr)

colnames(tasa_victimizacion_empr)[4:8] <- paste0("V", colnames(tasa_victimizacion_empr)[4:8])

tasa_victimizacion_empr$eva_victimizacion_empr <- case_when(tasa_victimizacion_empr$tasa_victimizacion_em<=tasa_victimizacion_empr$V5 & tasa_victimizacion_empr$tasa_victimizacion_em>tasa_victimizacion_empr$V4 ~ 1, 
                                                            tasa_victimizacion_empr$tasa_victimizacion_em<=tasa_victimizacion_empr$V4 & tasa_victimizacion_empr$tasa_victimizacion_em>tasa_victimizacion_empr$V3 ~ .75, 
                                                            tasa_victimizacion_empr$tasa_victimizacion_em<=tasa_victimizacion_empr$V3 & tasa_victimizacion_empr$tasa_victimizacion_em>tasa_victimizacion_empr$V2 ~ .5, 
                                                            tasa_victimizacion_empr$tasa_victimizacion_em<= tasa_victimizacion_empr$V2 & tasa_victimizacion_empr$tasa_victimizacion_em>= tasa_victimizacion_empr$V1 ~ .25)

tasa_victimizacion_empr <- tasa_victimizacion_empr %>% 
  select(3, 1, 2, 9)

i_seg_patrimonial_victim <- tasa_victimizacion_empr %>% 
  select(1:2, 4)

### enve: percepción inseguridad y delincuencia

per_inseguridad_empr <- readxl::read_xlsx("enve_per_inseguridad.xlsx")

per_inseguridad_empr <- per_inseguridad_empr %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", Entidad))

per_inseguridad_empr <- per_inseguridad_empr %>% 
  mutate(Clave_ent = ifelse(Entidad == "Aguascalientes", "01",
                            ifelse(Entidad == "Baja California", "02", 
                                   ifelse(Entidad == "Baja California Sur", "03",
                                          ifelse(Entidad == "Campeche", "04",
                                                 ifelse( Entidad == "Chiapas", "07", 
                                                         ifelse(Entidad == "Chihuahua", "08",
                                                                ifelse(Entidad == "Coahuila de Zaragoza", "05",
                                                                       ifelse(Entidad == "Colima", "06",
                                                                              ifelse(Entidad == "Ciudad de México", "09",
                                                                                     ifelse(Entidad == "Durango", "10",
                                                                                            ifelse(Entidad == "Guanajuato", "11", 
                                                                                                   ifelse(Entidad == "Guerrero", "12", 
                                                                                                          ifelse(Entidad == "Hidalgo", "13", 
                                                                                                                 ifelse(Entidad == "Jalisco", "14",
                                                                                                                        ifelse(Entidad == "México", "15",
                                                                                                                               ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                      ifelse(Entidad == "Morelos", "17",
                                                                                                                                             ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                    ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                           ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                  ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                         ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                                ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                       ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                              ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                     ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                            ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                   ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                          ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                                 ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                        ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                              ))))))))))


### categorizacion con jenks 

jenks_per_inseguridad_empr <-getJenksBreaks(per_inseguridad_empr$porcentaje_inseguridad, 5)

jenks_per_inseguridad_empr <- as.data.frame(jenks_per_inseguridad_empr)

jenks_per_inseguridad_empr <- rownames_to_column(jenks_per_inseguridad_empr, var = "rowname")

jenks_per_inseguridad_empr <- jenks_per_inseguridad_empr %>%
  spread(key = rowname, value = jenks_per_inseguridad_empr, fill = 0)

per_inseguridad_empr <- cbind(per_inseguridad_empr, jenks_per_inseguridad_empr)

colnames(per_inseguridad_empr)[4:8] <- paste0("V", colnames(per_inseguridad_empr)[4:8])

per_inseguridad_empr$eva_per_inseguridad_empr <- case_when(per_inseguridad_empr$porcentaje_inseguridad<=per_inseguridad_empr$V5 & per_inseguridad_empr$porcentaje_inseguridad>per_inseguridad_empr$V4 ~ 1, 
                                                           per_inseguridad_empr$porcentaje_inseguridad<=per_inseguridad_empr$V4 & per_inseguridad_empr$porcentaje_inseguridad>per_inseguridad_empr$V3 ~ .75, 
                                                           per_inseguridad_empr$porcentaje_inseguridad<=per_inseguridad_empr$V3 & per_inseguridad_empr$porcentaje_inseguridad>per_inseguridad_empr$V2 ~ .5, 
                                                           per_inseguridad_empr$porcentaje_inseguridad<= per_inseguridad_empr$V2 & per_inseguridad_empr$porcentaje_inseguridad>= per_inseguridad_empr$V1 ~ .25)

per_inseguridad_empr <- per_inseguridad_empr %>% 
  select(3, 1, 2, 9)

i_seg_patrimonial_ins_emp <- per_inseguridad_empr %>% 
  select(1:2, 4)

### enve: costo del delito

costo_delito_empr <- readxl::read_xlsx("enve_costo_delito.xlsx")

costo_delito_empr <- costo_delito_empr %>% 
  mutate(Entidad = ifelse(Entidad == "Estado de México", "México", Entidad))

costo_delito_empr <- costo_delito_empr %>% 
  mutate(Clave_ent = ifelse(Entidad == "Aguascalientes", "01",
                            ifelse(Entidad == "Baja California", "02", 
                                   ifelse(Entidad == "Baja California Sur", "03",
                                          ifelse(Entidad == "Campeche", "04",
                                                 ifelse( Entidad == "Chiapas", "07", 
                                                         ifelse(Entidad == "Chihuahua", "08",
                                                                ifelse(Entidad == "Coahuila de Zaragoza", "05",
                                                                       ifelse(Entidad == "Colima", "06",
                                                                              ifelse(Entidad == "Ciudad de México", "09",
                                                                                     ifelse(Entidad == "Durango", "10",
                                                                                            ifelse(Entidad == "Guanajuato", "11", 
                                                                                                   ifelse(Entidad == "Guerrero", "12", 
                                                                                                          ifelse(Entidad == "Hidalgo", "13", 
                                                                                                                 ifelse(Entidad == "Jalisco", "14",
                                                                                                                        ifelse(Entidad == "México", "15",
                                                                                                                               ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                      ifelse(Entidad == "Morelos", "17",
                                                                                                                                             ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                    ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                           ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                  ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                         ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                                ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                       ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                              ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                     ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                            ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                   ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                          ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                                 ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                        ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                              ))))))))))


### categorizacion con jenks 

jenks_costo_delito_empr <-getJenksBreaks(costo_delito_empr$costo_delito, 5)

jenks_costo_delito_empr <- as.data.frame(jenks_costo_delito_empr)

jenks_costo_delito_empr <- rownames_to_column(jenks_costo_delito_empr, var = "rowname")

jenks_costo_delito_empr <- jenks_costo_delito_empr %>%
  spread(key = rowname, value = jenks_costo_delito_empr, fill = 0)

costo_delito_empr <- cbind(costo_delito_empr, jenks_costo_delito_empr)

colnames(costo_delito_empr)[4:8] <- paste0("V", colnames(costo_delito_empr)[4:8])

costo_delito_empr$eva_costo_delito <- case_when(costo_delito_empr$costo_delito<=costo_delito_empr$V5 & costo_delito_empr$costo_delito>costo_delito_empr$V4 ~ 1, 
                                                costo_delito_empr$costo_delito<=costo_delito_empr$V4 & costo_delito_empr$costo_delito>costo_delito_empr$V3 ~ .75, 
                                                costo_delito_empr$costo_delito<=costo_delito_empr$V3 & costo_delito_empr$costo_delito>costo_delito_empr$V2 ~ .5, 
                                                costo_delito_empr$costo_delito<= costo_delito_empr$V2 & costo_delito_empr$costo_delito>= costo_delito_empr$V1 ~ .25)

costo_delito_empr <- costo_delito_empr %>% 
  select(3, 1, 2, 9)

i_seg_patrimonial_costo <- costo_delito_empr %>% 
  select(1:2, 4)


#### unificamos base
indicador_seg_patrimonial <- Reduce(function(x,y) merge(x=x, y=y, by = c( "Clave_ent", "Entidad")), list( incidencia_seg_patrimonial_sub, i_seg_patrimonial_costo, i_seg_patrimonial_victim, i_seg_patrimonial_ins_emp, i_seg_ejecutiva_p))

indicador_seg_patrimonial <- indicador_seg_patrimonial %>% 
  mutate(factor_riesgo_incidencia = riesgo_incidencia*.25,
         factor_costo_delito = eva_costo_delito*.25,
         factor_victimizacion_empr = eva_victimizacion_empr*.15,
         factor_per_inseguridad_empr = eva_per_inseguridad_empr*.1,
         factor_compuesto = eva_compuesto*.25)

indicador_seg_patrimonial <- indicador_seg_patrimonial %>% 
  mutate(evaluacion_riesgo = factor_riesgo_incidencia + factor_costo_delito + factor_victimizacion_empr + factor_per_inseguridad_empr + factor_compuesto )

### categorizacion con jenks 

jenks_indicador <-getJenksBreaks(indicador_seg_patrimonial$evaluacion_riesgo, 5)

jenks_indicador <- as.data.frame(jenks_indicador)

jenks_indicador <- rownames_to_column(jenks_indicador, var = "rowname")

jenks_indicador <- jenks_indicador %>%
  spread(key = rowname, value = jenks_indicador, fill = 0)

colnames(jenks_indicador)[1:5] <- paste0("V", colnames(jenks_indicador)[1:5])

indicador_seg_patrimonial$nivel_riesgo <- case_when(indicador_seg_patrimonial$evaluacion_riesgo<=jenks_indicador$V5 & indicador_seg_patrimonial$evaluacion_riesgo>jenks_indicador$V4 ~ "Riesgo muy alto", 
                                                    indicador_seg_patrimonial$evaluacion_riesgo<=jenks_indicador$V4 & indicador_seg_patrimonial$evaluacion_riesgo>jenks_indicador$V3 ~ "Riesgo alto", 
                                                    indicador_seg_patrimonial$evaluacion_riesgo<=jenks_indicador$V3 & indicador_seg_patrimonial$evaluacion_riesgo>jenks_indicador$V2 ~ "Riesgo medio", 
                                                    indicador_seg_patrimonial$evaluacion_riesgo<= jenks_indicador$V2 & indicador_seg_patrimonial$evaluacion_riesgo>=jenks_indicador$V1 ~ "Riesgo bajo")


indicador_seg_patrimonial_long <- indicador_seg_patrimonial %>% 
  select(1, 2, 15:26)

indicador_seg_patrimonial_long <- gather(indicador_seg_patrimonial_long, factor_riesgo, evaluacion, factor_d_propiedad:factor_compuesto, factor_key=TRUE)

jenks_factores_p <- ddply(indicador_seg_patrimonial_long, .(factor_riesgo), function(x) getJenksBreaks(x$evaluacion, 5))


##### mergeamos csv municipios: abrir carto .csv ####

entidad <- read_csv("/Users/aritzy/Documents/Rizika/pob-geo/entidad.csv", locale = locale())

entidad$Clave_ent<- str_pad(entidad$clave, 2, pad = 0)

## datos riesgo seguridad ejecutiva ##
carto_riesgo_seg_ejecutiva <- left_join(entidad,indicador_seg_ejecutiva,
                                        by ="Clave_ent")

carto_riesgo_seg_ejecutiva <- carto_riesgo_seg_ejecutiva %>% 
  select(1:4, 7:11, 24:37)

names(indicador_seg_ejecutiva) <- tolower(names(indicador_seg_ejecutiva))

names(carto_riesgo_seg_ejecutiva) <- tolower(names(carto_riesgo_seg_ejecutiva))
write.csv(carto_riesgo_seg_ejecutiva, file = "carto_riesgo_seg_ejecutiva.csv",
          row.names = FALSE)


## datos seguridad riesgos patrimoniales ##
carto_riesgo_seg_patrimonial <- left_join(entidad,indicador_seg_patrimonial,
                                          by ="Clave_ent")

carto_riesgo_seg_patrimonial <- carto_riesgo_seg_patrimonial %>% 
  select(1:4, 7:11, 24:37)

names(carto_riesgo_seg_patrimonial) <- tolower(names(carto_riesgo_seg_patrimonial))
write.csv(carto_riesgo_seg_patrimonial, file = "carto_riesgo_seg_patrimonial.csv",
          row.names = FALSE)


#### tablas recortadas


carto_riesgo_seg_ejecutiva <- read_csv("Documents/Rizika/I_riesgos/carto_riesgo_seg_ejecutiva.csv")

indicador_seg_ejecutiva <- carto_riesgo_seg_ejecutiva %>% 
  select(8:23)

View(indicador_seg_patrimonial)

write.csv(indicador_seg_ejecutiva, file="indicador_seg_ejecutiva.csv", fileEncoding ="UTF-8")


carto_riesgo_seg_patrimonial <- read_csv("Documents/Rizika/I_riesgos/carto_riesgo_seg_patrimonial.csv")
View(carto_riesgo_seg_patrimonial)

indicador_seg_patrimonial <- carto_riesgo_seg_patrimonial %>% 
  select(8:23)

write.csv(indicador_seg_patrimonial, file="indicador_seg_patrimonial.csv", fileEncoding ="UTF-8")







###################################### ANALISIS POR DELITO ############################
#### filtrar por delito de interes
violencia_fam_edo <-  delitos_indicadores %>% 
  filter(delito_ab=="viol_familiar") 


violencia_fam_nac <-  delitos_indicadores_nac %>% 
  filter(delito_ab=="viol_familiar") 

names(violencia_fam_nac)
violencia_fam_nac <- violencia_fam_nac %>%
  select(1,3:14)


incidencia_objetivo_t <-violencia_fam_nac  %>%
  pivot_longer(!Año, names_to = "mes", values_to = "incidencia_mes")



incidencia_objetivo_t$day <- 1

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(mes=ifelse(mes== "Enero", 1,
                    ifelse(mes== "Febrero", 2,
                           ifelse(mes=="Marzo", 3,
                                  ifelse(mes =="Abril", 4,
                                         ifelse(mes == "Mayo", 5,
                                                ifelse(mes =="Junio", 6,
                                                       ifelse(mes=="Julio", 7,
                                                              ifelse(mes=="Agosto", 8, 
                                                                     ifelse(mes=="Septiembre", 9,
                                                                            ifelse(mes=="Octubre", 10,
                                                                                   ifelse(mes=="Noviembre", 11, 12))))))))))))


p_load(lubridate, dygraphs)

incidencia_objetivo_t <-  incidencia_objetivo_t %>%  
  mutate(datetime = make_datetime(Año, mes, day)) %>% 
  arrange(datetime)

names(incidencia_objetivo_t)
incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  select(Año, datetime, incidencia_mes)

write.csv(incidencia_objetivo_t, file="violencia_fam_nact.csv", fileEncoding ="latin1")

