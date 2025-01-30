rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven, reshape2)

#### Se fijan parÃÂ¡metros del ÃÂ¡rea de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

setwd("/Users/asanchezm/Documents/")


#### 1. importar bases ####

victimas <- read_xlsx("sesnsp/datos/Estatal-Víctimas-2015-2024_dic2024/Estatal-Víctimas-2015-2024_dic2024.xlsx")

##### ---------limpieza - limpiar colnames -------- ########

names(victimas)

colnames(victimas) <-  c(
   "Año"    ,  "Clave_Ent", "Entidad", "bien_jur_afectado", "tipo_delito","subtipo","modalidad",
   "Sexo" ,    "rango_edad","Enero","Febrero","Marzo","Abril","Mayo",
   "Junio",    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"   )




##### ---------- 1.1. importar poblacion para sacar tasas de cambio--------------- ######


pob_conapo <- read_xlsx("pob/conapo/0_Pob_Inicio_1950_2070.xlsx")

names(pob_conapo)
names(pob_conapo)[2] <- "anual"
names(pob_conapo)[3] <- "Entidad"
names(pob_conapo)[4] <- "Clave_Ent"
names(pob_conapo)[7] <- "pobtot"

## agregar totales y por sexo y estado
base_pob <- aggregate(pobtot ~ anual + Clave_Ent + Entidad, data = pob_conapo, sum)

base_pob_sex <- aggregate(pobtot ~ anual + SEXO + Clave_Ent + Entidad, data = pob_conapo, sum)

base_pob_sex <- base_pob_sex %>%
  pivot_wider(names_from = SEXO, values_from = pobtot)

names(base_pob_sex)

base_pob_sex <- base_pob_sex %>% 
  select(1:2, 4:5)

base_pob <- left_join(base_pob, base_pob_sex, by = c("Clave_Ent", "anual"))

names(base_pob)[5] <- "Hombre"
names(base_pob)[6] <- "Mujer"

names(base_pob)
base_pob <- base_pob %>% 
  select(!"Entidad")

base_pob <- base_pob %>% 
  pivot_longer(!c("Clave_Ent", "anual"), names_to = "pob", values_to = "pobtot")

base_pob <- base_pob %>% 
  mutate(Sexo = ifelse(pob == "pobtot", "No identificado", pob))

# filtrar por último año
base_pob_2025 <- base_pob %>% 
  filter(anual ==2025)

# pob nac

base_pob_nac <- aggregate(pobtot ~ anual , data = pob_conapo, sum)

base_pob_nac_sex <- aggregate(pobtot ~ anual + SEXO , data = pob_conapo, sum)

base_pob_nac_sex <- base_pob_nac_sex %>%
  pivot_wider(names_from = SEXO, values_from = pobtot)

pob_nac <- left_join(base_pob_nac, base_pob_nac_sex, by = c( "anual"))

names(pob_nac)[3] <- "Hombre"
names(pob_nac)[4] <- "Mujer"

pob_nac <- pob_nac %>% 
  pivot_longer(!c( "anual"), names_to = "pob", values_to = "pobtot")

pob_nac <- pob_nac %>% 
  mutate(Sexo = ifelse(pob == "pobtot", "No identificado", pob))

# filtrar por último año
pob_nac_2025 <- pob_nac %>% 
  filter(anual ==2025)

#### 2.. procesar incidencia delictiva a nivel estatal ####
#### 
incidencia_objetivo <-  as_tibble(victimas)

names(incidencia_objetivo)

delitos_todos <- as.data.frame(unique(incidencia_objetivo$tipo_delito)) 


# seleccionar delitos de interes
#### plataforma riesgos###
# seleccionar delitos de interes
subtipos <- c("Homicidio doloso", 
              "Feminicidio",
              "Lesiones dolosas",
              "Lesiones culposas",
              "Otros delitos que atentan contra la vida y la integridad corporal" ,
              "Aborto",
              "Secuestro",
             "Tráfico de menores",                                                           
              "Rapto" ,
              "Otros delitos que atentan contra la libertad personal" ,
              "Trata de personas", 
              "Violencia de género en todas sus modalidades distinta a la violencia familiar", 
              "Incumplimiento de obligaciones de asistencia familiar"
)
incidencia_objetivo <- incidencia_objetivo %>% 
  filter(subtipo %in% subtipos)

subdelitos_subset <- as.data.frame(unique(incidencia_objetivo$subtipo)) 

delitos_subset <- as.data.frame(unique(incidencia_objetivo$tipo_delito)) 

incidencia_objetivo <- incidencia_objetivo %>% 
  mutate(delito_ab = ifelse(subtipo=="Homicidio doloso", "homicidio_dol",
                            ifelse(subtipo=="Feminicidio", "feminicidio",
                                   ifelse(subtipo=="Lesiones dolosas", "lesiones_dol",
                                          ifelse(subtipo=="Lesiones culposas", "lesiones_culp",
                                          ifelse(subtipo=="Aborto", "aborto",
                                                 ifelse(subtipo=="Trata de personas", "trata",
                                                        ifelse(subtipo=="Tráfico de menores", "trafico_menores",
                                                               ifelse(subtipo=="Rapto", "rapto",
                                                                      ifelse(subtipo=="Abuso sexual", "abuso_sexual",
                                                                             ifelse(subtipo=="Acoso sexual", "acoso_sexual",
                                                                                    ifelse(subtipo=="Hostigamiento sexual", "hostigamiento_sexual",
                                                                                           ifelse(subtipo=="Violación simple", "violacion_simple",
                                                                                                  ifelse(subtipo=="Violación equiparada", "violacion_equiparada",
                                                                                                         ifelse(subtipo=="Corrupción de menores", "corrupcion_menores",
                                                                                                                ifelse(subtipo=="Otros delitos que atentan contra la libertad personal", "otros_libertad",
                                                                                                                              ifelse(subtipo=="Otros delitos que atentan contra la vida y la integridad corporal", "otros_vida",
                                                                                                                                     ifelse(subtipo=="Secuestro", "secuestro", "ERROR:sin definir"))))))))))))))))))




#modificar sexo de delito de aborto
incidencia_objetivo <- incidencia_objetivo %>% 
  mutate(Sexo = ifelse(delito_ab == "aborto", "Mujer", Sexo))

# sumar por delito, subtipo, modalidad estatal
names(incidencia_objetivo)
meses <- colnames(incidencia_objetivo)[10:21]
incidencia_objetivo <- incidencia_objetivo %>% 
  group_by(Año, Clave_Ent, Entidad, bien_jur_afectado, 
           tipo_delito, Sexo, rango_edad, subtipo,modalidad,  delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)



#######-incidencia historica- NACIONAL##########

incidencia_historica_nac <- incidencia_objetivo %>% 
  group_by(Año, bien_jur_afectado, 
           tipo_delito, subtipo, Sexo,  delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


########  prueba para transformar a var de tiempo - BD tiempo ######
names(incidencia_objetivo)
incidencia_objetivo_nac <-incidencia_historica_nac  %>%
  pivot_longer(!c(Año, bien_jur_afectado, 
                  tipo_delito, subtipo, Sexo, delito_ab), names_to = "mes", values_to = "incidencia_mes")



incidencia_objetivo_nac$day <- 1

incidencia_objetivo_nac <- incidencia_objetivo_nac %>% 
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



incidencia_objetivo_nac <-  incidencia_objetivo_nac %>%  
  mutate(datetime = make_datetime(Año, mes, day)) %>% 
  arrange(datetime)


# agregar cols de mes y año as date
incidencia_objetivo_nac <- incidencia_objetivo_nac %>%
  mutate(mes = month(datetime),
         anual = year(datetime))

incidencia_objetivo_nac$mes_anual <- format(as.Date(incidencia_objetivo_nac$datetime), "%Y-%m")

#sumatoria por mes
incidencia_objetivo_nac <- incidencia_objetivo_nac %>%
  group_by( bien_jur_afectado, subtipo, delito_ab, Sexo, datetime, anual, mes_anual ) %>%
  dplyr::summarise(total_mensual = sum(incidencia_mes), .groups = 'drop')%>%
  mutate(prev_mes_total = lag(total_mensual),
         tc_prev_mes = (total_mensual - prev_mes_total) / total_mensual * 100)


# Calculate percentage difference with the same month in the previous year
incidencia_objetivo_nac <- incidencia_objetivo_nac %>%
  mutate(prev_mes_anual = lag(total_mensual, 12),
         tc_prev_my = (total_mensual - prev_mes_anual) / prev_mes_anual * 100)

incidencia_objetivo_nac$prev_mes_total[is.na(incidencia_objetivo_nac$prev_mes_total)]<-0
incidencia_objetivo_nac$prev_mes_anual[is.na(incidencia_objetivo_nac$prev_mes_anual)]<-0
incidencia_objetivo_nac$tc_prev_mes[is.na(incidencia_objetivo_nac$tc_prev_mes)]<-0
incidencia_objetivo_nac$tc_prev_my[is.na(incidencia_objetivo_nac$tc_prev_my)]<-0

###### unir pob para sacar tasas
incidencia_objetivo_nac <- left_join(incidencia_objetivo_nac, pob_nac, by = c("Sexo", "anual"))

names(incidencia_objetivo_nac)

incidencia_objetivo_nac <- incidencia_objetivo_nac %>% 
  mutate(mes2_t_pob = round((total_mensual/pobtot)*100000,2), 
         mes1_t_pob = round((prev_mes_total/pobtot)*100000,2))

# indicador cambio porcentual

incidencia_objetivo_nac <- incidencia_objetivo_nac %>% 
  mutate(t_crec_mensual = round(ifelse(mes1_t_pob==0 & mes2_t_pob!=0, 100,
                                       ((mes2_t_pob - mes1_t_pob)/mes1_t_pob)*100),2))
is.na(incidencia_objetivo_nac)<-sapply(incidencia_objetivo_nac, is.infinite)
incidencia_objetivo_nac[is.na(incidencia_objetivo_nac)]<-0

# fechar y guardar 
incidencia_objetivo_nac$fecha_act <- today()
write.csv(incidencia_objetivo_nac, file="sesnsp/resultados_procesados/victimas_mensual_nac.csv", fileEncoding ="latin1")

# 
# #####-----Categorización por jenks-----#####
# Da error porque varia mucho entre delitos el jenks
# p_load(BAMMtools)
# p_load(plyr)
# 
# names(incidencia_objetivo_nac)
# #Para la incidencia
# 
# jenks_crecimiento_mes <- ddply(incidencia_objetivo_nac, .(delito_ab), function(x) getJenksBreaks(x$t_crec_mensual, 5))
# 
# # write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")
# 
# 
# ##### ------ COMPARATIVA MENSUAL -----#######
# 
# # ##### 1.2 tasa de cambio mensual ####
# 
# indicador_mensual_nac <- left_join(incidencia_objetivo_nac, jenks_crecimiento_mes, by = "delito_ab")
# 
# indicador_mensual_nac <- as.data.frame(indicador_mensual_nac)
# 
# indicador_mensual_nac$eva_incidencia_tcmes2 <- case_when(indicador_mensual_nac$t_crec_mensual<=indicador_mensual_nac$V5 & indicador_mensual_nac$t_crec_mensual>indicador_mensual_nac$V4 ~ "crecimiento alto",
#                                                      indicador_mensual_nac$t_crec_mensual<=indicador_mensual_nac$V4 & indicador_mensual_nac$t_crec_mensual>=3 ~ "crecimiento medio",
#                                                      indicador_mensual_nac$t_crec_mensual==0 ~  "sin cambio",
#                                                      
#                                                      indicador_mensual_nac$t_crec_mensual>0 & indicador_mensual_nac$t_crec_mensual<=indicador_mensual_nac$V3~  "crecimiento bajo",
#                                                      indicador_mensual_nac$t_crec_mensual<0 & indicador_mensual_nac$t_crec_mensual>=indicador_mensual_nac$V2 ~ "decrecimiento bajo",
#                                                      indicador_mensual_nac$t_crec_mensual<=indicador_mensual_nac$V2 & indicador_mensual_nac$t_crec_mensual>=indicador_mensual_nac$V1 ~ "decrecimiento alto")
# 
# indicador_mensual_nac$eva_incidencia_tcmes2 <- factor(indicador_mensual_nac$eva_incidencia_tcmes2)
# 
# 
# names(indicador_mensual_nac)
# 
# indicador_mensual_nac <- indicador_mensual_nac %>%
#   select(1:15, eva_incidencia_tcmes2 )
# indicador_mensual_nac$fecha_pact <- today()
#write.csv(indicador_mensual_nac, file="indicador_victimas_mensual_estatal.csv", fileEncoding ="latin1")



####### -------------comparativo de tasas de cambio anuales------------------ ######
## crear variables de tiempo ANUALES

# mes<- format(as.Date(hoy), "%Y-%m")

mes_actual <- floor_date(Sys.Date() )

mes_pasado <- floor_date(Sys.Date() - months(1), "month")

yearBefore <- floor_date(Sys.Date() - years(1), "year")

biyearBefore <- floor_date(Sys.Date() - years(2), "year")

mes_actual <- as.Date(mes_actual)
yearBefore <- as.Date(yearBefore)
biyearBefore <- as.Date(biyearBefore)

a1 <- interval(mes_actual, yearBefore)
a2 <- interval(yearBefore, biyearBefore)

a1 <- as.list(a1)
a2 <- as.list(a2)
print(a1)

###### -----unir datos para sacar tasas de cambio anuales-----#######
#### agregar por sumatorias anuales a 12 meses
#### 

incidencia_ac_nac <- incidencia_objetivo_nac %>% 
  mutate(period = case_when(
    datetime %within% a2 ~ "total_a2",
    datetime %within% a1 ~ "total_a1"))


incidencia_ac_nac <- aggregate(total_mensual ~ period + Sexo + bien_jur_afectado + subtipo + delito_ab, data = incidencia_ac_nac, sum)

names(incidencia_ac_nac)[6] <- "total_12meses"

incidencia_ac_nac <- incidencia_ac_nac %>%
  pivot_wider(names_from = period, values_from = total_12meses)


###### 
incidencia_ac_nac <- left_join(incidencia_ac_nac, pob_nac_2025, by = c( "Sexo"))

names(incidencia_ac_nac)

incidencia_ac_nac <- incidencia_ac_nac %>% 
  mutate(a1_t_pob = round((total_a1/pobtot)*100000, 2),
         a2_t_pob = round((total_a2/pobtot)*100000,2))

# indicador cambio porcentual

incidencia_ac_nac <- incidencia_ac_nac %>% 
  mutate(t_crec_anual = round(ifelse(a1_t_pob==0 & a2_t_pob!=0, 100,
                                     ((a2_t_pob - a1_t_pob)/a1_t_pob)*100),2))
is.na(incidencia_ac_nac)<-sapply(incidencia_ac_nac, is.infinite)
incidencia_ac_nac[is.na(incidencia_ac_nac)]<-0


# fechar y guardar 
incidencia_ac_nac$fecha_act <- today()
write.csv(incidencia_ac_nac, file="sesnsp/resultados_procesados/victimas_anual_nac.csv", fileEncoding ="latin1")

#####-----Categorización por jenks-----#####
# 
# names(incidencia_ac_nac)
# #Para la incidencia
# 
# jenks_crecimiento <- ddply(incidencia_ac_nac, .(delito_ab), function(x) getJenksBreaks(x$t_crec_anual, 5))
# 
# # write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")
# 
# ##### ------ COMPARATIVA ANUAL -----#######
# 
# ##### 1.2 tasa de cambio anual ####
# 
# indicador_anual <- left_join(indicador_anual, jenks_crecimiento, by = "delito_ab")
# 
# indicador_anual <- as.data.frame(indicador_anual)
# 
# indicador_anual$eva_incidencia_tca2 <- case_when(indicador_anual$t_crec_anual<=indicador_anual$V5 & indicador_anual$t_crec_anual>indicador_anual$V4 ~ "crecimiento alto", 
#                                                  indicador_anual$t_crec_anual<=indicador_anual$V4 & indicador_anual$t_crec_anual>indicador_anual$V3 ~ "crecimiento medio", 
#                                                  indicador_anual$t_crec_anual<=indicador_anual$V3 & indicador_anual$t_crec_anual>0 ~  "crecimiento bajo",
#                                                  indicador_anual$t_crec_anual==0 ~  "sin cambio",
#                                                  indicador_anual$t_crec_anual<0 & indicador_anual$t_crec_anual>indicador_anual$V2 ~ "decrecimiento bajo", 
#                                                  indicador_anual$t_crec_anual<=indicador_anual$V2 & indicador_anual$t_crec_anual>=indicador_anual$V1 ~ "decrecimiento alto")
# 
# indicador_anual$eva_incidencia_tca2 <- factor(indicador_anual$eva_incidencia_tca2)
# 
# 
# names(indicador_anual)
# 
# indicador_anual <- indicador_anual %>% 
#   select(1:13, eva_incidencia_tca2 )
# indicador_anual$fecha_pact <- today()
# 
# write.csv(indicador_anual, file="indicador_victimas_anual_estatal.csv", fileEncoding ="latin1")

##### mergeamos csv municipios: abrir carto .csv ####



#######-incidencia historica- ESTATAL##########

incidencia_historica <- incidencia_objetivo %>% 
  group_by(Año, Clave_Ent, Entidad, bien_jur_afectado, 
           tipo_delito, subtipo, Sexo,  delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


########  prueba para transformar a var de tiempo - BD tiempo ######
names(incidencia_objetivo)
incidencia_objetivo_t <-incidencia_historica  %>%
  pivot_longer(!c(Año, Clave_Ent, Entidad, bien_jur_afectado, 
                  tipo_delito, subtipo, Sexo, delito_ab), names_to = "mes", values_to = "incidencia_mes")



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


# agregar cols de mes y año as date
incidencia_objetivo_t <- incidencia_objetivo_t %>%
  mutate(mes = month(datetime),
         anual = year(datetime))

incidencia_objetivo_t$mes_anual <- format(as.Date(incidencia_objetivo_t$datetime), "%Y-%m")

#sumatoria por mes

incidencia_objetivo_t <- incidencia_objetivo_t %>%
  group_by( Entidad,Clave_Ent,bien_jur_afectado, subtipo, delito_ab, Sexo, datetime, anual, mes_anual ) %>%
  dplyr::summarise(total_mensual = sum(incidencia_mes), .groups = 'drop')%>%
  mutate(prev_mes_total = lag(total_mensual),
         tc_prev_mes = (total_mensual - prev_mes_total) / total_mensual * 100)


# Calculate percentage difference with the same month in the previous year
incidencia_objetivo_t <- incidencia_objetivo_t %>%
  mutate(prev_mes_anual = lag(total_mensual, 12),
         tc_prev_my = (total_mensual - prev_mes_anual) / prev_mes_anual * 100)

incidencia_objetivo_t$prev_mes_total[is.na(incidencia_objetivo_t$prev_mes_total)]<-0
incidencia_objetivo_t$prev_mes_anual[is.na(incidencia_objetivo_t$prev_mes_anual)]<-0
incidencia_objetivo_t$tc_prev_mes[is.na(incidencia_objetivo_t$tc_prev_mes)]<-0
incidencia_objetivo_t$tc_prev_my[is.na(incidencia_objetivo_t$tc_prev_my)]<-0

###### unir pob para sacar tasas
incidencia_objetivo_t <- left_join(incidencia_objetivo_t, base_pob, by = c("Clave_Ent", "Sexo", "anual"))

names(incidencia_objetivo_t)

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(mes2_t_pob = round((total_mensual/pobtot)*100000,2), 
         mes1_t_pob = round((prev_mes_total/pobtot)*100000,2))

# indicador cambio porcentual

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(t_crec_mensual = round(ifelse(mes1_t_pob==0 & mes2_t_pob!=0, 100,
                                       ((mes2_t_pob - mes1_t_pob)/mes1_t_pob)*100),2))
is.na(incidencia_objetivo_t)<-sapply(incidencia_objetivo_t, is.infinite)
incidencia_objetivo_t[is.na(incidencia_objetivo_t)]<-0


#####-----Categorización por jenks-----#####

p_load(BAMMtools)
p_load(plyr)

names(incidencia_objetivo_t)
#Para la incidencia
jenks_mes <- ddply(incidencia_objetivo_t, .(delito_ab), function(x) getJenksBreaks(x$mes2_t_pob, 5))

jenks_crecimiento_mes <- ddply(incidencia_objetivo_t, .(delito_ab), function(x) getJenksBreaks(x$tc_prev_mes, 5))

# write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")


##### ------ COMPARATIVA MENSUAL -----#######

##### 1. tasa mensual ###

indicador_mensual <- left_join(incidencia_objetivo_t, jenks_mes, by = "delito_ab")

indicador_mensual <- as.data.frame(indicador_mensual)

indicador_mensual$eva_n_incidencia_m2 <- case_when(indicador_mensual$mes2_t_pob<=indicador_mensual$V5 & indicador_mensual$mes2_t_pob>indicador_mensual$V4 ~ 5, 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V4 & indicador_mensual$mes2_t_pob>indicador_mensual$V3 ~ 4, 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V3 & indicador_mensual$mes2_t_pob>indicador_mensual$V2 ~ 3, 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V2 & indicador_mensual$mes2_t_pob>indicador_mensual$V1 ~ 2,
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V1 & indicador_mensual$mes2_t_pob>0 ~ 1,
                                                 indicador_mensual$mes2_t_pob==0 ~ 0)

indicador_mensual$eva_n_incidencia_m2 <- factor(indicador_mensual$eva_n_incidencia_m2)

indicador_mensual$eva_incidencia_m2 <- case_when(indicador_mensual$mes2_t_pob<=indicador_mensual$V5 & indicador_mensual$mes2_t_pob>indicador_mensual$V4 ~ "incidencia muy alta", 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V4 & indicador_mensual$mes2_t_pob>indicador_mensual$V3 ~ "incidencia alta", 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V3 & indicador_mensual$mes2_t_pob>indicador_mensual$V2 ~ "incidencia media", 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V2 & indicador_mensual$mes2_t_pob>indicador_mensual$V1 ~ "incidencia baja",
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V1 & indicador_mensual$mes2_t_pob>0 ~ "incidencia muy baja",
                                                 indicador_mensual$mes2_t_pob==0 ~ "sin incidencia reportada")

indicador_mensual$eva_incidencia_m2 <- factor(indicador_mensual$eva_incidencia_m2)

# ##### 1.2 tasa de cambio mensual ####
names(indicador_mensual)

indicador_mensual <- indicador_mensual %>%
  select(1:19,eva_n_incidencia_m2,  eva_incidencia_m2 )

indicador_mensual <- left_join(indicador_mensual, jenks_crecimiento_mes, by = "delito_ab")

indicador_mensual <- as.data.frame(indicador_mensual)

indicador_mensual$eva_incidencia_tcmes2 <- case_when(indicador_mensual$t_crec_mensual<=indicador_mensual$V5 & indicador_mensual$t_crec_mensual>indicador_mensual$V4 ~ "crecimiento alto",
                                                 indicador_mensual$t_crec_mensual<=indicador_mensual$V4 & indicador_mensual$t_crec_mensual>0 ~ "crecimiento medio",
                                                 indicador_mensual$t_crec_mensual==0 ~  "sin cambio",
                                                 
                                                 indicador_mensual$t_crec_mensual<0 & indicador_mensual$t_crec_mensual>=indicador_mensual$V3~  "decrecimiento bajo",
                                                 indicador_mensual$t_crec_mensual<=indicador_mensual$V3 & indicador_mensual$t_crec_mensual>=indicador_mensual$V2 ~ "decrecimiento medio",
                                                 indicador_mensual$t_crec_mensual<=indicador_mensual$V2 & indicador_mensual$t_crec_mensual>=indicador_mensual$V1 ~ "decrecimiento alto")

indicador_mensual$eva_incidencia_tcmes2 <- factor(indicador_mensual$eva_incidencia_tcmes2)


names(indicador_mensual)

indicador_mensual <- indicador_mensual %>%
  select(1:21, eva_incidencia_tcmes2 )
indicador_mensual$fecha_act <- today()


write.csv(indicador_mensual, file="sesnsp/resultados_procesados/victimas_mensual_estatal.csv", fileEncoding ="latin1")

####### -------------comparativo de tasas de cambio anuales------------------ ######
## crear variables de tiempo ANUALES

# mes<- format(as.Date(hoy), "%Y-%m")

mes_actual <- floor_date(Sys.Date() )

mes_pasado <- floor_date(Sys.Date() - months(1), "month")

yearBefore <- floor_date(Sys.Date() - years(1), "year")

biyearBefore <- floor_date(Sys.Date() - years(2), "year")

mes_actual <- as.Date(mes_actual)
yearBefore <- as.Date(yearBefore)
biyearBefore <- as.Date(biyearBefore)

a1 <- interval(mes_actual, yearBefore)
a2 <- interval(yearBefore, biyearBefore)

a1 <- as.list(a1)
a2 <- as.list(a2)
print(a1)

###### -----unir datos para sacar tasas de cambio anuales-----#######
#### agregar por sumatorias anuales a 12 meses
#### 


incidencia_ac1 <- incidencia_objetivo_t %>% 
  mutate(period = case_when(
    datetime %within% a2 ~ "total_a2",
    datetime %within% a1 ~ "total_a1"))


incidencia_ac1 <- aggregate(total_mensual ~ period + Clave_Ent + Entidad + Sexo + bien_jur_afectado + subtipo + delito_ab, data = incidencia_ac1, sum)

names(incidencia_ac1)[8] <- "total_12meses"

incidencia_ac1 <- incidencia_ac1 %>%
  pivot_wider(names_from = period, values_from = total_12meses)


###### 
incidencia_ac1 <- left_join(incidencia_ac1, base_pob_2025, by = c("Clave_Ent", "Sexo"))

names(incidencia_ac1)

incidencia_ac1 <- incidencia_ac1 %>% 
  mutate(a1_t_pob = round((total_a1/pobtot)*100000, 2),
         a2_t_pob = round((total_a2/pobtot)*100000,2))

# indicador cambio porcentual

incidencia_ac1 <- incidencia_ac1 %>% 
  mutate(t_crec_anual = round(ifelse(a1_t_pob==0 & a2_t_pob!=0, 100,
                                     ((a2_t_pob - a1_t_pob)/a1_t_pob)*100),2))
is.na(incidencia_ac1)<-sapply(incidencia_ac1, is.infinite)
incidencia_ac1[is.na(incidencia_ac1)]<-0

#####-----Categorización por jenks-----#####

names(incidencia_objetivo_t)
#Para la incidencia

jenks_a2 <- ddply(incidencia_ac1, .(delito_ab), function(x) getJenksBreaks(x$a2_t_pob, 5))

jenks_crecimiento <- ddply(incidencia_ac1, .(delito_ab), function(x) getJenksBreaks(x$t_crec_anual, 5))

# write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")

##### ------ COMPARATIVA ANUAL -----#######
##### 1. tasa anual ###

indicador_anual <- left_join(incidencia_ac1, jenks_a2, by = "delito_ab")

indicador_anual <- as.data.frame(indicador_anual)

indicador_anual$eva_n_incidencia_a2 <- case_when(indicador_anual$a2_t_pob<=indicador_anual$V5 & indicador_anual$a2_t_pob>indicador_anual$V4 ~ 1, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V4 & indicador_anual$a2_t_pob>indicador_anual$V3 ~ .75, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V3 & indicador_anual$a2_t_pob>indicador_anual$V2 ~ .5, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V2 & indicador_anual$a2_t_pob>0 ~ .25, 
                                               indicador_anual$a2_t_pob==0 ~ 0)

indicador_anual$eva_n_incidencia_a2 <- factor(indicador_anual$eva_n_incidencia_a2)

indicador_anual$eva_incidencia_a2 <- case_when(indicador_anual$a2_t_pob<=indicador_anual$V5 & indicador_anual$a2_t_pob>indicador_anual$V4 ~ "incidencia muy alta", 
                                                 indicador_anual$a2_t_pob<=indicador_anual$V4 & indicador_anual$a2_t_pob>indicador_anual$V3 ~ "incidencia alta", 
                                                 indicador_anual$a2_t_pob<=indicador_anual$V3 & indicador_anual$a2_t_pob>indicador_anual$V2 ~ "incidencia media", 
                                                 indicador_anual$a2_t_pob<=indicador_anual$V2 & indicador_anual$a2_t_pob>indicador_anual$V1 ~ "incidencia baja",
                                                 indicador_anual$a2_t_pob<=indicador_anual$V1 & indicador_anual$a2_t_pob>0 ~ "incidencia muy baja",
                                                 indicador_anual$a2_t_pob==0 ~ "sin incidencia reportada")

indicador_anual$eva_incidencia_a2 <- factor(indicador_anual$eva_incidencia_a2)


##### 1.2 tasa de cambio anual ####
names(indicador_anual)

indicador_anual <- indicador_anual %>% 
  select(1:14, eva_n_incidencia_a2, eva_incidencia_a2 )

indicador_anual <- left_join(indicador_anual, jenks_crecimiento, by = "delito_ab")

indicador_anual <- as.data.frame(indicador_anual)

indicador_anual$eva_incidencia_tca2 <- case_when(indicador_anual$t_crec_anual<=indicador_anual$V5 & indicador_anual$t_crec_anual>indicador_anual$V4 ~ "crecimiento alto", 
                                                 indicador_anual$t_crec_anual<=indicador_anual$V4 & indicador_anual$t_crec_anual>indicador_anual$V3 ~ "crecimiento medio", 
                                                 indicador_anual$t_crec_anual<=indicador_anual$V3 & indicador_anual$t_crec_anual>0 ~  "crecimiento bajo",
                                                 indicador_anual$t_crec_anual==0 ~  "sin cambio",
                                                 indicador_anual$t_crec_anual<0 & indicador_anual$t_crec_anual>indicador_anual$V2 ~ "decrecimiento bajo", 
                                                 indicador_anual$t_crec_anual<=indicador_anual$V2 & indicador_anual$t_crec_anual>=indicador_anual$V1 ~ "decrecimiento alto")

indicador_anual$eva_incidencia_tca2 <- factor(indicador_anual$eva_incidencia_tca2)


names(indicador_anual)

indicador_anual <- indicador_anual %>% 
  select(1:16, eva_incidencia_tca2 )
indicador_anual$fecha_act <- today()

write.csv(indicador_anual, file="sesnsp/resultados_procesados/victimas_anual_estatal.csv", fileEncoding ="latin1")

##### mergeamos csv municipios: abrir carto .csv ####

