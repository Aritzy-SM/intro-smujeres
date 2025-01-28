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

incidencia  <- read_excel("incidencia delictiva/datos/Estatal-Delitos-2015-2024_dic2024/Estatal-Delitos-2015-2024_dic2024.xlsx")

victimas <- read_xlsx("incidencia delictiva/datos/Estatal-Víctimas-2015-2024_dic2024/Estatal-Víctimas-2015-2024_dic2024.xlsx")

##### ---------- 1.1. importar poblacion para sacar tasas de cambio--------------- ######


pob_conapo <- read_xlsx("pob/conapo/0_Pob_Inicio_1950_2070.xlsx")

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

names(base_pob)[5] <- "pobtot_hombres"
names(base_pob)[6] <- "pobtot_mujeres"

names(base_pob)
base_pob <- base_pob %>% 
  select(!"Entidad")

base_pob_2025 <- base_pob %>% 
  filter(anual ==2025)

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

tot_mensual_nac <- incidencia_objetivo %>% 
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
incidencia_objetivo_t <- left_join(incidencia_objetivo_t, base_pob_2025, by = c("Clave_Ent"))

names(incidencia_objetivo_t)

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(mes2_t_pob = round((total_mensual/pobtot_mujeres)*100000,2), 
         mes1_t_pob = round((prev_mes_total/pobtot_mujeres)*100000,2))

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
jenks_mes <- ddply(incidencia_objetivo_t, .(delito_ab), function(x) getJenksBreaks(x$total_mensual, 5))

#jenks_crecimieto_mes <- ddply(incidencia_objetivo_t, .(delito_ab), function(x) getJenksBreaks(x$tc_prev_mes, 5))

# write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")


##### ------ COMPARATIVA MENSUAL -----#######

##### 1. tasa mensual ###

indicador_mensual <- left_join(incidencia_objetivo_t, jenks_mes, by = "delito_ab")

indicador_mensual <- as.data.frame(indicador_mensual)

indicador_mensual$eva_incidencia_m2 <- case_when(indicador_mensual$mes2_t_pob<=indicador_mensual$V5 & indicador_mensual$mes2_t_pob>indicador_mensual$V4 ~ 1, 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V4 & indicador_mensual$mes2_t_pob>indicador_mensual$V3 ~ .75, 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V3 & indicador_mensual$mes2_t_pob>indicador_mensual$V2 ~ .5, 
                                                 indicador_mensual$mes2_t_pob<=indicador_mensual$V2 & indicador_mensual$mes2_t_pob>0 ~ .25, 
                                                 indicador_mensual$mes2_t_pob==0 ~ 0)

indicador_mensual$eva_incidencia_m2 <- factor(indicador_mensual$eva_incidencia_m2)

# ##### 1.2 tasa de cambio mensual ####
# names(indicador_mensual)
# 
# indicador_mensual <- indicador_mensual %>% 
#   select(1:12, eva_incidencia_a2 )
# 
# indicador_mensual <- left_join(indicador_mensual, jenks_crecimiento, by = "delito_ab")
# 
# indicador_mensual <- as.data.frame(indicador_mensual)
# 
# indicador_mensual$eva_incidencia_tca2 <- case_when(indicador_mensual$t_crec_anual<=indicador_mensual$V5 & indicador_mensual$t_crec_anual>indicador_mensual$V4 ~ "crecimiento alto", 
#                                                  indicador_mensual$t_crec_anual<=indicador_mensual$V4 & indicador_mensual$t_crec_anual>indicador_mensual$V3 ~ "crecimiento medio", 
#                                                  indicador_mensual$t_crec_anual<=indicador_mensual$V3 & indicador_mensual$t_crec_anual>0 ~  "crecimiento bajo",
#                                                  indicador_mensual$t_crec_anual==0 ~  "sin cambio",
#                                                  indicador_mensual$t_crec_anual<0 & indicador_mensual$t_crec_anual>indicador_mensual$V2 ~ "decrecimiento bajo", 
#                                                  indicador_mensual$t_crec_anual<=indicador_mensual$V2 & indicador_mensual$t_crec_anual>=indicador_mensual$V1 ~ "decrecimiento alto")
# 
# indicador_mensual$eva_incidencia_tca2 <- factor(indicador_mensual$eva_incidencia_tca2)
# 

names(indicador_mensual)

indicador_mensual <- indicador_mensual %>%
  select(1:11, eva_incidencia_m2 )
indicador_mensual$fecha_pact <- today()


write.csv(indicador_mensual, file="indicador_mensual.csv", fileEncoding ="latin1")

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


incidencia_ac1 <- aggregate(total_mensual ~ period + Clave_Ent + Entidad + delito_ab, data = incidencia_ac1, sum)

names(incidencia_ac1)[5] <- "total_12meses"

incidencia_ac1 <- incidencia_ac1 %>%
  pivot_wider(names_from = period, values_from = total_12meses)


###### 
incidencia_ac1 <- left_join(incidencia_ac1, base_pob_2025, by = c("Clave_Ent"))

names(incidencia_ac1)

incidencia_ac1 <- incidencia_ac1 %>% 
  mutate(a1_t_pob = round((total_a1/pobtot_mujeres)*100000, 2),
         a2_t_pob = round((total_a2/pobtot_mujeres)*100000,2))

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

indicador_anual$eva_incidencia_a2 <- case_when(indicador_anual$a2_t_pob<=indicador_anual$V5 & indicador_anual$a2_t_pob>indicador_anual$V4 ~ 1, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V4 & indicador_anual$a2_t_pob>indicador_anual$V3 ~ .75, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V3 & indicador_anual$a2_t_pob>indicador_anual$V2 ~ .5, 
                                               indicador_anual$a2_t_pob<=indicador_anual$V2 & indicador_anual$a2_t_pob>0 ~ .25, 
                                               indicador_anual$a2_t_pob==0 ~ 0)

indicador_anual$eva_incidencia_a2 <- factor(indicador_anual$eva_incidencia_a2)

##### 1.2 tasa de cambio anual ####
names(indicador_anual)

indicador_anual <- indicador_anual %>% 
  select(1:12, eva_incidencia_a2 )

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
  select(1:13, eva_incidencia_tca2 )
indicador_anual$fecha_pact <- today()

write.csv(indicador_anual, file="indicador_anual.csv", fileEncoding ="latin1")

##### mergeamos csv municipios: abrir carto .csv ####

