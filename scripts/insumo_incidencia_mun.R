rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven, reshape2, BAMMtools)

#### Se fijan parÃÂ¡metros del ÃÂ¡rea de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")

#Sys.setlocale("LC_ALL", "pt_PT.ISO8859-1")

options(scipen=999)

##########################################################

setwd("/Users/asanchezm/Documents/")


#### 1. importar bases ####

incidencia  <- read_excel("sesnsp/datos/Municipal-Delitos-2015-2024_dic2024/Municipal-Delitos-2015-2024_dic2024/2024.xlsx")

incidencia_23  <- read_excel("sesnsp/datos/Municipal-Delitos-2015-2024_dic2024/Municipal-Delitos-2015-2024_dic2024/2023.xlsx")

names(incidencia)

colnames(incidencia) <-  c(
  "Año"    ,  "Clave_Ent", "Entidad", "cve_mun", "mun", "bien_jur_afectado", "tipo_delito","subtipo","modalidad",
  "Enero","Febrero","Marzo","Abril","Mayo",
  "Junio",    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"   )


colnames(incidencia_23) <-  c(
  "Año"    ,  "Clave_Ent", "Entidad", "cve_mun", "mun", "bien_jur_afectado", "tipo_delito","subtipo","modalidad",
  "Enero","Febrero","Marzo","Abril","Mayo",
  "Junio",    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"   )

incidencia <- rbind( incidencia_23, incidencia)

# victimas <-  read_delim("sesnsp/datos/Municipal-Delitos-2015-2024_dic2024/Municipal-Delitos-2015-2024_dic2024/Municipal-Delitos-2015-2024_dic2024.csv",  locale = locale(encoding = "latin1"))

##### ---------- 1.1. importar poblacion para sacar tasas de cambio--------------- ######


pob_estatal <- read_excel("pob/estatal/15_Mexico/3_Indicadores_Dem_15_MX.xlsx")

pob_estatal <- pob_estatal %>% 
  filter(AÑO == 2024) %>% 
  select(CLAVE, NOM_MUN,HOM_MIT_AÑO, MUJ_MIT_AÑO, POB_MIT_MUN, RHM )

names(pob_estatal)


#### 2.. procesar incidencia delictiva a nivel estatal ####
#### 
incidencia_objetivo <-  as_tibble(incidencia)

names(incidencia_objetivo)

delitos_todos <- as.data.frame(unique(incidencia_objetivo$`Tipo de delito`)) 


# seleccionar delitos de interes
#### plataforma riesgos###
# seleccionar delitos de interes
subtipos <- c(
  
  #"Homicidio doloso", 
              "Feminicidio",
   #           "Lesiones dolosas",
              "Aborto",
    #          "Secuestro",
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
  filter(subtipo %in% subtipos)


delitos_subset <- as.data.frame(unique(incidencia_objetivo$`Tipo de delito`)) 

incidencia_objetivo <- incidencia_objetivo %>% 
  mutate(delito_ab = ifelse(subtipo=="Homicidio doloso", "homicidio_dol",
                            ifelse(subtipo=="Feminicidio", "feminicidio",
                                   ifelse(subtipo=="Lesiones dolosas", "lesiones_dol",
                                          ifelse(subtipo=="Aborto", "aborto",
                                                 ifelse(subtipo=="Secuestro", "secuestro",
                                                        ifelse(subtipo=="Tráfico de menores", "trafico_menores",
                                                               ifelse(subtipo=="Rapto", "rapto",
                                                                      ifelse(subtipo=="Abuso sexual", "abuso_sexual",
                                                                             ifelse(subtipo=="Acoso sexual", "acoso_sexual",
                                                                                    ifelse(subtipo=="Hostigamiento sexual", "hostigamiento_sexual",
                                                                                           ifelse(subtipo=="Violación simple", "violacion_simple",
                                                                                                  ifelse(subtipo=="Violación equiparada", "violacion_equiparada",
                                                                                                         ifelse(subtipo=="Incesto", "incesto",
                                                                                                                ifelse(subtipo=="Otros delitos que atentan contra la libertad y la seguridad sexual", "otros_seg_sex",
                                                                                                                       ifelse(subtipo=="Violencia familiar", "viol_familiar",
                                                                                                                              ifelse(subtipo=="Violencia de género en todas sus modalidades distinta a la violencia familiar", "viol_genero",
                                                                                                                                     ifelse(subtipo=="Incumplimiento de obligaciones de asistencia familiar", "asistencia_familiar", "ERROR:sin definir"))))))))))))))))))


# sumar por delito, subtipo, modalidad estatal
names(incidencia_objetivo)
meses <- colnames(incidencia_objetivo)[10:21]
incidencia_objetivo <- incidencia_objetivo %>% 
  group_by(Año, Clave_Ent, Entidad, cve_mun, mun, bien_jur_afectado, 
           tipo_delito, subtipo,modalidad,  delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


#######-incidencia historica- MUNICIPAL##########

incidencia_historica <- incidencia_objetivo %>% 
  group_by(Año, Clave_Ent, Entidad, cve_mun, mun, bien_jur_afectado, 
           tipo_delito, subtipo,  delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


########  prueba para transformar a var de tiempo - BD tiempo ######
names(incidencia_objetivo)
incidencia_objetivo_t <-incidencia_historica  %>%
  pivot_longer(!c(Año, Clave_Ent, Entidad,cve_mun, mun, bien_jur_afectado, 
                  tipo_delito, subtipo, delito_ab), names_to = "mes", values_to = "incidencia_mes")



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
  group_by( Entidad,Clave_Ent,cve_mun, mun,bien_jur_afectado, subtipo, delito_ab, datetime, anual, mes_anual ) %>%
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



### filtro para eEDOMEX
incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  filter(Clave_Ent == 15)

incidencia_objetivo_t$CLAVE =  incidencia_objetivo_t$cve_mun

incidencia_objetivo_t$CLAVE <-  as.numeric(incidencia_objetivo_t$CLAVE)

###### unir pob para sacar tasas
incidencia_objetivo_t <- left_join(incidencia_objetivo_t, pob_estatal, by = c("CLAVE"))

names(incidencia_objetivo_t)

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(mes2_t_pobm = round((total_mensual/MUJ_MIT_AÑO)*100000,2), 
         mes1_t_pobm = round((prev_mes_total/MUJ_MIT_AÑO)*100000,2))

# indicador cambio porcentual

incidencia_objetivo_t <- incidencia_objetivo_t %>% 
  mutate(t_crec_mensual = round(ifelse(mes1_t_pobm==0 & mes2_t_pobm!=0, 100,
                                       ((mes2_t_pobm - mes1_t_pobm)/mes1_t_pobm)*100),2))
is.na(incidencia_objetivo_t)<-sapply(incidencia_objetivo_t, is.infinite)
incidencia_objetivo_t[is.na(incidencia_objetivo_t)]<-0


#####-----Categorización por jenks-----#####
# 
# p_load(BAMMtools)
# p_load(plyr)
# 
# names(incidencia_objetivo_t)
# #Para la incidencia
# jenks_mes <- ddply(incidencia_objetivo_t, .(delito_ab), function(x) getJenksBreaks(x$mes2_t_pobm, 5))
# 
# jenks_crecimiento_mes <- ddply(incidencia_objetivo_t, .(delito_ab), function(x) getJenksBreaks(x$tc_prev_mes, 5))
# 
# # write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")
# 
# 
# ##### ------ COMPARATIVA MENSUAL -----#######
# 
# ##### 1. tasa mensual ###
# 
# indicador_mensual <- left_join(incidencia_objetivo_t, jenks_mes, by = "delito_ab")
# 
# indicador_mensual <- as.data.frame(indicador_mensual)
# 
# indicador_mensual$eva_n_incidencia_m2 <- case_when(indicador_mensual$mes2_t_pobm<=indicador_mensual$V5 & indicador_mensual$mes2_t_pobm>indicador_mensual$V4 ~ 5, 
#                                                    indicador_mensual$mes2_t_pobm<=indicador_mensual$V4 & indicador_mensual$mes2_t_pobm>indicador_mensual$V3 ~ 4, 
#                                                    indicador_mensual$mes2_t_pobm<=indicador_mensual$V3 & indicador_mensual$mes2_t_pobm>indicador_mensual$V2 ~ 3, 
#                                                    indicador_mensual$mes2_t_pobm<=indicador_mensual$V2 & indicador_mensual$mes2_t_pobm>indicador_mensual$V1 ~ 2,
#                                                    indicador_mensual$mes2_t_pobm<=indicador_mensual$V1 & indicador_mensual$mes2_t_pobm>0 ~ 1,
#                                                    indicador_mensual$mes2_t_pobm==0 ~ 0)
# 
# indicador_mensual$eva_n_incidencia_m2 <- factor(indicador_mensual$eva_n_incidencia_m2)
# 
# indicador_mensual$eva_incidencia_m2 <- case_when(indicador_mensual$mes2_t_pobm<=indicador_mensual$V5 & indicador_mensual$mes2_t_pobm>indicador_mensual$V4 ~ "incidencia muy alta", 
#                                                  indicador_mensual$mes2_t_pobm<=indicador_mensual$V4 & indicador_mensual$mes2_t_pobm>indicador_mensual$V3 ~ "incidencia alta", 
#                                                  indicador_mensual$mes2_t_pobm<=indicador_mensual$V3 & indicador_mensual$mes2_t_pobm>indicador_mensual$V2 ~ "incidencia media", 
#                                                  indicador_mensual$mes2_t_pobm<=indicador_mensual$V2 & indicador_mensual$mes2_t_pobm>indicador_mensual$V1 ~ "incidencia baja",
#                                                  indicador_mensual$mes2_t_pobm<=indicador_mensual$V1 & indicador_mensual$mes2_t_pobm>0 ~ "incidencia muy baja",
#                                                  indicador_mensual$mes2_t_pobm==0 ~ "sin incidencia reportada")
# 
# indicador_mensual$eva_incidencia_m2 <- factor(indicador_mensual$eva_incidencia_m2)
# 
# # ##### 1.2 tasa de cambio mensual ####
# names(indicador_mensual)
# 
# indicador_mensual <- indicador_mensual %>%
#   select(1:19,eva_n_incidencia_m2,  eva_incidencia_m2 )
# 
# indicador_mensual <- left_join(indicador_mensual, jenks_crecimiento_mes, by = "delito_ab")
# 
# indicador_mensual <- as.data.frame(indicador_mensual)
# 
# indicador_mensual$eva_incidencia_tcmes2 <- case_when(indicador_mensual$t_crec_mensual<=indicador_mensual$V5 & indicador_mensual$t_crec_mensual>indicador_mensual$V4 ~ "crecimiento alto",
#                                                      indicador_mensual$t_crec_mensual<=indicador_mensual$V4 & indicador_mensual$t_crec_mensual>0 ~ "crecimiento medio",
#                                                      indicador_mensual$t_crec_mensual==0 ~  "sin cambio",
#                                                      
#                                                      indicador_mensual$t_crec_mensual<0 & indicador_mensual$t_crec_mensual>=indicador_mensual$V3~  "decrecimiento bajo",
#                                                      indicador_mensual$t_crec_mensual<=indicador_mensual$V3 & indicador_mensual$t_crec_mensual>=indicador_mensual$V2 ~ "decrecimiento medio",
#                                                      indicador_mensual$t_crec_mensual<=indicador_mensual$V2 & indicador_mensual$t_crec_mensual>=indicador_mensual$V1 ~ "decrecimiento alto")
# 
# indicador_mensual$eva_incidencia_tcmes2 <- factor(indicador_mensual$eva_incidencia_tcmes2)


names(incidencia_objetivo_t)

indicador_mensual <- incidencia_objetivo_t %>%
  select(1:8, total_mensual, prev_mes_total, tc_prev_mes, prev_mes_anual, tc_prev_my , HOM_MIT_AÑO, MUJ_MIT_AÑO, POB_MIT_MUN, RHM, mes2_t_pobm, mes1_t_pobm, t_crec_mensual)
indicador_mensual$fecha_act <- today()


write.csv(indicador_mensual, file="sesnsp/resultados_procesados/victimas_mensual_estatal_edomex.csv", fileEncoding ="latin1")

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

a2 <- interval(mes_actual, yearBefore)
a1 <- interval(yearBefore, biyearBefore)

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


incidencia_ac1 <- aggregate(total_mensual ~ period + Clave_Ent + Entidad + cve_mun + mun+ bien_jur_afectado + subtipo + delito_ab + HOM_MIT_AÑO + MUJ_MIT_AÑO + POB_MIT_MUN, data = incidencia_ac1, sum)
names(incidencia_ac1)

names(incidencia_ac1)[12] <- "total_12meses"

incidencia_ac1 <- incidencia_ac1 %>%
  pivot_wider(names_from = period, values_from = total_12meses)


###### 

names(incidencia_ac1)

incidencia_ac1 <- incidencia_ac1 %>% 
  mutate(a1_t_pobm = round((total_a1/MUJ_MIT_AÑO)*100000, 2),
         a2_t_pobm = round((total_a2/MUJ_MIT_AÑO)*100000,2))

# indicador cambio porcentual

incidencia_ac1 <- incidencia_ac1 %>% 
  mutate(t_crec_anual = round(ifelse(a1_t_pobm==0 & a2_t_pobm!=0, 100,
                                     ((a2_t_pobm - a1_t_pobm)/a1_t_pobm)*100),2))
is.na(incidencia_ac1)<-sapply(incidencia_ac1, is.infinite)
incidencia_ac1[is.na(incidencia_ac1)]<-0

# #####-----Categorización por jenks-----#####
# 
# names(incidencia_objetivo_t)
# #Para la incidencia
# 
# jenks_a2 <- ddply(incidencia_ac1, .(delito_ab), function(x) getJenksBreaks(x$a2_t_pobm, 5))
# 
# unique(incidencia_objetivo_t$delito_ab)
# 
# 
# natural_breaks <- function(df, var) {
#   var_breaks <- BAMMtools::getJenksBreaks(df[[var]], k = 6)
#   df[[paste0('jenks_', var)]] <- findInterval(df[[var]], var_breaks)
#   df
# }
# 
# vfam <-  natural_breaks(incidencia_objetivo_t, "delito_ab$viol_familiar") 
# 
# 
# 
# 
# jenks_crecimiento <- ddply(incidencia_ac1, .(delito_ab), function(x) getJenksBreaks(x$t_crec_anual, 5))
# 
# # write.csv(jenks_a2, file="jenks_anual_sep_2020.csv", fileEncoding ="UTF-8")
# 
# ##### ------ COMPARATIVA ANUAL -----#######
# ##### 1. tasa anual ###
# 
# indicador_anual <- left_join(incidencia_ac1, jenks_a2, by = "delito_ab")
# 
# indicador_anual <- as.data.frame(indicador_anual)
# 
# indicador_anual$eva_n_incidencia_a2 <- case_when(indicador_anual$a2_t_pobm<=indicador_anual$V5 & indicador_anual$a2_t_pobm>indicador_anual$V4 ~ 1, 
#                                                  indicador_anual$a2_t_pobm<=indicador_anual$V4 & indicador_anual$a2_t_pobm>indicador_anual$V3 ~ .75, 
#                                                  indicador_anual$a2_t_pobm<=indicador_anual$V3 & indicador_anual$a2_t_pobm>indicador_anual$V2 ~ .5, 
#                                                  indicador_anual$a2_t_pobm<=indicador_anual$V2 & indicador_anual$a2_t_pobm>0 ~ .25, 
#                                                  indicador_anual$a2_t_pobm==0 ~ 0)
# 
# indicador_anual$eva_n_incidencia_a2 <- factor(indicador_anual$eva_n_incidencia_a2)
# 
# indicador_anual$eva_incidencia_a2 <- case_when(indicador_anual$a2_t_pobm<=indicador_anual$V5 & indicador_anual$a2_t_pobm>indicador_anual$V4 ~ "incidencia muy alta", 
#                                                indicador_anual$a2_t_pobm<=indicador_anual$V4 & indicador_anual$a2_t_pobm>indicador_anual$V3 ~ "incidencia alta", 
#                                                indicador_anual$a2_t_pobm<=indicador_anual$V3 & indicador_anual$a2_t_pobm>indicador_anual$V2 ~ "incidencia media", 
#                                                indicador_anual$a2_t_pobm<=indicador_anual$V2 & indicador_anual$a2_t_pobm>indicador_anual$V1 ~ "incidencia baja",
#                                                indicador_anual$a2_t_pobm<=indicador_anual$V1 & indicador_anual$a2_t_pobm>0 ~ "incidencia muy baja",
#                                                indicador_anual$a2_t_pobm==0 ~ "sin incidencia reportada")
# 
# indicador_anual$eva_incidencia_a2 <- factor(indicador_anual$eva_incidencia_a2)
# 
# 
# ##### 1.2 tasa de cambio anual ####
# names(indicador_anual)
# 
# indicador_anual <- indicador_anual %>% 
#   select(1:14, eva_n_incidencia_a2, eva_incidencia_a2 )
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

names(incidencia_ac1)

incidencia_ac1$fecha_act <- today()
write.csv(incidencia_ac1, file="sesnsp/resultados_procesados/victimas_anual_estatal_edomex.csv", fileEncoding ="latin1")


####### filtro por delito de interés

vida <- incidencia_ac1 %>% 
  filter(bien_jur_afectado == "La vida y la Integridad corporal" )

# graf edo

gg_edo <- 
  ggplot(vida, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
            
             nudge_y =.5,
             size = 2) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la vida de las mujeres en Edomex" ) +
  ylab("Tasa p/c cien mil habs del municipio")

gg_edo
####### filtro por muns de interés

vida_mun <- vida %>% 
  filter(mun == "La Paz" |mun == "Chimalhuacán" |mun == "Nezahualcóyotl" |mun == "Nezahualcóyotl"  |mun == "Valle de Chalco Solidaridad" |mun == "Ixtapaluca" 
         |mun == "Chicoloapan" |mun == "Texcoco" |mun == "Ecatepec de Morelos" )

# graf edo

gg_mun <- 
  ggplot(vida_mun, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la vida de las mujeres muns de interés" ) +
  ylab("Tasa p/c cien mil habs del municipio")

gg_mun

gg_mun_comp <- 
  ggplot(vida_mun, aes(x = t_crec_anual, y = mun, size = t_crec_anual)) +
  geom_point(aes( col = t_crec_anual), alpha = 0.5) +
  
  geom_label(aes(label = t_crec_anual),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("green","red")) +
  ggtitle("Crecimiento de delitos que atentan conta la vida de las mujeres muns de interés" ) +
  xlab("Tasa de crecimiento con respecto al año previo")

gg_mun_comp


##"La libertad y la seguridad sexual"   
lsex <- incidencia_ac1 %>% 
  filter(bien_jur_afectado == "La libertad y la seguridad sexual" )

# graf edo

gg_edo <- 
  ggplot(lsex, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la seguridad sexual de las mujeres en Edomex" ) +
  xlab("Tasa p/c cien mil habs del municipio")

gg_edo
####### filtro por muns de interés

lsex_mun <- lsex %>% 
  filter(mun == "La Paz" |mun == "Chimalhuacán" |mun == "Nezahualcóyotl" |mun == "Nezahualcóyotl"  |mun == "Valle de Chalco Solidaridad" |mun == "Ixtapaluca" 
         |mun == "Chicoloapan" |mun == "Texcoco" |mun == "Ecatepec de Morelos" )

# graf edo

gg_mun <- 
  ggplot(lsex_mun, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la seguridad sexual de las mujeres muns de interés" ) +
  xlab("Tasa p/c cien mil habs del municipio")

gg_mun


gg_mun_comp <- 
  ggplot(lsex_mun, aes(x = t_crec_anual, y = mun, size = t_crec_anual)) +
  geom_point(aes( col = t_crec_anual), alpha = 0.5) +
  
  geom_label(aes(label = t_crec_anual),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("green","red")) +
  ggtitle("Crecimiento de delitos que atentan conta la seguridad sexual de las mujeres muns de interés" ) +
  xlab("Tasa de crecimiento con respecto al año previo")

gg_mun_comp




##"La familia"                        "Libertad personal"   
fam <- incidencia_ac1 %>% 
  filter(bien_jur_afectado == "La familia" )

# graf edo

gg_edo <- 
  ggplot(fam, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la familia en Edomex" ) +
  xlab("Tasa p/c cien mil habs del municipio")

gg_edo



####### filtro por muns de interés

fam_mun <- fam %>% 
  filter(mun == "La Paz" |mun == "Chimalhuacán" |mun == "Nezahualcóyotl" |mun == "Nezahualcóyotl"  |mun == "Valle de Chalco Solidaridad" |mun == "Ixtapaluca" 
         |mun == "Chicoloapan" |mun == "Texcoco" |mun == "Ecatepec de Morelos" )

# graf edo

gg_mun <- 
  ggplot(fam_mun, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la familia muns de interés" ) +
  xlab("Tasa p/c cien mil habs del municipio")

gg_mun

# graf edo

gg_mun_comp <- 
  ggplot(fam_mun, aes(x = t_crec_anual, y = mun, size = t_crec_anual)) +
  geom_point(aes( col = t_crec_anual), alpha = 0.5) +
  
  geom_label(aes(label = t_crec_anual),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("green","red")) +
  ggtitle("Crecimiento de delitos que atentan conta la familia muns de interés" ) +
  xlab("Tasa de crecimiento con respecto al año previo")

gg_mun_comp


##"   "Libertad personal"   
lper <- incidencia_ac1 %>% 
  filter(bien_jur_afectado == "Libertad personal" )


####### filtro por muns de interés

lper_mun <- lper %>% 
  filter(mun == "La Paz" |mun == "Chimalhuacán" |mun == "Nezahualcóyotl" |mun == "Nezahualcóyotl"  |mun == "Valle de Chalco Solidaridad" |mun == "Ixtapaluca" 
         |mun == "Chicoloapan" |mun == "Texcoco" |mun == "Ecatepec de Morelos" )

# graf edo

gg_mun <- 
  ggplot(lper_mun, aes(x = a2_t_pobm, y = mun, size = a2_t_pobm)) +
  geom_point(aes( col = a2_t_pobm), alpha = 0.5) +
  
  geom_label(aes(label = a2_t_pobm),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~delito_ab) +
  scale_size(range = c(5, 10))+ 
  scale_colour_gradientn(colours=c("yellow","red")) +
  ggtitle("Delitos que atentan conta la libertad personal muns de interés" ) +
  xlab("Tasa p/c cien mil habs del municipio")

gg_mun