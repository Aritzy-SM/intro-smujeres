rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven)

#### Se fijan parÃÂ¡metros del ÃÂ¡rea de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

setwd("/Users/patatas/Documents/aritzy/mujeres/incidencia delictiva/")


#### procesar incidencia delictiva a nivel estatal ####

incidencia  <- read_excel("datos/Estatal-Delitos-2015-2024_nov2024.xlsx")


victimas <- read_xlsx("/Users/patatas/Documents/aritzy/mujeres/incidencia delictiva/datos/Estatal-V¡ctimas-2015-2024_nov2024.xlsx")


incidencia_objetivo <-  as_tibble(incidencia)

names(incidencia_objetivo)
# seleccionar delitos de interes
#### plataforma riesgos###
# seleccionar delitos de interes
subtipos <- c("Homicidio doloso", 
              "Feminicidio",
              "Lesiones dolosas",
              "Aborto",
              "Otros delitos que atentan contra la vida y la integridad corporal" ,
              "Secuestro",
              "Tráfico de menores",                                                           
              "Rapto" ,
              "Otros delitos que atentan contra la libertad personal" ,
              "Abuso sexual" ,
              "Acoso sexual" ,
              "Hostigamiento sexual" ,
              "Violación simple" ,
              "Violación equiparada" ,
              "Incesto",
              "Otros delitos que atentan contra la libertad y la seguridad sexual" ,
              
              "Robo a casa habitación", 
              "Robo de vehículo automotor",
              "Robo a transportista", 
              "Robo a transeúnte en espacio abierto al público", 
              "Robo a transeúnte en vía pública", 
              "Robo a negocio", 
              "Robo en transporte individual", 
              "Robo en transporte público colectivo",
              "Robo en transporte público individual", 
              
              "Extorsión",
              
              "Violencia familiar", 
              "Violencia de género en todas sus modalidades distinta a la violencia familiar", 
              "Incumplimiento de obligaciones de asistencia familiar" ,
              
              "Narcomenudeo", 
              "Amenazas"
)
select_delitos_todos <- incidencia_objetivo %>% 
  filter(`Subtipo de delito` %in% subtipos)

# simplificar mÃ¡s de una modalidad 
robo_v_cv <- c("Robo de coche de 4 ruedas Con violencia",
               "Robo de embarcaciones pequeñas y grandes Con violencia",
               "Robo de motocicleta Con violencia")
robo_v_sv <- c("Robo de coche de 4 ruedas Sin violencia",
               "Robo de embarcaciones pequeñas y grandes Sin violencia",
               "Robo de motocicleta Sin violencia")

delitos <- select_delitos_todos %>% 
  mutate(delito=ifelse(`Subtipo de delito`=="Robo de vehículo automotor" & Modalidad %in% robo_v_cv, "Robo de vehículo con violencia",
                       ifelse(`Subtipo de delito`=="Robo de vehículo automotor" & Modalidad %in% robo_v_sv, "Robo de vehículo sin violencia",
                              ifelse(`Subtipo de delito`=="Robo a transportista" & Modalidad=="Con violencia", "Robo a transportista con violencia",
                                     ifelse(`Subtipo de delito`=="Robo a transportista" & Modalidad=="Sin violencia", "Robo a transportista sin violencia", `Subtipo de delito`)))))

delitos_todos <- as.data.frame(unique(delitos$delito)) 

delitos <- delitos %>% 
  mutate(delito_ab = ifelse(delito=="Homicidio doloso", "homicidio_dol",
                            ifelse(delito=="Lesiones dolosas", "lesiones_dol",
                                   ifelse(delito=="Feminicidio", "feminicidio",
                                          ifelse(delito=="Secuestro", "secuestro",
                                                 ifelse(delito=="Robo de vehículo con violencia", "robo_veh_cv",
                                                        ifelse(delito=="Robo de vehículo sin violencia", "robo_veh_sv",
                                                               ifelse(delito=="Robo a transportista con violencia", "robo_transportista_cv",
                                                                      ifelse(delito=="Robo a transportista sin violencia", "robo_transportista_sv",
                                                                             ifelse(delito=="Robo a transeúnte en vía pública", "robo_transeunte_viap",
                                                                                    ifelse(delito=="Robo a transeúnte en espacio abierto al público", "robo_transeunte_ep",
                                                                                           ifelse(delito=="Robo en transporte público individual", "robo_transppub_ind",
                                                                                                  ifelse(delito=="Robo en transporte público colectivo", "robo_transppub_col",
                                                                                                         ifelse(delito=="Robo en transporte individual", "robo_transp_ind",
                                                                                                                ifelse(delito=="Robo a negocio", "robo_negocio",
                                                                                                                       ifelse(delito=="Robo de ganado", "robo_ganado",
                                                                                                                              ifelse(delito=="Extorsión", "extorsion",
                                                                                                                                     ifelse(delito=="Violencia familiar", "viol_familiar",
                                                                                                                                            ifelse(delito=="Robo de maquinaria", "robo_maquinaria",       
                                                                                                                                                   ifelse(delito=="Violencia de género en todas sus modalidades distinta a la violencia familiar", "viol_genero",
                                                                                                                                                          ifelse(delito=="Robo a institución bancaria", "robo_banco",
                                                                                                                                                                 ifelse(delito == "Robo a casa habitación", "robo_casa",
                                                                                                                                                                        ifelse(delito == "Daño a la propiedad", "d_propiedad", "narcomenudeo")))))))))))))))))))))))


# sumar por delito estatal
names(delitos)
meses <- colnames(delitos)[8:19]
delitos_indicadores <- delitos %>% 
  group_by(Año, Clave_Ent, Entidad, `Bien jurídico afectado`, 
           `Tipo de delito`, delito, delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)


# sumar por delito nacional

delitos_indicadores_nac <- delitos %>% 
  group_by(Año, delito_ab) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

###################################### ANALISIS POR DELITO ############################
#### filtrar por delito de interes
violencia_fam_edo <-  delitos_indicadores %>% 
  filter(delito_ab=="viol_familiar") 


violencia_fam_nac <-  delitos_indicadores_nac %>% 
  filter(delito_ab=="viol_familiar") 

names(violencia_fam_nac)
violencia_fam_nac <- violencia_fam_nac %>%
  select(1,3:14)


vfc_time <-violencia_fam_nac  %>%
  pivot_longer(!Año, names_to = "mes", values_to = "incidencia_mes")



vfc_time$day <- 1

vfc_time <- vfc_time %>% 
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

vfc_time <-  vfc_time %>%  
  mutate(datetime = make_datetime(Año, mes, day)) %>% 
  arrange(datetime)

names(vfc_time)
vfc_time <- vfc_time %>% 
  select(Año, datetime, incidencia_mes)

write.csv(vfc_time, file="violencia_fam_nact.csv", fileEncoding ="latin1")


### grafica interactiva
p <- dygraph(vfc_time)


#### ggplot
#### 
vfc_time$datetime <-  as.Date(vfc_time$datetime)
pgg <- ggplot(vfc_time, aes(x=datetime, y=incidencia_mes)) +
  geom_line() + 
  geom_area(fill = "gray", alpha = 0.9) +
  xlab("")+
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year", 
               date_minor_breaks = "1 month") 

pgg <- ggplot(vfc_time, aes(x=datetime, y=incidencia_mes, label = incidencia_mes)) +
  geom_line(colour='brown4', size =.8)+
  geom_point(colour='brown4', size =.4)+
  xlab("")+
  scale_x_date(date_labels = "%b", 
               limits = c(as.Date("2015-01-01"), as.Date("2024-11-01")), 
               breaks = seq(as.Date("2015-01-01"), as.Date("2024-11-01"), 
                            by = "1 months"))+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(check_overlap = TRUE, vjust = 0, nudge_x = 0.05, size=2)


pgg


#####  graph como timeseries 

vfc_time$datetime <-  as.Date(vfc_time$datetime)

pgg2 <- ggplot(vfc_time, aes(x=datetime, y=incidencia_mes, label = incidencia_mes, colour = factor(Año))) +
  geom_line() + 
  geom_point()+
  xlab("")+
  scale_x_date(date_labels = "%b", 
               limits = c(as.Date("2015-01-01"), as.Date("2024-11-01")), 
               breaks = seq(as.Date("2015-01-01"), as.Date("2024-11-01"), 
                            by = "1 months"))+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(check_overlap = TRUE, vjust = 0, nudge_x = 0.05, size=2)


pgg2

#### ---------- victimas y otros --------------- ###

