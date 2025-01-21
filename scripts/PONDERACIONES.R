
rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, treemapify, ggplot2)

#### Se fijan parámetros del área de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

#######################################################################################################
### -------------------------------------------------------- 2. ponderaciones ------------------------------

ponderaciones_2024 <- read_excel("~/Documents/aritzy/Electoral/prioridades_2025/ponderaciones_2025.xlsx")

names(ponderaciones_2024)
ponderaciones_2024 <- ponderaciones_2024 %>% 
  pivot_longer(-c(ESTADO, cve_ent, modelo, eleccion))

names(ponderaciones_2024)[6] <- "ponderacion"

ponderaciones_2024$ponderacion[is.na(ponderaciones_2024$ponderacion)] <- 0

ponderaciones_2024 <- ponderaciones_2024 %>% 
  filter(modelo == "alc")

ponderaciones_2024 <-  as.data.frame(ponderaciones_2024)
#DURANGO
#

ponderaciones_durango <-  ponderaciones_2024 %>% 
  filter(ESTADO == "Durango")


prueba <- ponderaciones_durango %>% 
  select(eleccion, ponderacion) 


prueba <- aggregate(prueba$ponderacion, by=list(prueba$eleccion), FUN=sum)

names(prueba)[1] <- "Eleccion"
names(prueba)[2] <- "Porcentaje"


# graf
# 
graph_dur <-  ggplot(prueba, aes(area = Porcentaje, fill = Eleccion, label = paste(Eleccion, Porcentaje, sep = "\n"))) +
geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Reds")

graph_dur


# graf
# 
# 
# 
prueba_an <- ponderaciones_durango %>% 
  select(name, ponderacion) 

prueba_an <- aggregate(prueba_an$ponderacion, by=list(prueba_an$name), FUN=sum)

names(prueba_an)[1] <- "Eleccion"
names(prueba_an)[2] <- "Porcentaje"

graph_dur_an <-  ggplot(prueba_an, aes(area = Porcentaje, fill = Eleccion, label = paste(Eleccion, Porcentaje, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Reds")

graph_dur_an



#VERACRUZ
#

ponderaciones_vera <-  ponderaciones_2024 %>% 
  filter(cve_ent == 30)


ponderaciones_vera <- ponderaciones_vera %>% 
  select(eleccion, ponderacion) 


ponderaciones_vera <- aggregate(ponderaciones_vera$ponderacion, by=list(ponderaciones_vera$eleccion), FUN=sum)

names(ponderaciones_vera)[1] <- "Eleccion"
names(ponderaciones_vera)[2] <- "Porcentaje"


# graf
# 
graph_2 <-  ggplot(ponderaciones_vera, aes(area = Porcentaje, fill = Eleccion, label = paste(Eleccion, Porcentaje, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Reds")

graph_2


