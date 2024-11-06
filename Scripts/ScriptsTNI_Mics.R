
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-01----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------Tasa de natalidad
#------------------------------Por Elias Preza
#/////////////////////////////////////////////////////////////////////////////////////////////////


#=======================================================================LIBRERIAS

library(tidyverse)#---Wrangling data
library(sjmisc)#---Frecuencias
library(haven)#---Abrir base en formatos: SPSS,STATA y SAS
library(codebook)#--Uso de frecuencias
library(githubinstall) #-- uso Git
library(scales)     #--Porcentajes
library(data.table) #--Data table
library(readxl)     #--Para importar datos
library(pyramid)    #--Para graficar 
library(plotrix)    #--Para graficar
library(GGally)     #--Para graficar
library(plotly)     #--Para graficar
library(treemapify) #--Para graficar
library(labelled)
library(lubridate)


# Cargar el archivo
data_bh <- read_sav("~/Onedrive/Jarecdata/Temas de Investigacion/Demografia_R/Bases/bh.sav")
data_bh <- read_sav("C:/Users/RURAL01/OneDrive/DGDR/Desarrollo_R/Demografia_R/Bases/bh.sav")
data_wm <- read_sav("~/Onedrive/Jarecdata/Temas de Investigacion/Demografia_R/Bases/wm.sav")
data_wm <- read_sav("C:/Users/RURAL01/OneDrive/DGDR/Desarrollo_R/Demografia_R/Bases/wm.sav")
data_hl <- read_sav("~/Onedrive/Jarecdata/Temas de Investigacion/Demografia_R/Bases/hl.sav")
data_hl <- read_sav("C:/Users/RURAL01/OneDrive/DGDR/Desarrollo_R/Demografia_R/Bases/hl.sav")

dplyr::glimpse(data_bh)
dplyr::glimpse(data_wm)
dplyr::glimpse(data_hl)

Dic_00<-codebook_table(data_bh)#---Construyendo el diccionario
Dic_01<-codebook_table(data_wm)
#Dic_02<-codebook_table(data_hl)

#=================================================================================



data_bh <- data_bh %>%
  mutate(
    fecha_nacimiento = as.Date("1900-01-01") + months(WDOB),
    edadM = 2014 - as.numeric(format(fecha_nacimiento, "%Y")),
    edadMM = (2014 - BH4Y) * 12 # Cálculo de meses entre 2011 y 2014
  )



data_bh <- data_bh %>%
  mutate(
    edadMC = case_when(
      edadM >= 15 & edadM <= 19 ~ 1,
      edadM >= 20 & edadM <= 24 ~ 2,
      edadM >= 25 & edadM <= 29 ~ 3,
      edadM >= 30 & edadM <= 34 ~ 4,
      edadM >= 35 & edadM <= 39 ~ 5,
      edadM >= 40 & edadM <= 44 ~ 6,
      edadM >= 45 & edadM <= 49 ~ 7,
      TRUE ~ NA_real_  # Para edades fuera del rango especificado
    )
  )


# Filtrar nacimientos en los últimos 3 años eliminando la probabilidad de mortalidad
data_bh_filtered <- data_bh %>%
  filter(BH5 == 1,edadMM<=36)  # Considerando BH4Y como año de nacimiento y filtro para tres años anteriores

# # Filtrar nacimientos en los últimos 3 años eliminando la probabilidad de mortalidad
# data_bh_filteredHijas <- data_bh %>%
#   filter(BH3==2, BH5 == 1,edadMM<=36)  # Considerando BH4Y como año de nacimiento y filtro para tres años anteriores
# 


# trabajando con las edades de las mujeres
data_wm <- data_wm %>%
  mutate(
    edadMA = 2014 - WB1Y  # Cálculo de la edad en años en 2014
  )

data_wm <- data_wm %>%
  mutate(
    edadMAC = case_when(
      edadMA >= 15 & edadMA <= 19 ~ 1,
      edadMA >= 20 & edadMA <= 24 ~ 2,
      edadMA >= 25 & edadMA <= 29 ~ 3,
      edadMA >= 30 & edadMA <= 34 ~ 4,
      edadMA >= 35 & edadMA <= 39 ~ 5,
      edadMA >= 40 & edadMA <= 44 ~ 6,
      edadMA >= 45 & edadMA <= 49 ~ 7,
      TRUE ~ NA_real_  # Para edades fuera del rango especificado
    )
  )


# Filtrando los NA de la base de mujeres
data_wmF <- data_wm %>%
  filter(WM7==1,!is.na(edadMAC))

# Calcular TGF para cada grupo de edad

naci <- data_bh_filtered %>%
  group_by(edadMC) %>%
  summarise(live_births = n())

# naci2 <- data_bh %>% filter(data_bh$edadMM<=12) %>% 
#   #group_by(edadMC) %>%
#   summarise(live_births = n())


#1. 15-19, 2. 20-24, 3. 25-29, 4. 30-34, 5. 35-39, 6. 40-44, 7. 45-49
cmuj<-data_wmF %>%
  filter(edadMAC >= 1 | edadMAC <= 7 ) %>%
  group_by(edadMAC) %>%
  summarise(totalmuj = n())

# cmuj2<-data_wmF %>%
#   filter(edadMAC >= 1 | edadMAC <= 7 ) %>%
#   #group_by(edadMAC) %>%
#   summarise(totalmuj = n())


tasas <-left_join(naci,cmuj,by = join_by(edadMC==edadMAC)) %>% 
  dplyr::mutate(PHNV = (live_births / totalmuj)) %>% # Promedio de Hijos Nacidos Vivos
  dplyr::mutate(TFE = (live_births / totalmuj)*1000 ) %>% #Tasa Fecundidad Edad
  dplyr::mutate(TGF=sum(TFE/1000)) #Tasa Global de Fecundidad

tasas$edadMC <- as.factor(tasas$edadMC)
levels(tasas$edadMC) <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")

#Tasa Global de Fecundidad para mujeres de 15 a 49 años
TGF<-tasas$TGF
TGF

# 
# #Tasa Fecundidad General para mujeres de 15 a 49 años
# TFG<-naci2/cmuj2*1000
# TFG
# 
# 
# #////////////////////////////////////////////
# # Calcular TFE Tasa de fecundidad específica por edad para mujeres de 15 a 19 años
# 
# # Filtrar nacimientos en los últimos 3 años eliminando la probabilidad de mortalidad
# 
# #BH5 == 1
# data_bh_filtered2 <- data_bh %>%
#   filter(BH5 == 1,edadMM<=12)  # Considerando BH4Y como año de nacimiento y filtro para tres años anteriores
# 
# 
# 
# nacitfe <- data_bh_filtered2 %>%
#   filter(edadM>=15,edadM<=19 ) %>%
#   group_by(edadM) %>%
#   summarise(live_births = n()) %>% 
#   filter(!is.na(edadM))
# 
# cmujtfe<-data_wmF %>%
#   filter(edadMA >= 15 & edadMA<= 19) %>%
#   group_by(edadMA) %>%
#   summarise(totalmuj = n())
# 
# 
# asfr_dataTFE <-left_join(nacitfe,cmujtfe,by = join_by(edadM==edadMA)) %>% 
#   dplyr::mutate(asfrtfe= (live_births / totalmuj)) %>% 
#   dplyr::mutate(asfrtfeS=sum(asfrtfe))
# 
# n<-sum(asfr_dataTFE$live_births)
# d<-sum(asfr_dataTFE$totalmuj)
# (n/d*1000)
# 
# # Mostrar resultados
# cat("Tasa Global de Fecundidad (TFR) para mujeres de 15 a 49 años:", TFR, "\n")
# cat("Tasa de Fecundidad Específica por Edad (ASFR) para mujeres de 15 a 19 años:", ASFR_15_19/1000, "\n")
# cat("Porcentaje de Mujeres de 20 a 24 años que tuvo al menos un nacido vivo antes de los 18 años:", porcentaje_mujeres_20_24_con_hijos_antes_18, "%", "\n")
# 
# 

#//////////////////////////////////////////////////////////////////

# 
# 
# #/////////////////////////////////////////////////////////////////////////////////////////////////
# #------------------------------Script-01----------------------------------------------------------
# #------------------------------Análisis Demográfico-----------------------------------------------
# #------------------------------Tasa de natalidad
# #------------------------------Por Elias Preza
# #/////////////////////////////////////////////////////////////////////////////////////////////////
# 
# 
# #=======================================================================LIBRERIAS
# 
# library(tidyverse)#---Wrangling data
# library(sjmisc)#---Frecuencias
# library(haven)#---Abrir base en formatos: SPSS,STATA y SAS
# library(codebook)#--Uso de frecuencias
# library(githubinstall) #-- uso Git
# library(scales)     #--Porcentajes
# library(data.table) #--Data table
# library(readxl)     #--Para importar datos
# library(pyramid)    #--Para graficar 
# library(plotrix)    #--Para graficar
# library(GGally)     #--Para graficar
# library(plotly)     #--Para graficar
# library(treemapify) #--Para graficar
# library(labelled)
# library(lubridate)
# 
# 
# # Cargar el archivo
# data_bh <- read_sav("~/Onedrive/Jarecdata/Temas de Investigacion/Demografia_R/Bases/bh.sav")
# data_wm <- read_sav("~/Onedrive/Jarecdata/Temas de Investigacion/Demografia_R/Bases/wm.sav")
# #data_hl<- read_sav("~/Onedrive/Jarecdata/Temas de Investigacion/Demografia_R/Bases/hl.sav")
# 
# 
# dplyr::glimpse(data_bh)
# dplyr::glimpse(data_wm)
# #dplyr::glimpse(data_hl)
# 
# Dic_00<-codebook_table(data_bh)#---Construyendo el diccionario
# Dic_01<-codebook_table(data_wm)
# #Dic_02<-codebook_table(data_hl)
# 
# 
# data_bh <- data_bh %>%
#   mutate(fecha_nacimiento = as.Date("1900-01-01") + months(WDOB),
#          edadM = 2014 - as.numeric(format(fecha_nacimiento, "%Y")))
# 
# data_bh <- data_bh %>%
#   mutate(
#     edadMC = case_when(
#       edadM >= 15 & edadM <= 19 ~ 1,
#       edadM >= 20 & edadM <= 24 ~ 2,
#       edadM >= 25 & edadM <= 29 ~ 3,
#       edadM >= 30 & edadM <= 34 ~ 4,
#       edadM >= 35 & edadM <= 39 ~ 5,
#       edadM >= 40 & edadM <= 44 ~ 6,
#       edadM >= 45 & edadM <= 49 ~ 7,
#       TRUE ~ NA_real_  # Para edades fuera del rango especificado
#     )
#   )
# 
# # Filtrar nacimientos en los últimos 3 años
# data_bh_filtered <- data_bh %>%
#   filter(BH5 == 1, BH4Y >= 2011)  # Considerando BH4Y como año de nacimiento y filtro para tres años anteriores
# 
# # Calcular ASFR para cada grupo de edad
# 
# 
# naci <- data_bh_filtered %>%
#   group_by(edadMC) %>%
#   summarise(live_births = n())
# 
# cmuj<-data_wm %>%
#   filter(WAGE >= 1 | WAGE <= 7 ) %>%
#   group_by(WAGE) %>%
#   summarise(totalmuj = n())
# 
# asfr_data <-left_join(naci,cmuj,by = join_by(edadMC==WAGE)) %>% 
#   mutate(asfr = (live_births / totalmuj) * 1000)
# 
# 
# # Cargar librerías necesarias
# library(dplyr)
# 
# # Calcular Tasa Global de Fecundidad (TFR)
# TFR <- sum(asfr_data$asfr, na.rm = TRUE) * 5
# 
# # Calcular Tasa de Fecundidad Específica por Edad (ASFR) para mujeres de 15 a 19 años
# ASFR_15_19 <- asfr_data %>%
#   filter(edadMC == 1) %>%
#   pull(asfr)
# 
# # Calcular Porcentaje de Mujeres de 20 a 24 años que tuvo al menos un nacido vivo antes de los 18 años
# # Filtrar nacimientos y mujeres en rango de edad específico
# mujeres_20_24_con_hijos_antes_18 <- data_bh %>%
#   filter(edadMC == 2, edadM < 18, BH5 == 1) %>%
#   group_by(LN) %>% # LN como identificador único de mujeres
#   summarise(hijos_antes_18 = n()) %>%
#   filter(hijos_antes_18 >= 1) %>% # Al menos un hijo antes de los 18
#   nrow()
# 
# 
# total_mujeres_20_24 <- data_wm %>%
#   filter(WAGE == 2) %>%
#   summarise(total = n()) %>%
#   pull(total)
# 
# porcentaje_mujeres_20_24_con_hijos_antes_18 <- (mujeres_20_24_con_hijos_antes_18 / total_mujeres_20_24) * 100
# 
# # Mostrar resultados
# cat("Tasa Global de Fecundidad (TFR) para mujeres de 15 a 49 años:", TFR/1000, "\n")
# cat("Tasa de Fecundidad Específica por Edad (ASFR) para mujeres de 15 a 19 años:", ASFR_15_19/1000, "\n")
# cat("Porcentaje de Mujeres de 20 a 24 años que tuvo al menos un nacido vivo antes de los 18 años:", porcentaje_mujeres_20_24_con_hijos_antes_18, "%", "\n")
# 
# #===========================================================
# 
# 
# 
