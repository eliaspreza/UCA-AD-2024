
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-01----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------INTRODUCCIÓN A LA EHPM
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

#--------------------------Ayudas Librerias
?tidyverse
?sjmisc
?haven
?codebook

#=======================================================================APERTURA BASES


EHPM<-read_sav("Bases/EHPM_2023.sav")#---Se lee EHPM de datos
#Dic_00<-codebook_table(EHPM)#---Construyendo el diccionario
#write_excel_csv(Dic_00,"Bases/Diccionario.csv")#---Se guarda diccionario
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
#View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos

head(EHPM,15)#---Viusliza los primeros 15 registros

#------------------------Explorando el contenido de la EHPM

dplyr::glimpse(EHPM) #---Estructura del dataframe
head(EHPM,5) #-----Visualización
tail(EHPM,20) #-----Visualización
nrow(EHPM) #---número de lineas o casos
ncol(EHPM) #---número de columnas o variables
dim(EHPM) #---Dimension de la EHPM: lineas - columnas
length(EHPM) #---número de columnas o variables
colnames(EHPM) #----Nombre de las columnas
names(EHPM) #----Nombre de las columnas
str(EHPM) #--Estructura del dataframe

#------------------------Explorando variables

str(EHPM$region) #--Estructura de la variable
dplyr::glimpse(EHPM$region) #--Estructura de la variable

#==================================================================================================================
#/////////////////////////////                            ////////////////////////////////////////////////////////
#/////////////////////////////FILTROS CONSULTAS           ////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================

#=======================================================================FILTROS CONSULTAS

EHPM_hogares<-EHPM %>%
              dplyr::filter(r103==1)

sum(EHPM_hogares$fac00)#--Cantidad de Hogares
sum(EHPM$fac00)#--Cantidad de personas

#---Creando la seccion 1

EHPM_S01<-EHPM %>% 
  dplyr::select(idboleta,r004,r101,r103,r103otr,r104,r105m,r105a,r106,r107,r108a,r108b,fac00) %>% 
  dplyr::mutate(id2=c(1:nrow(EHPM)))#---Construyo un correlativo llamado id2

dplyr::glimpse(EHPM_S01)

#----Creando la seccion 1 Forma de un DT
DT<-as.data.table(EHPM) #---se transforma la base a datatable

EHPM_S01_2<-DT[,.(idboleta,r004,r101,r103,r103otr,r104,r105m,r105a,r106,r107,r108a,r108b,fac00)]

EHPM_S01_2<-DT[,.(id3= c(1:nrow(DT)),idboleta,r004,r101,r103,r103otr,r104,r105m,r105a,r106,
                  r107,r108a,r108b,fac00)] #---Construir un id



