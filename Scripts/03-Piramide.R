

#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-03----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------PIRÁMIDE DE LA POBLACIÓN
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
EHPM_hogares<-EHPM %>% dplyr::filter(r103==1)#---Se filtra para hogares
DF<-EHPM #---Se renombra del DF
DT<-as.data.table(EHPM) #---se transforma la base a datatable
#Dic_00<-codebook_table(EHPM)#---Construyendo el diccionario
#write_excel_csv(Dic_00,"Bases/Diccionario.csv")#---Se guarda diccionario
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
#View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos

head(EHPM,15)#---Viusliza los primeros 15 registros

#==================================================================================================================
#/////////////////////////////                            ////////////////////////////////////////////////////////
#/////////////////////////////PIRAMIDE DE POBLACION       ////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================

#---Codificar la variable edad
rangoE <- vector()
rangoE[DF$r106 >= 0 & DF$r106 <= 4] <- 1
rangoE[DF$r106 >= 5 &  DF$r106 <= 9] <- 2
rangoE[DF$r106 >= 10 & DF$r106 <= 14] <- 3
rangoE[DF$r106 >= 15 & DF$r106 <= 19] <- 4
rangoE[DF$r106 >= 20 & DF$r106 <= 24] <- 5
rangoE[DF$r106 >= 25 & DF$r106 <= 29] <- 6
rangoE[DF$r106 >= 30 & DF$r106 <= 34] <- 7
rangoE[DF$r106 >= 35 & DF$r106 <= 39] <- 8
rangoE[DF$r106 >= 40 & DF$r106 <= 44] <- 9
rangoE[DF$r106 >= 45 & DF$r106 <= 49] <- 10
rangoE[DF$r106 >= 50 & DF$r106 <= 54] <- 11
rangoE[DF$r106 >= 55 & DF$r106 <= 59] <- 12
rangoE[DF$r106 >= 60 & DF$r106 <= 64] <- 13
rangoE[DF$r106 >= 65 & DF$r106 <= 69] <- 14
rangoE[DF$r106 >= 70 & DF$r106 <= 74] <- 15
rangoE[DF$r106 >= 75 & DF$r106 <= 79] <- 16
rangoE[DF$r106 >= 80 & DF$r106 <= 84] <- 17
rangoE[DF$r106 >= 85] <- 18
DF$rangoE <- as.factor(rangoE)
levels(DF$rangoE) <- c("De 0 a 4","De 5 a 9","De 10 a 14","De 15 a 19","De 20 a 24","De 25 a 29"," De 30 a 34","De 35 a 39",
                       "De 40 a 44","De 45 a 49", "De 50 a 54", "De 55 a 59", "De 60 a 64","De 65 a 69", "De 70 a 74",
                       "De 75 a 79","De 80 a 84","85 y más")

#--Se construye la variable sexo con factor
DF$sexo<-as.factor(DF$r104)
levels(DF$sexo)<- c("Hombre","Mujer")


#--Se construye la variable cantidad de Hombres
Hombre<-DF%>%
  dplyr::select(sexo,rangoE,fac00) %>% 
  dplyr::filter(sexo=="Hombre") %>% 
  dplyr::group_by(rangoE) %>% 
  dplyr::summarise(Hombres=round(sum(fac00),digits = 0)/1000)

#--Se construye la variable cantidad de Mujeres
Mujer<-DF%>%
  dplyr::select(sexo,rangoE,fac00) %>% 
  dplyr::filter(sexo=="Mujer") %>% 
  dplyr::group_by(rangoE) %>% 
  dplyr::summarise(Mujeres=round(sum(fac00),digits = 0)/1000)

#--Construyendo la pirámide
DataPiramide<-cbind(Hombre,Mujer)

DataPiramide<-DataPiramide[,c(2,4,1)]

#--graf piramide
datos<-data.frame(DataPiramide)
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="",
        main="Población El Salvador, EHPM 2023\n (en miles de personas)",Lcol="cyan",
        Rcol="pink", Cgap=0.2,Cadj=0)

datos<-data.frame(DataPiramide)
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="",
        main="Población El Salvador, EHPM 2023\n (en miles de personas)",Lcol="cyan",
        Rcol="pink", Cgap=0)

#====================================================================================================
#------------------------------------------------------------Creando la pirámide con library(ggplot2)
#====================================================================================================

DPy<-DF %>% 
  dplyr::select(rangoE,sexo,fac00) %>% 
  dplyr::group_by(rangoE,sexo) %>% 
  dplyr::summarise(Pob=round(sum(fac00),digits = 0)/1000) %>%
  dplyr::arrange(sexo)


p <- ggplot(DPy,aes(x=rangoE, fill=sexo,
                    y=ifelse(sexo=='Hombre',-Pob,Pob)))

p+geom_bar(stat = "identity")+
  scale_y_continuous(limits = max(DPy$Pob)*c(-1,1),labels=abs)+
  geom_text(aes(x=rangoE,y=ifelse(sexo=='Hombre',-Pob,Pob),label=abs(round(Pob,digits=2))),size = 3,hjust = "inward")+
  scale_fill_brewer(palette ="Blues")+
  ggtitle("Pirámide Poblacional", subtitle = "El Salvador 2022")+
  labs(y="Población en miles",x="Rango Edad",
       caption = ("Fuente: Base EHPM 2021 ONEC/BCR"))+
  coord_flip()+
  theme_classic()


p+geom_bar(stat = "identity")+
  scale_y_continuous(limits = max(DPy$Pob)*c(-1,1),labels=abs)+
  #geom_text(aes(x=rangoE,y=ifelse(sexo=='Hombre',-Pob,Pob),label=abs(round(Pob,digits=2))),size = 3,hjust = "inward")+
  scale_fill_brewer(palette ="Blues")+
  ggtitle("Pirámide Poblacional", subtitle = "El Salvador 2022")+
  labs(y="Población en miles",x="Rango Edad",
       caption = ("Fuente: Base EHPM 2021 ONEC/BCR"))+
  coord_flip()+
  theme_classic()

