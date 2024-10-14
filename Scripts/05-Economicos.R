
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-05----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------INDICADORES ECONÓMICOS
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
library(forcats)
library(REAT)
library(ineq )

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
#/////////////////////////////EMPLEO Y OCUPACIÓN         /////////////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================


#--Personas de 16 años y más con condicion de trabajo
#---dplyr

#---Población en edad de trabajar PET
DF %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16)

frq(DF$actpr2012,weights = fac00)

#---Población en Economicamente activa PEA
DF %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30)


#--Personas de 16 años y más con condicion de trabajo


#---datatable

PET<-DT[r106>=16,.(r106,actpr,actpr2012,actse,fac00)]
PET

dplyr::glimpse(PET)


PEA<-DT[r106>=16 & actpr2012<30,.(r106,actpr,actpr2012,actse,fac00)]

dplyr::glimpse(PEA)


DT[r106>=16 & actpr2012<30,.(Poblacion=round(sum(fac00),digits = 0) ),
   by=actpr2012]

#=====================================================================
#----------------------------------PET y PEA
#=================================================================

#--Explorando las variables

str(EHPM$r106)
str(EHPM$actpr)
str(EHPM$actpr2012)
str(EHPM$actse)

frq(EHPM$actpr,weights= EHPM$fac00 )
frq(EHPM$actpr2012,weights =EHPM$fac00)
frq(EHPM$actse,weights =EHPM$fac00 )

#--Personas de 16 años y más con condicion de trabajo
#---dplyr

#---Población en edad de trabajar PET

EHPM %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16)

EHPM %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16) %>% 
  dplyr::summarise(PET=sum(fac00)) 




#---Población en Economicamente activa PEA

EHPM %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30)

EHPM %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30)%>% 
  dplyr::summarise(PEA=sum(fac00)) 


## Tasa bruta y neta de participación



#=====================================================================
#----------------------------------Tasa Neta y Bruta de Participación
#=================================================================

frq(EHPM$actpr,weights= EHPM$fac00 )
frq(EHPM$actpr2012,weights =EHPM$fac00)
frq(EHPM$actse,weights =EHPM$fac00 )



#---Población
Poblacion<-EHPM %>% summarise(Poblacion=sum(fac00))
Poblacion

#----PET
PET<-EHPM %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16) %>% 
  dplyr::summarise(PET=sum(fac00)) 
PET

#----PEA
PEA<-EHPM %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30)%>% 
  dplyr::summarise(PEA=sum(fac00)) 
PEA

#----Tasa Neta de Participacion PEA/PET


TNP<-(PEA/PET*100)
TNP


#----Tasa Bruta de Participacion PEA/Poblacion

TBP<-(PEA/Poblacion*100)
TBP


## Cóndición de ocupación


#--------Condición de ocupación y tasa de desocupación

EHPM%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30) %>% 
  dplyr::group_by(actpr2012)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(Pob_16=sum(Poblacion))%>%
  dplyr::mutate(CondicionOcupado=scales::percent(Poblacion/Pob_16,accuracy =0.1))



## Tasa de desocupación abierta

#--------Tasa de desocupación abierta

EHPM%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30) %>% 
  dplyr::group_by(actpr)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(Pob_16=sum(Poblacion))%>%
  dplyr::mutate(CondicionDesocupado=scales::percent(Poblacion/Pob_16,accuracy =0.1))



#==================================================================================================================
#/////////////////////////////                            ////////////////////////////////////////////////////////
#/////////////////////////////INGRESO Y POBREZA          /////////////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================


#====================================================================================================
#---------------------------------------------------------------------POBLACION INGRESO FAMILIAR
#====================================================================================================


dplyr::glimpse(DF$ingfa) #-------ingreso familiar
summary(DF$ingfa)

dplyr::glimpse(DF$ingpe)#----ingreso percapita
summary(DF$ingpe)

#-----------Ingreso familiar
ingresoF<-DF %>%
  dplyr::select(area,ingfa,fac00)%>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingfa))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
ingresoF$area<- as.factor(ingresoF$area)
levels(ingresoF$area )<- c("Rural","Urbano")

ingresoF

#-----graficando
p1 <- ggplot(ingresoF, aes(x=' ',IngresoMedio, fill=area), group=area) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso Medio Familiar",
       subtitle ="Por Área Geográfica (En US$)",
       x = "Área Geográfica", y = "US$",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+ 
  geom_text(data = ingresoF, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
p1

#-----------Ingreso percapita
ingresoP<-DF %>%
  dplyr::select(r104,ingpe,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingpe))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
ingresoP$r104<- as.factor(ingresoP$r104)
levels(ingresoP$r104 )<- c("Hombre","Mujer")

ingresoP

p2 <- ggplot(ingresoP, aes(x=' ',IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso Medio Percapita",
       subtitle ="Por Sexo (En US$)",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+ 
  geom_text(data = ingresoP, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
p2

#---imeds ingreso por trabajo dependiente

imeds<-DF %>%
  dplyr::select(r104,imeds,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*imeds))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
imeds$r104<- as.factor(imeds$r104)
levels(imeds$r104 )<- c("Hombre","Mujer")

imeds

#-----graficando
p3 <- ggplot(imeds, aes(x=' ',IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso Medio Dependiente",
       subtitle ="Por Sexo (En US$)",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+ 
  geom_text(data = imeds, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
p3

#---imeds ingreso por trabajo independiente

imei<-DF %>%
  dplyr::select(r104,imei,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*imei))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
imei$r104<- as.factor(imei$r104)
levels(imei$r104 )<- c("Hombre","Mujer")

imei

#-----graficando
p4 <- ggplot(imei, aes(x=' ',IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso Medio Independiente",
       subtitle ="Por Sexo (En US$)",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+ 
  geom_text(data = imei, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
p4


#---imeds ingreso por trabajo dependiente

GiniInfa<-ineq(DF$ingfa,type = "Gini")
GiniInfa
GiniInpe<-ineq(DF$ingpe,type = "Gini")
GiniInpe
GiniImed<-ineq(DF$imeds,type = "Gini")
GiniImed
GiniIneto<-ineq(DF$ingneto,type = "Gini")
GiniIneto

plot(Lc(DF$ingfa))

plot(Lc(DF$ingpe))

plot(Lc(DF$imeds))

plot(Lc(DF$ingneto))

var1<-var.coeff(DF$ingfa*DF$fac00)
var1
var2<-var.coeff(DF$ingpe*DF$fac00)
var2

#========================================================================
#-----pobreza


#---Condicion de pobreza en personas
DF %>%
  dplyr::select(r104,pobreza,fac00)%>% 
  dplyr::group_by(pobreza)%>% 
  dplyr::summarise(Poblacion=sum(fac00))

DF %>%
  dplyr::select(r104,pobreza,fac00)%>% 
  dplyr::group_by(pobreza,r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00))


#p <- ggplot(DF, aes(pobreza, fill = r104)) 

#p

DF$pobreza<- as.factor(DF$pobreza)
levels(DF$pobreza)<- c("Pobreza Extrema","Pobreza Relativa","No Pobre")


DF$r104<- as.factor(DF$r104)
levels(DF$r104)<- c("Hombre","Mujer")


p + geom_bar(position = "dodge",aes(weight = fac00/1000000))+
  labs(title ="Población según condición de pobreza por género",
       subtitle ="(En millones de personas)",
       x = "Condición de Pobreza", y = "Millones personas",
       caption = "Elaboración propia con información de la Base EHPM 2021 DIGESTYC")+ 
  scale_fill_brewer(palette = "YlOrRd")

p + geom_bar(position = "dodge",aes(weight = fac00/1000000))+
  labs(title ="Población según condición de pobreza por género",
       subtitle ="(En millones de personas)",
       x = "Condición de Pobreza", y = "Millones personas",
       caption = "Elaboración propia con información de la Base EHPM 2021 DIGESTYC")+ 
  scale_fill_brewer(palette = "YlOrRd")+
  facet_wrap(~pobreza, ncol = 1,strip.position = "left")

p + geom_bar(position = "dodge",aes(weight = fac00/1000000))+
  labs(title ="Población según condición de pobreza por género",
       subtitle ="(En millones de personas)",
       x = "Condición de Pobreza", y = "Millones personas",
       caption = "Elaboración propia con información de la Base EHPM 2021 DIGESTYC")+ 
  scale_fill_brewer(palette = "YlOrRd")+
  facet_wrap(~pobreza, nrow = 1,strip.position = "top")

