
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-02----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------TAMAÑO Y COMPOSICIÓN DE LA POBLACIÓN
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
#/////////////////////////////TAMAÑO Y COMPOSICIÓN DE LA POBLACIÓN////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================

#====================================================================================================
#---------------------------------------------------------------------POBLACION TOTAL Y HOGARES
#====================================================================================================

sum(EHPM_hogares$fac00)#--Cantidad de Hogares
sum(EHPM$fac00)#--Cantidad de personas

#====================================================================================================
#---------------------------------------------------------------------POBLACION SEGÚN SEXO
#====================================================================================================


Sexo<-DF%>%
  dplyr::select(r104,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(poblacion=sum(fac00))%>%
  dplyr::mutate(total=sum(DF$fac00)) %>% 
  dplyr::mutate(porcentaje=(poblacion/total*100))
Sexo


#--GRAFICA

Sexo$r104<-as.factor(Sexo$r104) #---Transformo a factor
levels(Sexo$r104)<-c("Hombre","Mujer") #--se asigna los niveles

ggplot(Sexo,aes(x="",y=porcentaje, fill=r104))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("steelblue","tan2"))+
  theme_void()+
  labs(title ="El Salvador: Población según género 2023",
       subtitle ="(En porcentajes)",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR")


#---Forma de dona

ggplot(Sexo,aes(x=2,y=porcentaje, fill=r104))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("steelblue","tan2"))+
  theme_void()+
  labs(title ="El Salvador: Población según género 2023",
       subtitle ="(En porcentajes)",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+
  xlim(0.5,2.5)

#====================================================================================================
#---------------------------------------------------------------------POBLACION EDAD PROMEDIO
#====================================================================================================


EdadMedia<-DF%>%
  dplyr::select(r104,fac00,r106)%>% 
  dplyr::rename(Sexo=r104)%>% 
  dplyr::group_by(Sexo)%>% 
  dplyr::summarise(poblacion=sum(fac00),sumaedad=sum(fac00*r106)) %>% 
  dplyr::mutate(edadMedia=(sumaedad/poblacion))
EdadMedia

#---GRAFICA

EdadMedia$Sexo<-as.factor(EdadMedia$Sexo) #---Transformo a factor
levels(EdadMedia$Sexo)<-c("Hombre","Mujer") #--se asigna los niveles

ggplot(EdadMedia, aes(x=Sexo, y=poblacion/1000000, fill=Sexo)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Edad media por sexo según población año 2023",
       subtitle ="(En millones de personas y años)",
       x = "Sexo", y = "Población (Millones)",caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+
  geom_text(data = EdadMedia, aes(label = round(edadMedia,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()


#====================================================================================================
#---------------------------------------------------------------------POBLACION DE 0 A 4 AÑOS
#====================================================================================================

Ind001<-DF %>%
  dplyr::select(r104,r106,fac00) %>% 
  dplyr::rename(Sexo=r104,Edad=r106) %>% 
  dplyr::filter(Edad<=4) %>% 
  dplyr::group_by(Sexo) %>% 
  dplyr::summarise(Poblacion=round(sum(fac00),digits = 0)) %>% 
  dplyr::mutate(PopT=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=round((Poblacion/PopT)*100,digits = 1))
Ind001

sum(Ind001$Poblacion)

Ind001.01<-DF %>%
  dplyr::select(r104,r106,fac00) %>% 
  dplyr::rename(Sexo=r104,Edad=r106) %>% 
  dplyr::filter(Edad<=4) %>% 
  dplyr::group_by(Edad) %>% 
  dplyr::summarise(Poblacion=round(sum(fac00),digits = 0)) %>% 
  dplyr::mutate(PopT=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=round((Poblacion/PopT)*100,digits = 1))
Ind001.01


#====================================================================================================
#---------------------------------------------------------------------POBLACION GEOGRÁFICA
#====================================================================================================


#--POR AREA

graf<-EHPM %>% 
  dplyr::select(area,fac00) %>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)
graf

#---GRAFICA

graf$area<-as.factor(graf$area) #---Transformo a factor
levels(graf$area)<-c("Rural","Urbano") #--se asigna los niveles

ggplot(graf, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="Población según área geográfica año 2023",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+
  geom_text(data = graf, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()+
  coord_flip()

#--v1
ggplot(graf, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="Población según área geográfica año 2019",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2019 DIGESTYC")+
  geom_text(data = graf, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()

#--v2
ggplot(graf, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="Población según área geográfica año 2019",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2019 DIGESTYC")+
  geom_text(data = graf, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()+
  facet_wrap(~area)


#--POR REGION

graf2<-EHPM %>% 
  dplyr::select(region,fac00) %>% 
  dplyr::group_by(region)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)

#---GRAFICA

graf2$region<-as.factor(graf2$region) #---Transformo a factor
levels(graf2$region)<-c("Occidental","Central I","Central II","Oriental","AMSS") #--se asigna los niveles

ggplot(graf2, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="Población según región geográfica año 2023",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+
  geom_text(data = graf2, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  #guides(fill=guide_legend(reverse=T))+
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()+
  coord_flip()

#--v1
ggplot(graf2, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="Población según región geográfica año 2023",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+
  geom_text(data = graf2, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()

#--v2
ggplot(graf2, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="Población según región geográfica año 2023",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2023 BCR")+
  geom_text(data = graf2, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()+
  facet_wrap(~region)

#--POR DEPARTAMENTO

depto<-DF %>% 
  dplyr::select(region,r004,fac00) %>% 
  dplyr::group_by(r004)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)

depto


#---GRAFICA

depto$r004<-as.factor(depto$r004) #---Transformo a factor
levels(depto$r004)<-c("Ahuachapán","Santa Ana","Sonsonate","Chalatenango","La Libertad",
                      "San Salvador","Cuscatlán","La Paz","Cabañas","San Vicente","Usulután",
                      "San Miguel","Morazán","La Unión") #--se asigna los niveles
ggplot(depto,
       aes(fill = r004,
           area = Porcentaje,
           label = r004)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre") +
  labs(title = "Peso de la población por departamentos (Fuente: BCR EHPM 2023)") +
  theme(legend.position = "none")

#==================================================================================================================
#/////////////////////////////                            ////////////////////////////////////////////////////////
#/////////////////////////////INDICADORES DE LA POBLACIÓN/////////////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================

#====================================================================================================
#---------------------------------------------------------------------POBLACION RAZON DE MASCULINIDAD
#====================================================================================================


H<-DF %>%
  dplyr::select(r104,fac00) %>% 
  dplyr::group_by(r104) %>%
  dplyr::filter(r104==1) %>%
  dplyr::summarise(PoblacionH=sum(fac00)) 
H

#-Mujeres
M<-DF %>%
  dplyr::select(r104,fac00) %>% 
  dplyr::group_by(r104) %>%
  dplyr::filter(r104==2) %>%
  dplyr::summarise(PoblacionM=sum(fac00)) 

M

#-Razon de masculinidad

RM<-(H/M)*100 

RM

#-Razon de masculininad forma Data Table
H2<-DT[r104==1,.(Hombres=sum(fac00))]

H2

M2<-DT[r104==2,.(Mujeres=sum(fac00))]

M2

RM2<-(H2/M2)*100
RM2

#====================================================================================================
#---------------------------------------------------------------------POBLACION RAZON DE DEPENDENCIA
#====================================================================================================


#-Menores de 16 años

M16<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106<16) %>%
  dplyr::summarise(PoblacionMenor16=sum(fac00)) 

M16

#-Mayores de 64 años

M64<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>64) %>%
  dplyr::summarise(PoblacionMayor64=sum(fac00)) 

M64

#Numerador
SumM16M64<-sum(M16,M64)
SumM16M64

#-Denominador
PobOcup<-sum(DF$fac00)-SumM16M64
PobOcup

RD1<-(SumM16M64/PobOcup)*100
RD1

#------------------Forma EHPM
#--Poblacion ocupada
dplyr::glimpse(DF$actpr2012)

PobOcup2<-DF %>% 
  dplyr::select(r106,actpr2012,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012==10) %>% 
  dplyr::summarise(PobOcupada=sum(fac00))

PobOcup2

#--Población total
PobTotal<-DF %>% summarise(Poblacion=sum(fac00))

PobTotal

#--Población dependiente
PobDep<-PobTotal-PobOcup2

PobDep

#--Razón de dependencia
RD2<-(PobDep/PobOcup2)*100
RD2

RD2<-(PobDep/PobOcup2)
RD2

RD2_EHPM<-(PobDep/PobTotal)

#====================================================================================================
#---------------------------------------------------------------------------POBLACIÓN RAZÓN DE VEJEZ
#====================================================================================================

#---Poblacion mayor o igual a 65 años
M65<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=65) %>%
  dplyr::summarise(PoblacionMayor65=sum(fac00)) 

M65

#----Población económicamente activa
Pea<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=15&r106<65) %>%
  dplyr::summarise(PoblacionActiva=sum(fac00)) 

Pea


RV<-(M65/Pea)*100

RV


#====================================================================================================
#------------------------------------------------------------------POBLACION CONDICION DE ALFABETISMO
#====================================================================================================


str(EHPM$r106)
str(EHPM$r202a)

 alfa<-DF%>%
  dplyr::select(r106,r202a,r104,fac00) %>% 
  dplyr::filter(r106>=10) %>% 
  dplyr::group_by(r202a)%>% 
  dplyr::summarise(Poblacion=sum(fac00))%>%
  dplyr::mutate(PoblacionTotal_10=sum(Poblacion))%>%
  dplyr::mutate(CondicionAlfabetizado=Poblacion/PoblacionTotal_10, 
                Porc=scales::percent(CondicionAlfabetizado))

#---GRAFICA
p <- ggplot(alfa, aes(r202a, fill = r104)) +
  xlab(NULL) + ylab(NULL)


alfa$r202a<- as.factor(alfa$r202a)
levels(alfa$r202a)<- c("Si sabe leer","No sabe leer","Solo leer")

p + geom_bar(position = "dodge",aes(weight = fac00/1000000))+
  labs(title ="Población según condición de alfabetismo por género",
       subtitle ="(En millones de personas)",
       x = "Condición de Alfabetismo", y = "Millones personas",
       caption = "Elaboración propia con información de la Base EHPM 2023 BCR") 