
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-00-Mapas----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------MAPAS
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
library(leaflet)
library(sp)
library(cartography)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(sqldf)
library(tmap)
library(rmapshaper)
library(maps)
library(shinyjs)

#----------------Abriendo el shape file con los municipios
#-------Paleta tmaptools::palette_explorer()


#mapaSeg <- readOGR("/media/elias/ELIAS TRABAJO/CONSULTORIAS/IdeaData/Temas Investigacion/Docencia/BasesEHPM/Shapes/Ahuachapan1.shp",layer = "Ahuchapan1")

mtq2<-st_read("Shapes/Ahuachapan1.shp")
#mtq2<-st_read("/media/elias/ELIAS TRABAJO/CONSULTORIAS/IdeaData/Temas Investigacion/Docencia/BasesEHPM/Shapes/Ahuachapan1.shp")
#head(mapaSeg ,5)
head(mtq2,5)

plot(mapaSeg)
plot(mtq2)

tmap_mode("view")
tm_shape(mapaSeg) + tm_fill(col = "personas",alpha=0.8, style = "equal", id="CANTON",title = "personas",palette="")+
  tm_shape(mtq2)+tm_borders(col = NA, lwd = 1, lty = "solid")

#----Mapa ahuchapan
tmap_mode("view")
tm_shape(mtq2) + tm_fill(col = "personas",alpha=0.8, style = "pretty", id="CANTON",
                         title = "personas",palette="Oranges")+
  tm_shape(mtq2)+tm_borders(col = NA, lwd = 1, lty = "solid")

#==============Mapa 2


DB.Censo.Prod<-read_sav("Bases/PRODUCTORES.sav")#Base de prod censo
glimpse(DB.Censo.Prod)

DB.Censo.Prod2<-DB.Censo.Prod %>% 
  subset(select =c(-S01P02PN,-S01P02SN,-S01P02PA,-S01P02SA,-S01P02TEL,-S01P02CEL,-S01P03PN,-S01P03SN,-S01P03PA))

write_sav(DB.Censo.Prod2,"Bases/ProductoresNew.sav")

CensoSutSet<-DB.Censo.Prod2 %>% 
  subset(select=c(DEPDSC,MUNDSC,CAFDSC,S09C225,S09C226,S09C227,ACTDSC,CX,CY))

write_sav(CensoSutSet,"Bases/SubProductores.sav")

#-----Mapa

MapaC<-DB.Censo.Prod %>% 
  dplyr::filter(CAFDSC=="SI") %>% 
  dplyr::select(DEPDSC,MUNDSC,S01P02PN,S01P02SN,S01P02PA,S09C225,S09C226,S09C227,ACTDSC,CX,CY) 

MapaC<-DB.Censo.Prod %>% 
  dplyr::filter(CAFDSC=="SI") %>% 
  dplyr::select(DEPDSC,MUNDSC,S09C225,S09C226,S09C227,ACTDSC,CX,CY) 

library(leaflet)
#CartoDB.DarkMatter
#Esri.WorldStreetMap

leaflet() %>%
  addProviderTiles('OpenStreetMap.France')%>%
  #addTiles() %>% 
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  #addMarkers(lng=MapaC$CY, lat=MapaC$CX, popup=paste(MapaC$S01P02PN,MapaC$S01P02SN))
  addCircleMarkers(lng=MapaC$CY, lat=MapaC$CX, color = "#FF5733",radius = 2,
                   popup=paste(MapaC$S01P02PN,MapaC$S01P02SN,MapaC$S01P02PA,
                               "Bajío",MapaC$S09C225,"Media",MapaC$S09C226,
                               "Estricta Altura",MapaC$S09C227))



#===============================
#---------Forma ggplot
#===============================

#---Cargando el Shape file
mtq2<-st_read("Shapes/Ahuachapan1.shp")

head(mtq2,5)

#---Mapa ggplot
Map2<-ggplot(data=mtq2)+geom_sf()
Map2

Map3<-ggplot(mtq2) +
  geom_sf(aes(fill = personas ))+
  labs(title = "Mapa de segmentos: Ahuachapán",
       caption = "Fuente: DIGESTYC (2007)
                      Elaboración propia",
       x="Longitud",
       y="Latitud")+
  scale_fill_continuous(guide_legend(title = "Personas"))
Map3

