

#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Scrip-00-APIS----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#------------------------------APIS
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
library(FAOSTAT)
library(dygraphs)
library(quarto)
library(rt)
library(downloader)
library(data.table)
library(jsonlite)
library(httr)
library(openssl)
library(curl)
library(devtools)
library(agregadorindicadores)
library(WDI)


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
#/////////////////////////////TRABAJANDO CON LAS APIS      ////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================

#====================================================================================Trabajando con FAOSTAT

# Buscar el codigo de pais que esta disponible
metadata_area <- read_dimension_metadata("RL", "area")
# Buscar cual codigo de items esta disponible
metadata_item <- read_dimension_metadata("RL", "item")
# Buscar cual codigo de elemento esta dispoble
metadata_element <- read_dimension_metadata("RL", "element")


# Get data for Cropland (6620) Area (5110) in Antigua and Barbuda (8) in 2017
Barbuda= read_fao(area_codes = "8", element_codes = "5110", item_codes = "6620", 
                  year_codes = "2017")
# Load cropland area for a range of year
df= read_fao(area_codes = "60", element_codes = "5110", item_codes = "6602", 
             year_codes = 1961:2020)

plot(df$Value,type = "l")

# Crear el gráfico de línea con suavizado
ggplot(data = df, aes(x = Year, y = Value)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +  # Agregar la línea suavizada
  labs(title = "Evolución de Hectáreas con Suavizado",
       x = "Año",
       y = "Hectáreas")



df23<-df %>% select(Year,Value)
df23$Value<-log (df23$Value)
dygraph(df23)


dygraph(df23,main = "El Salvador Tendencia de las áreas de cultivos (1960-2020)",
        xlab = "Años",
        ylab = "Areas cultivables(1000 ha)") %>%
  dyShading(from = "1961", to = "1970",color = "#FFE6E6") %>%
  dyShading(from = "1980", to = "1985",color = "#D1F2EB") %>%
  dyShading(from = "1990", to = "2000",color = "#CCEBD6")



#================================================================================Trabajando con Banco Mundial
library(WDI)
library(ggplot2)
install.packages('WDI')

pais<-WDI(country = "all")



# Otro ejemplo
#Grab GNI per capita data for Chile, Hungary and Uruguay

dat = WDI(indicator='NY.GNP.PCAP.CD', country=c('CL','HU','UY'), start=1960, end=2012)

#a quick plot with legend, title and lable

dat = WDI(indicator='NY.GNP.PCAP.CD', country=c('CL','HU','UY'), start=1960, end=2012)
ggplot(dat, aes(year, NY.GNP.PCAP.CD, color=country)) + geom_line() +
  labs(title ="PIB Percapita de Chile, Hungria y Uruguay",
       subtitle ="Serie de 1960 a 2022", tag="Graf.01",caption = "Fuente:Base de BM",
       x = "Años", y = "US$")



datSV = WDI(indicator='NY.GNP.PCAP.CD', country=c('CL','UY','SV'), start=1965, end=2022)
ggplot(datSV, aes(year, NY.GNP.PCAP.CD, color=country)) + geom_line() +
  labs(title ="PIB Percapita de Chile, El Salvador y Uruguay",
       subtitle ="Serie de 1960 a 2022", tag="Graf.01",caption = "Fuente:Base de BM",
       x = "Años", y = "US$")

#Puede buscar datos utilizando palabras clave en . Por ejemplo, si buscas datos sobre el 
#Producto Interior Bruto:WDIsearch

b<-WDIsearch('gdp')

b1<-WDIsearch('gdp')[1:10,]

dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('MX','CA','US'), start=1960, end=2012)
head(dat)

library(ggplot2)
ggplot(dat, aes(year, NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')


#Algunas series del Banco Mundial están disponibles con una frecuencia mensual o trimestral. Puedes descargarlos simplemente 
#usando los argumentos and:startend

tri<-WDI(indicator = 'DPANUSSPB', country = 'CHN', start = '2012M01', end = '2012M05')


#Si se asigna un nombre al vector al que se proporciona, la función cambiará automáticamente el 
#nombre de las columnas siempre que sea posible

dat <- WDI(indicator = c("gdp_per_capita" = "NY.GDP.PCAP.KD",
                         "population" = "SP.POP.TOTL"))

#Para acelerar la búsqueda, se envía con una lista local de todas las series WDI disponibles. Esta lista se actualizará con regularidad, pero es posible que desee actualizarla manualmente para obtener acceso a las series de datos más recientes. 
#Para ello, utilice la función:WDIcache

new_cache = WDIcache()
WDIsearch('gdp', cache=new_cache)



#====================================================================================Trabajando con BID

install.packages('devtools')
library(devtools)
install_github('EL-BID/Libreria-R-Numeros-para-el-Desarrollo')
install_github("arcuellar88/govdata360R")
install_github('EL-BID/Agregador-de-indicadores')
library(agregadorindicadores)

#2. Buscar indicadores
#En este caso buscaremos indicadores relacionados con desempleo.

df<-ind_search(pattern="unemployment")

#3. Descargar informacion de los indicadores
#En este ejemplo vamos a descargar los datos de dos indicadores para dos países entre el 2014 y el 2015

data<-ai(indicator = c("SL.UEM.TOTL.FE.ZS","SOC_6562"), country = c("CO","PE"),startdate = 2014, enddate=2015)


#Plot one indicator "Agricultural land (% of land area)" for 4 countries in 2014
df<-ai(indicator = "AG.LND.AGRI.ZS", country = c("CO", "PE","ZA","US"), startdate = 2014)

df$fCountry <- factor(df$country)

p <- ggplot(df, aes(x=fCountry, y=value,colour=fCountry,hover = indicator))  +
  geom_point(shape=1) 
p <- ggplotly(p)
p

#==================================================================================================================
#/////////////////////////////                            ////////////////////////////////////////////////////////
#/////////////////////////////TRABAJANDO CON LAS APIS UN      ////////////////////////////////////////////////
#/////////////////////////////                           ////////////////////////////////////////////////////////
#==================================================================================================================

#==================================================================================================================================
# API Naciones Unidas Población=https://population.un.org/dataportal/about/dataapi

library(jsonlite)
library(httr)

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Creates the target URL, indicators, in this instance
target <- paste0(base_url, "/indicators/")

# Get the response, which includes data as well as information on pagination and number of records
response <- fromJSON(target)

# Get the first page of data
df_Ind <- response$data

# Loop until there are new pages with data
while (!is.null(response$nextPage)){
  
  #call the API for the next page
  response <- fromJSON(response$nextPage)
  
  #add the data of the new page to the data.frame with the data of the precious pages
  df <- rbind(df, response$data)
  
}

status_code(GET(target))


#-----------------Example 2: Returning a list of geographical areas

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Update relative path to retrieve records on locations
target <- paste0(base_url, "/locations/")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_Loca<- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df_Loca <- rbind(df_Loca, response$data)
  
}


#---Example 3: Returning a single indicator for a single geographical area


# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Update the relative path to search for data on a specific indicator, location, and for specific years
target <- paste0(base_url, "/data/indicators/1/locations/4/start/2005/end/2010")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_Afga <- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df_Afga<- rbind(df, response$data)
  
}

df2_Afga <- df_Afga[(df_Afga$variant=="Median") & df_Afga$category=="All women", names(df_Afga) %in% c("timeMid", "value", "category", "variant")]
df2_Afga <- df_Afga[(df_Afga$variant=="Median") , names(df_Afga) %in% c("timeMid", "value", "category", "variant")]


p<-ggplot(df2_Afga,aes(x=timeMid, y=value,group=category, fill=category))+
  geom_area()+
  ggtitle("Prevalencia anticonceptiva: Cualquier método (Porcentaje)")
p

#====================================================
# Update the relative path to search for data on a specific indicator, location, and for specific years
# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"
target <- paste0(base_url, "/data/indicators/1/locations/222/start/1990/end/2021")

#--Otra forma de filtrar

base_url <- "https://population.un.org/dataportalapi/api/v1"
Indicador<-1
Pais<-222
AñoI<-1990
AñoF<-2021

target<-paste0(base_url,"/data/indicators/",Indicador,"/locations/",Pais,"/start/",AñoI,"/end/",AñoF)

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_SV <- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df_SV<- rbind(df_SV, response$data)
  
}

#--------Graficando
df2_SV <- df_SV[(df_SV$variant=="Median") , names(df_SV) %in% c("timeLabel","timeMid", "value", "category", "variant")]


df2_SV$Estado <- as.factor(df2_SV$category)
levels(df2_SV$Estado) <- c("Todas las Mujeres","Casadas o Unión Libre","Divorciadas")

p<-ggplot(df2_SV,aes(x=timeLabel, y=value,group=Estado, fill=Estado))+
  geom_area()+
  #ggtitle("El Salvador: Prevalencia anticonceptiva, Cualquier método (Porcentaje)")+
  labs(title ="El Salvador: Prevalencia anticonceptiva, Cualquier método",
       subtitle ="(En Porcentajes)",
       x = "Años", y = "Porcentajes",
       caption = "Elaboración propia con información de Naciones Unidas")+
  #geom_text(data = df2_SV, aes(label = round(value,digits=2)),size = 4)+
  facet_wrap(~Estado, ncol = 1,strip.position = "top") 

p+scale_fill_brewer(palette="YlOrBr")







