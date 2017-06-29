#Base de datos a nivel municipal de resultados de la votación presidencial 2012
#con identificador de municipio original e identificador de municipio que corresponda
#al marco geoestadístico del INEGI

library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)
library(stringr)
library(data.table)

#########################################################################################################
########################  Datos de elecciones a presidente  #############################################
#########################################################################################################
#Juntar los tres archivos por seccion
P2000Secc<-fread(input = "http://siceef.ine.mx/BD/Presidente2000Seccion.csv", 
                 sep = ",", encoding = "UTF-8")
colnames(P2000Secc)<- paste("P2000", colnames(P2006Secc), sep = "_")
P2006Secc<-fread(input = "http://siceef.ine.mx/BD/Presidente2006Seccion.csv", 
                 sep = ",", encoding = "UTF-8")
colnames(P2006Secc)<- paste("P2006", colnames(P2006Secc), sep = "_")
P2012Secc<-fread(input = "http://siceef.ine.mx/BD/Presidente2012Seccion.csv", 
                 sep = ",", encoding = "UTF-8")
colnames(P2012Secc)<- paste("P2012", colnames(P2012Secc), sep = "_")


colnames(P2000Secc)
colnames(P2006Secc)
colnames(P2012Secc)



#download.file("http://siceef.ine.mx/BD/Presidente2000Seccion.csv", "./Elecciones/Presidente2000Seccion.csv")
download.file(url = "http://siceef.ine.mx/BD/Presidente2006Seccion.csv", 
              destfile = "./Elecciones/Presidente2006Seccion.csv",
              method = "wb")
#download.file("http://siceef.ine.mx/BD/Presidente2012Seccion.csv", "./Elecciones/Presidente2012Seccion.csv")

####################################################################################
###################   Presidente 2012 Sección    ###################################
####################################################################################


P2012Secc <- read.csv("Presidente2012Seccion.csv")
P2012Secc$CVUN <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))

P2012Mun <-P2012Secc %>%
  group_by(CVUN =as.factor(P2012Secc$CVUN)) %>%
  summarise(TOTAL = sum(TOTAL_VOTOS, na.rm =TRUE), PAN = sum(PAN, na.rm = TRUE), PRI = sum(PRI, na.rm = TRUE), 
            PRD = sum(PRD, na.rm = TRUE), PVEM = sum(PVEM, na.rm = TRUE), PT = sum(PT, na.rm = TRUE),
            MC = sum(MC, na.rm = TRUE), NVA_ALIANZA = sum(NVA_ALIANZA, na.rm = TRUE),
            PRI_PVEM = sum(PRI_PVEM, na.rm = TRUE), PRD_PT_MC = sum(PRD_PT_MC, na.rm = TRUE),
            PRD_PT = sum(PRD_PT, na.rm = TRUE),PRD_MC = sum(PRD_MC, na.rm = TRUE),
            PT_MC = sum(PRD_PT_MC, na.rm = TRUE),NUM_VOTOS_NULOS = sum(NUM_VOTOS_NULOS, na.rm = TRUE),
            TOTAL_VOTOS = sum(TOTAL_VOTOS, na.rm = TRUE), LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = TRUE), 
            TOTAL_CASILLAS = sum(CASILLAS, na.rm = TRUE), MUNICIPIO = unique(MUNICIPIO), 
            ID_MUNICIPIO_ELECTORAL = unique(ID_MUNICIPIO))

P2012Mun <- mutate(P2012Mun, POR_NULOS = NUM_VOTOS_NULOS/TOTAL, 
                    POR_PRI_ALIANZA = (PRI +PRI_PVEM)/TOTAL, POR_PRD_ALIANZA = (PRD_PT+PRD_PT_MC+PRD_MC)/TOTAL)

temp<-dplyr::select(P2012Secc, ID_ESTADO, CVUN, NOMBRE_ESTADO, MUNICIPIO, ID_MUNICIPIO)
temp<- unique(temp[complete.cases(temp),])
#####Nombres de los municipios de la base de datos de resultados electorales en minusculas
temp$MunMin <- tolower(x = temp$MUNICIPIO) #municipios en minúsculas, fuente datos electorales, 2446

MGEOINEGI<- readOGR("./Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp")
LINEGI <- MGEOINEGI@data
rm(MGEOINEGI)
LINEGI$MunMin <- tolower(LINEGI$NOM_MUN) #minusculas datos del INEGI, 2448
LINEGI$MunMin<-iconv(LINEGI$MunMin,from="latin1",to="ASCII//TRANSLIT")
MunicipiosMapa<- data.frame()
list <-LINEGI$CVE_ENT

for ( i in 1:32) {
  A2 <- dplyr::filter(temp, temp$ID_ESTADO == i )
  B2 <- dplyr::filter(LINEGI, LINEGI$CVE_ENT== unique(LINEGI$CVE_ENT)[i])
  print(unique(LINEGI$CVE_ENT)[i])
  MunicipiosMapa<-rbind(MunicipiosMapa,left_join(B2,A2, by = "MunMin")) #INEGI a la izquierda
}

ClaveMun<-dplyr::select(MunicipiosMapa, CVE_ENT, CVE_MUN, NOM_MUN, CVUN)
#P2012Secc$CVUN <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))
ClaveMun$ClaveINEGI <- str_c(str_pad(ClaveMun$CVE_ENT, width =2, "left", "0"), str_pad(ClaveMun$CVE_MUN, width = 3, "left", "0"))
ClaveMun[is.na(ClaveMun$CVUN),]$CVUN<-ClaveMun[is.na(ClaveMun$CVUN),]$ClaveINEGI#69 casos, los llenamos con la clave INEGI

####################################################################################
###################   Presidente 2006 Sección    ###################################
####################################################################################
#write.csv(x = P2006Secc, file = "Elecciones/Presidente2006Seccion.csv")

P2006Secc$CVUN <- str_c(str_pad(P2006Secc$ID_ESTADO, width = 2, "left", "0"),
                        str_pad(P2006Secc$ID_MUNICIPIO, width = 3, "left", "0"))
#hay un ID_MUNICIPIO de 580, es voto en el extranjero, 300 de ellos
colnames(P2006Secc)
table(P2006Secc)
table(P2006Secc$CVUN)
#coalición por el bien de todos
#Alianza por México
#


P2006Mun <-P2006Secc %>%
  group_by(CVUN =as.factor(P2006Secc$CVUN)) %>%
  summarise(TOTAL = sum(TOTAL_VOTOS, na.rm =TRUE), PAN = sum(PAN, na.rm = TRUE), PRI = sum(PRI, na.rm = TRUE), 
            PRD = sum(PRD, na.rm = TRUE), PVEM = sum(PVEM, na.rm = TRUE), PT = sum(PT, na.rm = TRUE),
            MC = sum(MC, na.rm = TRUE), NVA_ALIANZA = sum(NVA_ALIANZA, na.rm = TRUE),
            PRI_PVEM = sum(PRI_PVEM, na.rm = TRUE), PRD_PT_MC = sum(PRD_PT_MC, na.rm = TRUE),
            PRD_PT = sum(PRD_PT, na.rm = TRUE),PRD_MC = sum(PRD_MC, na.rm = TRUE),
            PT_MC = sum(PRD_PT_MC, na.rm = TRUE),NUM_VOTOS_NULOS = sum(NUM_VOTOS_NULOS, na.rm = TRUE),
            TOTAL_VOTOS = sum(TOTAL_VOTOS, na.rm = TRUE), LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = TRUE), 
            TOTAL_CASILLAS = sum(CASILLAS, na.rm = TRUE), MUNICIPIO = unique(MUNICIPIO), 
            ID_MUNICIPIO_ELECTORAL = unique(ID_MUNICIPIO))

P2012Mun <- mutate(P2012Mun, POR_NULOS = NUM_VOTOS_NULOS/TOTAL, 
                   POR_PRI_ALIANZA = (PRI +PRI_PVEM)/TOTAL, POR_PRD_ALIANZA = (PRD_PT+PRD_PT_MC+PRD_MC)/TOTAL)

temp<-dplyr::select(P2012Secc, ID_ESTADO, CVUN, NOMBRE_ESTADO, MUNICIPIO, ID_MUNICIPIO)
temp<- unique(temp[complete.cases(temp),])
#####Nombres de los municipios de la base de datos de resultados electorales en minusculas
temp$MunMin <- tolower(x = temp$MUNICIPIO) 




####################################################################################

muns<- readOGR("./Mapa muy reducido/areas_geoestadisticas_municipales.shp",
               "areas_geoestadisticas_municipales")
states <- readOGR("Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_estatales.shp", "areas_geoestadisticas_estatales")
States_Map <- fortify(states)

muns@data$ClaveINEGI <- paste(muns@data$CVE_ENT,muns@data$CVE_MUN, sep = "")
muns@data$id <- as.numeric(muns@data$ClaveINEGI)
#ClaveMun nos permite hablar entre los datos a nivel municipal y los datos del MGEOINEG
P2012Mun <-right_join(x = P2012Mun, y = ClaveMun, by = "CVUN")
Muns_Map <-fortify(muns , region = "ClaveINEGI")
P2012Mun$id<-P2012Mun$ClaveINEGI

Muns_Map<-plyr::join(Muns_Map, P2012Mun, by = "id")
bb<- bbox(as(extent(muns), "SpatialPolygons"))

writeOGR(obj = muns,dsn = "Shapefiles Creados", layer = Muns_Map ,driver = "ESRI Shapefile")

head(muns@data)
#######Hasta aquí base de datos, sigue mapa


