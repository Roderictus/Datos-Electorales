#mapas del Estado de México
library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)
library(stringr)
library(viridis)
library(extrafont)
library(foreign)

#Darle a los municipios los números de la base de datos del INEGI 
Catalogo_Municipal <- read.dbf("./cat_municipio_OCT2016.dbf") #mayor disparidad entre población femenina y masculina en el Estado
Catalogo_Municipal$CVE_INEGI <- str_c(str_pad(Catalogo_Municipal$CVE_ENT, width = 2, "left", "0"), 
                                      str_pad(Catalogo_Municipal$CVE_MUN, width = 3, "left", "0"))
Catalogo_Municipal$Municipio_Minuscula<- tolower(Catalogo_Municipal$NOM_MUN)
#length(unique(Catalogo_Municipal$CVE_INEGI))#2,458
P2012Mun$Municipio_Minuscula <- tolower(x = P2012Mun$Municipio)
colnames(P2012Mun)
dplyr::select(P2012Mun, CVUN, Municipio_Minuscula)

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
#

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
