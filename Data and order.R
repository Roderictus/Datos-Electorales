####################################################################
##############################Carga de programas ###################
####################################################################
library("tidyverse")
library("readr")
library("dplyr")
library("stringr")
library("downloader")
library("rgeos")
library("ggplot2")
library("viridis")
library("raster")
library("htmltools")
library("htmlTable")
library("scales")
library("broom")
library("tidyr")
library("maptools")
library("rgdal")
library("gsubfn")
library("foreign")
library("maptools")
library("RColorBrewer")
library("ggmap")
library("data.table")

#####################################################################
####### A.Descargar bases de datos, resultados a nivel Sección y casilla en .csv, 
#####################################################################
###1.Datos electorales
#datos de http://siceef.ine.mx/downloadDB.html
###1.1 Diputados Mayoria Relativa 2015###############################
#download.file("http://siceef.ine.mx/BD/DiputadosMR2015Seccion.csv", "DiputadosMR2015Seccion.csv")
#1.2.1 Presidente 2012 Casilla
#download.file("http://siceef.ine.mx/BD/Presidente2012.csv", "Presidente2012Casilla.csv")
#1.2.3 Presidente 2012 Sección
#download.file("http://siceef.ine.mx/BD/Presidente2012Seccion.csv", "Presidente2012Seccion.csv")

#####################################################################
####################Presidente 2012##################################
#####################################################################
P2012Secc <- tbl_df(read.csv("Presidente2012Seccion.csv"))
############Presidente 2012 sección
############ID para municipios
#max(P2012Secc$ID_ESTADO) #32
#max(P2012Secc$ID_MUNICIPIO, na.rm = TRUE) #570
##############################ID para secciones######################
P2012Secc$CVUN <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))
####################Resultados a nivel municipal######################
MunPres12 <-P2012Secc %>%
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

##############################Porcentajes##############################
MunPres12 <- mutate(MunPres12, POR_NULOS = NUM_VOTOS_NULOS/TOTAL, 
                    POR_PRI_ALIANZA = (PRI +PRI_PVEM)/TOTAL)
#Unir con el nombre de los estados
temp<-dplyr::select(P2012Secc, ID_ESTADO, CVUN, NOMBRE_ESTADO, MUNICIPIO, ID_MUNICIPIO)
temp<- unique(temp[complete.cases(temp),])
#####Nombres de los municipios de la base de datos de resultados electorales en minusculas
temp$MunMin <- tolower(x = temp$MUNICIPIO) #municipios en minúsculas, fuente datos electorales, 2446
########################################################################
#Shapefiles del Marco Geoestadístico del INEGI más reciente, Junio 2016
########################################################################
#D################Datos del marco geoestadístico más reciente 
#download.file("http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825217341_s.zip", "MarcoGeoJunioo2016.zip")
#unzip("MarcoGeoJunioo2016.zip", exdir = "Marco Geoestadistico")
#list.files("./Marco Geoestadistico/conjunto_de_datos/")
MGEOINEGI<- readOGR("./Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp")
LINEGI <- tbl_df(MGEOINEGI@data)
#Tenemos la clave municipal que utiliza el INEGI en su marco geoestadístico
#Es necesario pasarla a minúsculas y unirla a los datos electorales
#utilizando una separación por estado y los nombres de los municipios
#eso nos empatará la clave municipal utilizada por los datos electorales con 
#la clave municipal utilizada por el Marco Geoestadístico
LINEGI$MunMin <- tolower(LINEGI$NOM_MUN) #minusculas datos del INEGI, 2448
#El nombre de los municipios de INEGI tiene acentos
LINEGI$MunMin<-iconv(LINEGI$MunMin,from="latin1",to="ASCII//TRANSLIT")
#Unamos los municipios separando por estado
#intentaré unir solo para los municipios que aparezcan en la lista del INEGI
#left join primer término INEGI, el segundo grupo debe de estar contenido en el primero 
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
#Aqui vemos que ya tenemos forma de empatar los resultados del INEGI con los datos electorales, 
#molesta decisión del INE no seguir con la clasificación del marcogeoestadístico nacional
#Eso o hay algo que no entiendo
#Tenemos NAs en ClaveMun$CVUN
ClaveMun[is.na(ClaveMun$CVUN),]$CVUN<-ClaveMun[is.na(ClaveMun$CVUN),]$ClaveINEGI#69 casos, los llenamos con la clave INEGI
#ya no hay nas!
############################################
#####Mapa con datos a nivel municipal
############################################
##########Municipios

#muns<- readOGR("./Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp",
               "areas_geoestadisticas_municipales")#un archivo demasiado grande 

muns<- readOGR("./Mapa muy reducido/areas_geoestadisticas_municipales.shp",
               "areas_geoestadisticas_municipales")#un archivo pequeño al que le hacen falta cosas
rm(MGEOINEGI)

#muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2")
states <- readOGR("Marco Geoestadistico/MapasSimplificadas/areas_geoestadisticas_estatales.shp", "areas_geoestadisticas_estatales")

#Pegar los datos a los municipios antes de hacerles fortify
#el paso más importante posiblemente,
#unir al marco de los datos geográficos los resultados del análisis 
#electoral

#Tres variables para pegarlo, probablemente no haya necesidad de las tres
muns@data$concat <- paste(muns@data$CVE_ENT,muns@data$CVE_MUN, sep = "")
muns@data$ClaveINEGI <- paste(muns@data$CVE_ENT,muns@data$CVE_MUN, sep = "")
muns@data$id <-as.numeric(muns@data$concat)

muns

# MunPres12 + Clave Mun usando CVUN
DataEdo<-right_join(x = MunPres12, y = ClaveMun, by ="CVUN")

#Juntamos usando clave Inegi, renombrandola a concat
DataEdo$concat <- DataEdo$ClaveINEGI.x
muns@data <- plyr::join(muns@data, DataEdo, by = "concat")
#################################################################
####################Estados######################################
#################################################################
#states<-readOGR("Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_estatales.shp", 
                #"areas_geoestadisticas_estatales")#Este es un archivo demasiado grande

states_df<-fortify(states)
bb<- bbox(as(extent(muns), "SpatialPolygons"))

## Theme for maps
theme_bare <-theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())

#muns_df <- fortify(muns, region = "concat")
muns_df <- fortify(muns)

DataEdo$id <- DataEdo$concat
muns_df<-plyr::join(muns_df, DataEdo, by = "id")
colnames(muns_df)
#muns_df<-tbl_df(muns_df)
#muns_df<-dplyr::select(muns_df,long, lat, order, hole, piece, id, group, CVUN, 
#                POR_NULOS, TOTAL_VOTOS, LISTA_NOMINAL, POR_PRI_ALIANZA)

gc()


MAPA_NULOS <-ggplot() +
  geom_map(data = muns_df, map = muns_df, 
           aes(map_id = id, x = long, y = lat, group = group, fill = POR_NULOS),
           color = "white", size = 0.04) +
  geom_polygon(data = states_df,
               aes(long, lat, group = group),
               color = "#aaaaaa", fill = NA, size = 0.3) +
  scale_fill_viridis("Porcentaje", trans = "sqrt", labels = percent) +
  coord_map() +
  labs(x = "", y = "", 
       title = "") +
  coord_map("albers", lat0 = bb[2,1], lat1 = bb[2,2]) +
  theme_bw() +
  theme(legend.key = element_rect(fill = NA)) +
  theme_bare

#ggsave(filename = "Mapa1.png", plot = MAPA_NULOS, width = 10, height = 8, units = "cm")

