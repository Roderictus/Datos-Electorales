####################################################################
##############################Carga de programas ###################
####################################################################
library("readr", "dplyr","stringr", "downloader", "downloader", "rgeos", "ggplot2","viridis","raster", "htmltools")
library("htmlTable","scales", "broom", "tidyr", "maptools","rgdal","gsubfn", "foreign")
#####################################################################
#A.Descargar bases de datos, resultados a nivel Sección y casilla en .csv, 
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
MunPres12 <- mutate(MunPres12, POR_NULOS = NUM_VOTOS_NULOS/TOTAL)
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
list.files("./Marco Geoestadistico/conjunto_de_datos/")
MGEOINEGI<- readShapePoly("./Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp")
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
colnames(temp)

MunicipiosMapa<- data.frame()
list <-LINEGI$CVE_ENT

for ( i in 1:32) {
      A2 <- dplyr::filter(temp, temp$ID_ESTADO == i )
      B2 <- dplyr::filter(LINEGI, LINEGI$CVE_ENT== unique(LINEGI$CVE_ENT)[i])
      print(unique(LINEGI$CVE_ENT)[i])
      MunicipiosMapa<-rbind(MunicipiosMapa,left_join(B2,A2, by = "MunMin")) #INEGI a la izquierda
}

ClaveMun<-dplyr::select(MunicipiosMapa, CVE_ENT, CVE_MUN, NOM_MUN, CVUN)

P2012Secc$CVUN <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))

ClaveMun$ClaveINEGI <- str_c(str_pad(ClaveMun$CVE_ENT, width =2, "left", "0"), str_pad(ClaveMun$CVE_MUN, width = 3, "left", "0"))

ClaveMun[ClaveMun$CVE_ENT == "03",]

left_join(MunPres12,ClaveMun, by = "CVUN")
############################################
#####Mapa con datos a nivel municipal
############################################
area<- readShapePoly("./Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp")
area.points<-fortify(area)
area@data


#muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2") #shape
#states_df <- fortify(states)
#bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
#muns@data$id = as.numeric(muns@data$concat)



muns@data$id = as.numeric(muns@data$concat)
########meter acá la información de resultados electorales
muns@data <- plyr::join(muns@data, MunPres12, by = "id")
muns_df <- fortify(muns,region = "concat")
muns_df <- plyr::join(muns_df, MunPres12, by="id")


#muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2") #shape
muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2", encoding = "UTF-8") #shape
states <- readOGR("map/mge2013v6_2.shp", "mge2013v6_2")
states_df <- fortify(states)
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
#muns@data$id = as.numeric(muns@data$concat)
muns@data$id = as.numeric(muns@data$concat)
########meter acá la información de resultados electorales
muns@data <- plyr::join(muns@data, MunPres12, by = "id")
muns_df <- fortify(muns,region = "concat")

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

MAPA <- ggplot()+
      geom_map(data = muns_df, map = muns_df, 
               aes(map_id = id, x = long, y = lat, group = group, fill= POR_NULOS),
               color = "white", size = 0.04) + geom_polygon(data = states_df,
                   aes(long, lat, group = group),
                   color = "#aaaaaa", fill = NA, size = 0.3) +
      scale_fill_viridis("Porcentaje", trans = "sqrt", labels = percent) +
      coord_map() +
      labs(x = "", y = "", title = "")
coord_map("albers", lat0 = bb[2,1], lat1 = bb[2,2]) +
      theme_bw() +
      theme(legend.key = element_rect(fill = NA)) +
      theme_bare

MAPA
