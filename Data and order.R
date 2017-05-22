####################################################################
##############################Carga de programas ###################
####################################################################
library("readr", "dplyr","stringr", "downloader", "downloader", "rgeos", "ggplot2","viridis","raster", "htmltools")
library("htmlTable","scales", "broom", "tidyr", "maptools","rgdal","gsubfn", "foreign")
#####################################################################
#A.Descargar bases de datos, resultados a nivel Secci�n y casilla en .csv, 
#####################################################################
###1.Datos electorales
#datos de http://siceef.ine.mx/downloadDB.html
###1.1 Diputados Mayoria Relativa 2015###############################
#download.file("http://siceef.ine.mx/BD/DiputadosMR2015Seccion.csv", "DiputadosMR2015Seccion.csv")
#1.2.1 Presidente 2012 Casilla
#download.file("http://siceef.ine.mx/BD/Presidente2012.csv", "Presidente2012Casilla.csv")
#1.2.3 Presidente 2012 Secci�n
#download.file("http://siceef.ine.mx/BD/Presidente2012Seccion.csv", "Presidente2012Seccion.csv")



#####################################################################
####################Presidente 2012##################################
#####################################################################
P2012Secc <- tbl_df(read.csv("Presidente2012Seccion.csv"))
############Presidente 2012 secci�n
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
##############################Porcentajes
MunPres12 <- mutate(MunPres12, POR_NULOS = NUM_VOTOS_NULOS/TOTAL)
#Unir con el nombre de los estados
temp<-dplyr::select(P2012Secc, ID_ESTADO, CVUN, NOMBRE_ESTADO, MUNICIPIO, ID_MUNICIPIO)
temp<- unique(temp[complete.cases(temp),])
temp$MunMin <- tolower(x = temp$MUNICIPIO) #municipios en min�sculas, fuente datos electorales, 2446
#####Nombres de los municipios de la base de datos de resultados electorales en minusculas
########################################################################
#Shapefiles del Marco Geoestad�stico del INEGI m�s reciente, Junio 2016
########################################################################
#D################Datos del marco geoestad�stico m�s reciente 
#download.file("http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825217341_s.zip", "MarcoGeoJunioo2016.zip")
#unzip("MarcoGeoJunioo2016.zip", exdir = "Marco Geoestadistico")
list.files("./Marco Geoestadistico/conjunto_de_datos/")
MGEOINEGI<- readShapePoly("./Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp")
LINEGI <- tbl_df(MGEOINEGI@data)
#Tenemos la clave municipal que utiliza el INEGI en su marco geoestad�stico
#Es necesario pasarla a min�sculas y unirla a los datos electorales
#utilizando una separaci�n por estado y los nombres de los municipios
#eso nos empatar� la clave municipal utilizada por los datos electorales con 
#la clave municipal utilizada por el Marco Geoestad�stico
LINEGI$MunMin <- tolower(LINEGI$NOM_MUN) #minusculas datos del INEGI, 2448
#El nombre de los municipios de INEGI tiene acentos

head(LINEGI$MunMin)

iconv(LINEGI$MunMin,from="latin1",to="ASCII//TRANSLIT")



#Unamos los municipios separando por estado
A<-full_join(LINEGI, temp, by = "MunMin")
A[A$CVE_ENT == "03",]
temp[temp$ID_ESTADO ==3,]
sum(table(temp$ID_ESTADO))



############################################
#####Mapa con datos a nivel municipal
############################################
#muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2") #shape
muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2", encoding = "UTF-8") #shape
states <- readOGR("map/mge2013v6_2.shp", "mge2013v6_2")
states_df <- fortify(states)
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
#muns@data$id = as.numeric(muns@data$concat)
muns@data$id = as.numeric(muns@data$concat)
########meter ac� la informaci�n de resultados electorales
muns@data <- plyr::join(muns@data, MunPres12, by = "id")
muns_df <- fortify(muns,region = "concat")


#homologar los nombres de los municipios
muns@data[muns@data$CVE_ENT == 3, ]
sum(as.numeric(table(muns@data$CVE_ENT)))
#Mapas de los marcos geoestad�sticos m�s recientes

#remover caract�res
#empatar por proximidad
unique(filter(temp, ID_ESTADO== 3))[complete.cases(unique(filter(temp, ID_ESTADO== 3))),]




#####quitar acentos en el df de INEGI
#######Transformar a may�sculas
#########hacer Merge por ID estatal y despu�s nombre de municipio
#####ID INEGI

head(muns@data)
toupper(muns@data$NOM_MUN)




muns_df <- plyr::join(muns_df, MunPres12, by="id")
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

muns@data[muns@data$CVE_ENT == "03",]
order(-muns_df$POR_NULOS)

muns_df
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