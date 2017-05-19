library (foreign)
library(dplyr)
library(stringr) # Para str_c
library(rgdal)#ReadOGR
library(ggplot2) #fortify
library(raster)#extent
library(rgeos)
library(rgdal)
library(maptools)

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



#Descargar bases de datos, resultados a nivel Sección y casilla en .csv, 
#datos de http://siceef.ine.mx/downloadDB.html

#Base de datos Elección Extraordinaria de Diputados de Mayoria Relativa Proceso Electoral 2014-2015
#Sección
#Casilla

#Diputados Mayoria Relativa 2015
#Sección
#Casilla
#####################################################################
##########################Presidente 2012############################
#####################################################################

#Sección
#download.file("http://siceef.ine.mx/BD/Presidente2012Seccion.csv", "Presidente2012Seccion.csv")

#Casilla
#download.file("http://siceef.ine.mx/BD/Presidente2012.csv", "Presidente2012Casilla.csv")

#Mapa electoral de la votación por presidente 2012
P2012Secc <- tbl_df(read.csv("Presidente2012Seccion.csv"))
P2012Cas  <- tbl_df(read.csv("Presidente2012.csv"))

############Presidente 2012 sección

############ID para municipios
#max(P2012Secc$ID_ESTADO) #32
#max(P2012Secc$ID_MUNICIPIO, na.rm = TRUE) #570
P2012Secc$id <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))
####ID para secciones 
max(P2012Cas$SECCION) #6393



MunPres12 <-P2012Secc %>%
      group_by(id =as.factor(P2012Secc$CVUN)) %>%
      summarise(TOTAL = sum(TOTAL_VOTOS, na.rm =TRUE), PAN = sum(PAN, na.rm = TRUE), PRI = sum(PRI, na.rm = TRUE), 
                PRD = sum(PRD, na.rm = TRUE), PVEM = sum(PVEM, na.rm = TRUE), PT = sum(PT, na.rm = TRUE),
                MC = sum(MC, na.rm = TRUE), NVA_ALIANZA = sum(NVA_ALIANZA, na.rm = TRUE),
                PRI_PVEM = sum(PRI_PVEM, na.rm = TRUE), PRD_PT_MC = sum(PRD_PT_MC, na.rm = TRUE),
                PRD_PT = sum(PRD_PT, na.rm = TRUE),PRD_MC = sum(PRD_MC, na.rm = TRUE),
                PT_MC = sum(PRD_PT_MC, na.rm = TRUE),NUM_VOTOS_NULOS = sum(NUM_VOTOS_NULOS, na.rm = TRUE),
                TOTAL_VOTOS = sum(TOTAL_VOTOS, na.rm = TRUE), LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = TRUE), 
                TOTAL_CASILLAS = sum(CASILLAS, na.rm = TRUE))
colnames(P2012Secc)
############################################
#####Mapa con datos a nivel municipal
############################################
muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2") #shape
states <- readOGR("map/mge2013v6_2.shp", "mge2013v6_2")
states_df <- fortify(states)
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
muns@data$id = as.numeric(muns@data$concat)
########meter acá la información de resultados electorales
muns@data <- plyr::join(muns@data, MunPres12, by = "id")
muns_df <- fortify(muns,region = "concat")
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


MAPA <- ggplot()+
      geom_map(data = muns_df, map = muns_df, 
               aes(map_id = id, x = long, y = lat, group = group, fill= TOTAL),
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
