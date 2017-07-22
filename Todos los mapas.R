############  paqueteria  #############
library(dplyr)
library(rgdal)
library(ggplot2)
library(viridis)
library(stringr)
library(data.table)
library(knitr)
library(kableExtra)
library(plotly)
########################### Datos Electorales ##############################
P2012Secc<-fread(input = "http://siceef.ine.mx/BD/Presidente2012Seccion.csv", #Leer resultados nacionales a nivel seccion
                 sep = ",", encoding = "Latin-1")
P2012Secc$CV_MUN <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),
                          str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))
P2012Secc$CVE_SECC<-str_c(str_pad(P2012Secc$ID_ESTADO, width =2, "left", "0"),
                          str_pad(P2012Secc$SECCION, width = 4, "left", "0"))
P2012SeccEdo<- filter(P2012Secc, ID_ESTADO == 15) #6390 #tomar resultados solamente para el EdoMex (15)
#cartografia electoral
Secciones <-readOGR("C:/Proyectos R/Datos-Electorales/Cartografía electoral/15/SECCION.shp", 
                    "SECCION") #6459
Secciones$CVE_SECC<-str_c(str_pad(Secciones$entidad, width =2, "left", "0"),
                          str_pad(Secciones$seccion, width = 4, "left", "0"))
Secciones_Map <- fortify(Secciones, 
                         region = "CVE_SECC")
Secciones_Map$CVE_SECC <- Secciones_Map$id
Secciones_Map<-left_join(x = Secciones_Map, y = P2012SeccEdo, by = "CVE_SECC")
Secciones_Map$Por_PRI <-  ((Secciones_Map$PRI + Secciones_Map$PRI_PVEM)/(Secciones_Map$TOTAL_VOTOS-Secciones_Map$NUM_VOTOS_NULOS))*100
Secciones_Map[is.na(Secciones_Map$Por_PRI),]$Por_PRI <- median(Secciones_Map$Por_PRI,na.rm = TRUE)
colnames(Secciones_Map)
Secciones_Map$Por_Participacion <- Secciones_Map$TOTAL_VOTOS/Secciones_Map$LISTA_NOMINAL

################  Temática del mapa #############################
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

#for (i in 1:length(unique(Secciones_Map$MUNICIPIO))-1) {}
for (i in 1:1) {
  Secciones_Map_Municipio <- filter(Secciones_Map, ID_MUNICIPIO == i)
  Nombre_Municipio<-names(table(Secciones_Map_Municipio$MUNICIPIO))
  Titulo = str_c(Nombre_Municipio," Edomex Elecciones Presidenciales 2012")
  
  ####################### Clases discretas ###################################
  
  #https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
  no_classes <- 6
  labels <- c()
  quantiles <-quantile(Secciones_Map_Municipio$Por_Participacion,
                       probs = seq(0,1,length.out = no_classes + 1 ))
  labels <- c()
  for(idx in 1:length(quantiles)) {
    labels<-c(labels, paste0(round(quantiles[idx],2),
                             "-",
                             round(quantiles[idx + 1],2)))
  }
  labels <- labels[1:length(labels)-1]
  Secciones_Map_Municipio$Por_Participacion <- cut(Secciones_Map_Municipio$Por_Participacion,
                                                   breaks = quantiles, 
                                                   labels = labels, 
                                                   include.lowest = T)
  ####################  Mapa  ##################################################
  
  Plot_Municipio <- ggplot() + geom_polygon(data = Secciones_Map_Municipio, aes(fill = Por_Participacion, 
                                                     x = long,
                                                     y = lat, 
                                                     group = group)) +
    geom_path(data = Secciones_Map_Municipio, aes( x = long, 
                                                   y = lat, 
                                                   group = group),
              color = "white", size = 0.2) +
    coord_equal() +
    theme_map() +
    labs( x = NULL, 
          y = NULL, NULL,
          title = Titulo) +
    theme(legend.position = "bottom") +
    scale_fill_viridis(
      option = "viridis",
      name = "Participación Votación Presidencial 2012",
      discrete = T,
      direction = -1,
      guide = guide_legend(
        keyheight = unit(5, units = "mm"),
        title.position = 'top',
        reverse = T
      ))
  
  Archivo <-str_c("C:/Proyectos R/Datos-Electorales/Mapas Nuevos/Secciones Municipales/15/Participacion Electoral/", Nombre_Municipio, ".png")
  class(Plot_Municipio)
  
  ggsave(filename = Archivo,  device = "jpg" ,plot = last_plot())
}

