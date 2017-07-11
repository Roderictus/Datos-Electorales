#Mapa que utiliza datos de población, señala a nivel municipal disparidad de géneros
library(rgeos)
library(rgdal)
library(dplyr)
library(foreign)
library(ggplot2)
library(stringr)
library(viridis)
#library(raster)

Catalogo_Municipal <- read.dbf("./cat_municipio_OCT2016.dbf",as.is = TRUE) #mayor disparidad entre población femenina y masculina en el Estado
Catalogo_Municipal$PTOT<- as.numeric(Catalogo_Municipal$PTOT)
Catalogo_Municipal$PMAS<- as.numeric(Catalogo_Municipal$PMAS)
Catalogo_Municipal$PFEM<- as.numeric(Catalogo_Municipal$PFEM)
Catalogo_Municipal$VTOT<- as.numeric(Catalogo_Municipal$VTOT)
#Dos municipios nuevos que aparecen sin población, Bacalar y Puerto Morelos, los dos son municipios nuevos
#nas<-is.na(as.numeric(as.character(Catalogo_Municipal$PTOT)))
#Catalogo_Municipal[nas, ]
Catalogo_Municipal$CVE_INEGI <- str_c(str_pad(Catalogo_Municipal$CVE_ENT, width = 2, "left", "0"), 
                                      str_pad(Catalogo_Municipal$CVE_MUN, width = 3, "left", "0"))

#Info geográfica
Municipios<- readOGR("./Mapa muy reducido/areas_geoestadisticas_municipales.shp",
               "areas_geoestadisticas_municipales")
Municipios@data$CVE_INEGI <- str_c(Municipios@data$CVE_ENT, Municipios@data$CVE_MUN)
Municipios@data$id <- Municipios@data$CVE_INEGI
states <- readOGR("Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_estatales.shp", "areas_geoestadisticas_estatales")
States_Map <- fortify(states)
Pais_Mapa<-fortify(Municipios, region = "id")
Pais_Mapa$CVE_INEGI<-Pais_Mapa$id
Pais_Mapa<-left_join(x = Pais_Mapa, y = Catalogo_Municipal, by = "CVE_INEGI")
length(unique(Pais_Mapa$CVE_INEGI))
Pais_Mapa$MomioMujer <-  as.numeric(Pais_Mapa$PFEM)/as.numeric(Pais_Mapa$PMAS)
Mapa_Municipios <- filter(.data = Pais_Mapa, CVE_ENT == "15")
summary(Mapa_Municipios$MomioMujer)

####################################################################################
######################  Elementos específicos al mapa  #############################
####################################################################################
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
####################### Clases discretas ###################################

#https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
no_classes <- 6
labels <- c()
quantiles <-quantile(Mapa_Municipios$MomioMujer,
                     probs = seq(0,1,length.out = no_classes + 1 ))
labels <- c()
for(idx in 1:length(quantiles)) {
  labels<-c(labels, paste0(round(quantiles[idx],2),
                           "-",
                           round(quantiles[idx + 1],2)))
}
labels <- labels[1:length(labels)-1]
Mapa_Municipios$MomioMujer_cuantil <- cut(Mapa_Municipios$MomioMujer,
                                          breaks = quantiles, 
                                          labels = labels, 
                                          include.lowest = T)

ggplot() +
  geom_polygon(data = Mapa_Municipios, aes(fill = MomioMujer_cuantil, 
                                           x = long,
                                           y = lat, 
                                           group = group)) +
  geom_path(data = Mapa_Municipios, aes( x = long, 
                                         y = lat, 
                                         group = group),
            color = "white", size = 0.2) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Razón entre mujeres y hombres en municipios \n                      del Estado de México") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis",
    name = "Proporción Mujeres/ Hombres",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
  
  
  
  #scale_fill_viridis(
  #  option = "viridis", 
  #  direction = -1,
  #  name = "Proporción Mujeres/Hombres",
  #  guide = guide_colorbar(
  #    direction = "horizontal",
  #    barheight = unit(2, units = "mm"),
  #    barwidth = unit(50, units = "mm"),
  #    title.position = 'top',
  #    title.hjust = 0.5,
  #    label.hjust = 0.5
  #  ))
