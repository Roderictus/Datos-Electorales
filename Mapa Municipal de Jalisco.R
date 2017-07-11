install.packages("viridis")

#Mapa que utiliza datos de población, señala a nivel municipal disparidad de géneros
library(rgeos)
library(rgdal)
library(dplyr)
library(foreign)
library(ggplot2)
library(stringr)
library(viridis)

#library(raster)



Catalogo_Municipal <- read.dbf("./cat_municipio_OCT2016.dbf") #mayor disparidad entre población femenina y masculina en el Estado
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



ggplot() +
  geom_polygon(data = Mapa_Municipios, aes(fill = MomioMujer, 
                                           x = long,
                                           y = lat, 
                                           group = group)) +
  geom_path(data = Mapa_Municipios, aes( x = long, 
                                         y = lat, 
                                         group = group)) +
  coord_equal() +
  #theme_bare() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Razón entre hombres y mujeres en municipios del Estado de México") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Proporción Mujeres/Hombres",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    ))
