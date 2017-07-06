#Mapa que utiliza datos de población, señala a nivel municipal disparidad de géneros
library(rgeos)
library(rgdal)
library(dplyr)
library(ggplot2)
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

head(Pais_Mapa)


Muns_Map <-fortify(muns , region = "ClaveINEGI")
ggplot() +
  geom_polygon()




Mapa_Municipios <- filter(.data = Muns_Map, CVE_ENT == "15")

ggplot() +
  geom_polygon(data = Mapa_Municipios, aes(fill = POR_PRI_ALIANZA, 
                                           x = long,
                                           y = lat, 
                                           group = group)) +
  geom_path(data = Mapa_Municipios, aes( x = long, 
                                         y = lat, 
                                         group = group)) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Votos Alianza PRI en Elección Presidencial 2012") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Porcentaje de Votos a favor de EPN en elección 2012",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    ))