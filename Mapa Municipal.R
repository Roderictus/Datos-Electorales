#Mapa Municipal
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

#####Catálogo de entidades y municipios 
#file:///C:/Users/IN334909/Downloads/Claves%20Entidades%20Federativas%20y%20Municipios%20PEF%202012.pdf
###

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu" , color = "#22211d"),
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


p<-ggplot() +
  geom_polygon(data = Muns_Map, aes(fill = POR_NULOS, 
                                    x = long,
                                    y = lat, 
                                    group = group)) +
  geom_path(data = Muns_Map, aes( x = long, 
                                  y = lat, 
                                  group = group)) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Votos nulos en la Elección Presidencial 2012")

p +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Porcentaje de Votos Nulos",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

#######################################################################################
########   Votos Nulos en el Estado de México Elección Presidencial 2012   ############
#######################################################################################
#Subset para el estado de méxico
p<-ggplot() +
  geom_polygon(data = Muns_Map, aes(fill = POR_NULOS, 
                                    x = long,
                                    y = lat, 
                                    group = group)) +
  geom_path(data = Muns_Map, aes( x = long, 
                                  y = lat, 
                                  group = group)) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Votos nulos en la Elección Presidencial 2012") +
    theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Porcentaje de Votos Nulos",
      guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))


#######################################################################################
########   Votos por EPN en el Estado de México Elección Presidencial 2012   ############
#######################################################################################
head(Mapa_Municipios)


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

############################################################################################################
##########################################  Voto Nulo en tres periodos  ####################################
############################################################################################################

Estado

#Lista de Estados

d
