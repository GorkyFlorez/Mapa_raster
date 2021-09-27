library(raster)
library(sf)
library(ggplot2)
library( colorspace)
Año3 = raster("raster/temporada/Año 2003.tif")
Año4 = raster("raster/temporada/Año 2004.tif")
Año5 = raster("raster/temporada/Año 2005.tif")
Año6 = raster("raster/temporada/Año 2006.tif")
Año7 = raster("raster/temporada/Año 2007.tif")
Año8 = raster("raster/temporada/Año 2008.tif")
Año9 = raster("raster/temporada/Año 2009.tif")
Año10= raster("raster/temporada/Año 2010.tif")
Año11= raster("raster/temporada/Año 2011.tif")
Año12= raster("raster/temporada/Año 2012.tif")
Año13= raster("raster/temporada/Año 2013.tif")
Año14= raster("raster/temporada/Año 2014.tif")
lib  <- st_read ("raster/temporada/lower_Indus_basin.shp")
landsat8_Natu = stack(Año3,Año4,Año5,Año6,Año7,Año8,Año9,Año10,Año11,Año12,Año13,Año14)
lbla         <- data.frame(month_abb = month.abb, año=2003:2014)
# Elaboramos los meses Para precipitacion-----------------------------------------
vls         <- rasterToPoints(landsat8_Natu) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(año = parse_number(var))


map=ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ var) +
  scale_fill_binned_sequential(palette = "Inferno",
                       na.value = 'white') +
  geom_sf(data = lib , fill = NA, color = 'black', size = 1)+
  theme_bw() +
  labs(title = '', fill = 'medida',  x = 'Longitud', y = 'Latitud', caption = "Gorky Florez") +
  theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(66, 71, 76)) 

ggsave(plot = map,"MAPAS/lib.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico


  