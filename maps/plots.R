library(sf)
library(ggplot2)

# Ler shapefile das RMs (CEM)
cem_rm <- st_read("CEMbrRMEa20/CEMbrRMEa20.shp", options = "ENCODING=LATIN1")

# Mapa
ggplot() +
  geom_sf(data = cem_rm, aes(fill = NOME_AC), color = "black", size = 0.1) +
  theme_minimal() +
  theme(legend.position = "none") +  # tira legenda gigante
  labs(title = "Regiões Metropolitanas, Aglomerações Urbanas e RIDEs - Brasil (CEM 2020)")


library(leaflet)

pal <- colorFactor(palette = "Set3", domain = cem_rm$NOME_AC)

leaflet(cem_rm) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "black", weight = 0.5, smoothFactor = 0.2,
              fillOpacity = 0.6, fillColor = ~pal(NOME_AC),
              popup = ~paste("<b>", NOME_AC, "</b><br>UF:", UF_SIGLA, "<br>Tipo:", TIPO, "<br>Polo:", POLO)) %>%
  addLegend("bottomleft", pal = pal, values = cem_rm$NOME_AC,
            title = "RMs / AUs / RIDEs", opacity = 0.7)
