library(shiny)
library(sf)
library(leaflet)
library(dplyr)

sf::sf_use_s2(FALSE)  # desliga s2

# Pastas
dir_setores <- "setores_censitarios"
cem_rm <- st_read("CEMbrRMEa20/CEMbrRMEa20.shp", options = "ENCODING=LATIN1", quiet = TRUE) |> 
  st_make_valid()

ui <- fluidPage(
  titlePanel("Mapa LEVE de Setores Censitários por Região Metropolitana"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rm_escolhida", "Escolha a RM:",
                  choices = sort(unique(cem_rm$NOME_AC)),
                  selected = "São Paulo"),
      helpText("Visualização leve: mostra apenas centroides dos setores.")
    ),
    mainPanel(
      leafletOutput("mapa", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  output$mapa <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron")
  })
  
  observeEvent(input$rm_escolhida, {
    rm_nome <- input$rm_escolhida
    rm_sf <- cem_rm %>% filter(NOME_AC == rm_nome) |> st_make_valid()
    uf <- rm_sf$UF_SIGLA[1]
    
    arq_uf <- file.path(dir_setores, paste0(uf, "_setores_CD2022.gpkg"))
    if (!file.exists(arq_uf)) {
      showNotification(paste("Arquivo não encontrado para UF:", uf), type = "error")
      return()
    }
    
    setores_uf <- st_read(arq_uf, quiet = TRUE) |> st_make_valid()
    if (st_crs(setores_uf) != st_crs(rm_sf)) {
      setores_uf <- st_transform(setores_uf, st_crs(rm_sf))
    }
    
    # Selecionar setores que intersectam a RM
    keep <- lengths(st_intersects(setores_uf, rm_sf)) > 0
    setores_rm <- setores_uf[keep, , drop = FALSE]
    
    # Criar centroides
    setores_pts <- st_centroid(setores_rm)
    
    # Popup
    setores_pts$popup <- paste0(
      "<b>Município:</b> ", setores_pts$CD_MUN,
      if ("CD_SETOR" %in% names(setores_pts)) paste0("<br><b>Setor:</b> ", setores_pts$CD_SETOR) else ""
    )
    
    leafletProxy("mapa") %>%
      clearShapes() %>%
      addPolygons(data = rm_sf, color = "red", weight = 2, fill = FALSE) %>%
      addCircleMarkers(data = setores_pts,
                       radius = 2, color = "blue", fillOpacity = 0.6,
                       popup = ~popup) %>%
      fitBounds(st_bbox(rm_sf)[["xmin"]], st_bbox(rm_sf)[["ymin"]],
                st_bbox(rm_sf)[["xmax"]], st_bbox(rm_sf)[["ymax"]])
  })
}

shinyApp(ui, server)

