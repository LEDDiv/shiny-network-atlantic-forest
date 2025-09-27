# packages
options(
  HTTPUserAgent =
    sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    )
)

if (!require(dplyr)) install.packages("dplyr")
if (!require(magrittr)) install.packages("magrittr")
if (!require(readr)) install.packages("readr")
if (!require(shiny)) install.packages("shiny")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(leaflet)) install.packages("leaflet")
if (!require(sf)) install.packages("sf")
if (!require(arrow)) install.packages("arrow", repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")
if (!require(geosphere)) install.packages("geosphere")
if (!require(scales)) install.packages("scales")
if (!require(igraph)) install.packages("igraph")
if (!require(network)) install.packages("network")
if (!require(GGally)) install.packages("GGally")

# import data
grid <- sf::st_read("data/grid.gpkg")
lim <- sf::st_read("data/lim.gpkg")

# Calcula bbox do lim para usar no zoom
lim_bbox <- lim %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  as.numeric()

# Converte grid para data.frame com coordenadas (centroide)
grid_coords <- grid %>% 
  st_transform(4326) %>% 
  mutate(lon = st_coordinates(st_centroid(grid))[,1],
         lat = st_coordinates(st_centroid(grid))[,2]) %>% 
  st_drop_geometry()

# ui ----
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Ecological networks of the Atlantic Forest"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      selectizeInput(
        "cell_select", 
        "Find cell by ID:", 
        choices = NULL,
        options = list(
          placeholder = 'Select a cell or type to search',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      actionButton("reset_zoom", "Reset Zoom", icon = icon("search-minus")),
      
      tags$hr(),
      h5("Find cell by coordinates"),
      numericInput("lat_input", "Latitude:", value = -25, step = 0.01),
      numericInput("lon_input", "Longitude:", value = -44, step = 0.01),
      actionButton("find_cell_btn", "Find nearest cell"),
      
      tags$hr(),
      downloadButton("download_network", "Download network data (.csv)"),
    
      tags$hr(),
      h4("Network visualization"),
      plotOutput("network_plot", height = "300px")),

    mainPanel(
      width = 9,
      leafletOutput("map", height = "85vh")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  # Atualiza o selectizeInput com as opções
  observe({
    updateSelectizeInput(
      session, 
      "cell_select", 
      choices = c("None" = NA, sort(grid$id)), 
      server = TRUE
    )
  })
  
  # Reactive value to store clicked cell data
  clicked_cell_data <- reactiveVal(NULL)
  
  # Mapa inicial
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("OpenStreetMap", group = "Street") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
      addLayersControl(
        baseGroups = c("Satellite", "Light", "Street", "Dark", "Terrain"),
        options = layersControlOptions(collapsed = FALSE, position = "topright")
      ) %>%
      addPolygons(
        data = lim,
        color = "black",
        weight = 2,
        fillOpacity = 0.5,
        label = "",
        group = "lim"
      ) %>%
      addPolygons(
        data = grid,
        color = "steelblue",
        weight = 1,
        fillOpacity = 0.2,
        label = ~paste("Cell ID:", id),
        group = "grid",
        layerId = ~as.character(id),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                lng2 = lim_bbox[3], lat2 = lim_bbox[4])
  })
  
  # Observe cell clicks
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id) && click$group == "grid") {
      cell_id <- as.numeric(click$id)
      updateSelectizeInput(session, "cell_select", selected = cell_id)
      
      # Zoom to the clicked cell
      selected_cell <- grid %>% filter(id == cell_id)
      bbox <- selected_cell %>% 
        st_geometry() %>% 
        st_transform(4326) %>% 
        st_bbox() %>% 
        as.numeric()
      
      leafletProxy("map") %>%
        clearGroup("selected_cell") %>%
        addPolygons(
          data = selected_cell,
          group = "selected_cell",
          color = "darkgreen",
          weight = 2,
          fillOpacity = 0.5,
          label = ~paste("Célula:", id)
        ) %>%
        flyToBounds(
          lng1 = bbox[1],
          lat1 = bbox[2],
          lng2 = bbox[3],
          lat2 = bbox[4]
        )
      
      # Load and process network data
      file_path <- file.path("https://leddiv.github.io/ms-atlantic-forest-networks-edge-lists/edge_list/", 
                             paste0("edgelist_", cell_id, "_compressed.parquet"))
      tryCatch({
        local_edge <- arrow::read_parquet(file_path)
        print(local_edge)
        clicked_cell_data(local_edge)
      }, error = function(e) {
        clicked_cell_data(NULL)
        showNotification("Error loading cell data", type = "error")
      })
    }
  })
  
  # Função para resetar o zoom
  observeEvent(input$reset_zoom, {
    leafletProxy("map") %>%
      clearGroup("selected_cell") %>%
      fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                lng2 = lim_bbox[3], lat2 = lim_bbox[4])
    updateSelectizeInput(session, "cell_select", selected = NA)
    clicked_cell_data(NULL)
  })
  
  output$network_plot <- renderPlot({
    local_edge <- clicked_cell_data()
    validate(need(!is.null(local_edge), "Select a cell to view its network"))
    
    # network
    G <- graph_from_edgelist(as.matrix(local_edge[,1:2]), directed = FALSE)
    E(G)$weights <- local_edge[,3]
    L <- layout.fruchterman.reingold(G)
    
    # species degree
    degree <- c(tapply(local_edge$int_freq, INDEX = local_edge$frug_sp, sum),
                tapply(local_edge$int_freq, INDEX = local_edge$plant_sp, sum))
    
    most.conn <- order(degree, decreasing = T)[1:10]
    
    # vertex colors #CORRIGIR
    colors <- c(rep('#DEAA79',length(unique(local_edge[,1]))),
                rep('#659287',length(unique(local_edge[,2]))))
    
    # edge color gradient
    CRP <- colorRampPalette(c('white',"lightgray", "darkgray", "black"))
    # edge_col <- CRP(10)[base::cut(E(G)$weights, breaks = 10)]
    
    V(G)$label <- NA
    V(G)$label[most.conn] <- names(degree[most.conn])
    
    
    plot(G, layout = L, vertex.color = colors, vertex.size = 2+log(1+degree),
         vertex.label = NA,
         vertex.label.color = 'black',  vertex.label.cex = 1, 
         # edge.color = edge_col 
    )
    
  })
  
  
  # Download handler para exportar os dados da rede como CSV
  output$download_network <- downloadHandler(
    filename = function() {
      cell_id <- input$cell_select
      paste0("network_cell_", cell_id, ".csv")
    },
    content = function(file) {
      local_edge <- clicked_cell_data()
      if (!is.null(local_edge)) {
        readr::write_csv(local_edge, file)
      } else {
        writeLines("No network data available for this cell.", con = file)
      }
    }
  )
  
  # Reação à seleção direta de célula
  observeEvent(input$cell_select, {
    if (!is.na(input$cell_select)) {
      show_selected_cell(as.numeric(input$cell_select))
    } else {
      leafletProxy("map") %>%
        clearGroup("selected_cell") %>%
        fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                  lng2 = lim_bbox[3], lat2 = lim_bbox[4])
      clicked_cell_data(NULL)
    }
  })
  
  # Reação ao botão "Find nearest cell"
  observeEvent(input$find_cell_btn, {
    req(input$lat_input, input$lon_input)
    
    # Calcula distância de cada célula ao ponto digitado
    dist_to_point <- distm(
      grid_coords[, c("lon", "lat")],
      matrix(c(input$lon_input, input$lat_input), ncol = 2)
    )
    
    # Identifica a célula mais próxima
    nearest_index <- which.min(dist_to_point)
    nearest_cell_id <- grid_coords$id[nearest_index]
    
    # Atualiza o selectInput e chama o observeEvent correspondente
    updateSelectizeInput(session, "cell_select", selected = nearest_cell_id)
  })
  
  # Função auxiliar para mostrar uma célula no mapa
  show_selected_cell <- function(cell_id) {
    selected_cell <- grid %>% filter(id == cell_id)
    
    bbox <- selected_cell %>% 
      st_geometry() %>% 
      st_transform(4326) %>% 
      st_bbox() %>% 
      as.numeric()
    
    leafletProxy("map") %>%
      clearGroup("selected_cell") %>%
      addPolygons(
        data = selected_cell,
        group = "selected_cell",
        color = "darkgreen",
        weight = 2,
        fillOpacity = 0.5,
        label = ~paste("Célula:", id)
      ) %>%
      flyToBounds(
        lng1 = bbox[1],
        lat1 = bbox[2],
        lng2 = bbox[3],
        lat2 = bbox[4]
      )
    
    # Importa parquet associado, se existir
    file_path <- file.path("https://leddiv.github.io/ms-atlantic-forest-networks-edge-lists/edge_list/", 
                           paste0("edgelist_", cell_id, "_compressed.parquet"))
    tryCatch({
      local_edge <- arrow::read_parquet(file_path)
      clicked_cell_data(local_edge)
    }, error = function(e) {
      clicked_cell_data(NULL)
      showNotification("Error loading cell data", type = "error")
    })
  }
}


shinyApp(ui, server)