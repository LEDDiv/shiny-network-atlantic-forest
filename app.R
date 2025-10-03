# packages
options(
  HTTPUserAgent =
    sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    )
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require(shiny)) install.packages("shiny")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(magrittr)) install.packages("magrittr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(sf)) install.packages("sf")
if (!require(arrow)) install.packages("arrow", repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")
if (!require(geosphere)) install.packages("geosphere")
if (!require(scales)) install.packages("scales")
if (!require(igraph)) install.packages("igraph")
if (!require(network)) install.packages("network")
if (!require(leaflet)) install.packages("leaflet")
if (!require(shinyjs)) install.packages("shinyjs")

# import data
lim <- st_read("data/lim.gpkg")
grid_5km <- st_read("data/grid.gpkg")

# Calcula bbox do lim para usar no zoom
lim_bbox <- lim %>% 
  st_buffer(-.5) %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  as.numeric()

# ui ----
ui <- fluidPage(
  
  useShinyjs(),  # Adiciona shinyjs
  
  theme = shinytheme("flatly"),
  
  titlePanel("Ecological networks of the Atlantic Forest"),
  
  sidebarLayout(
    position = "right",
    
    sidebarPanel(
      
      width = 4,

      h4("Find network by coordinates (it takes a while)"),
      numericInput("lon_input", "Longitude:", value = -42, step = 0.01),
      numericInput("lat_input", "Latitude:", value = -3, step = 0.01),
      
      actionButton("find_cell_btn", "Find nearest cell"),
      actionButton("reset_zoom", "Reset Zoom", icon = icon("search-minus")),
      
      br(),
      
      h4("Network visualization"),
      plotOutput("network_plot", width = "570px", height = "340px"),
      
      br(),
      
      downloadButton("download_network", "Download network data (.csv)"),
      downloadButton("download_network_image", "Download network image (.png)"),
      
      h6(HTML('Developed by: <a href="https://www.mathiasmpires.net.br/index.html" target="_blank" rel="noopener">Laboratory for studies on the structure and dynamics of diversity (LDDiv)</a>.'))
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = "90vh")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  # Reactive value to store network plot
  network_plot_obj <- reactiveVal(NULL)
  
  # Reactive value para armazenar dados da célula clicada
  clicked_cell_data <- reactiveVal(NULL)
  
  # Mapa inicial - mostra todos os grids de 100km e limites
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
        fill = TRUE,
        fillColor = "black",
        weight = 3,
        fillOpacity = 0.3,
        label = "Atlantic Forest",
        group = "lim"
      ) %>%
      fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                lng2 = lim_bbox[3], lat2 = lim_bbox[4])
  })
  
  # Observe clicks no mapa - apenas para células
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    
    if (!is.null(click$id) && grepl("cell_", click$id)) {
      cell_id <- as.numeric(gsub("cell_", "", click$id))
      show_selected_cell(cell_id)
    }
  })
  
  # Função auxiliar para mostrar uma célula no mapa
  show_selected_cell <- function(cell_id) {
    tryCatch({
      # Encontra a célula no grid completo
      selected_cell <- grid_5km %>% filter(id == cell_id)
      
      if (nrow(selected_cell) > 0) {
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
            weight = 3,
            fillColor = "green",
            fillOpacity = 0.3,
            label = ~paste("Selected Cell:", id)
          ) %>%
          flyToBounds(
            lng1 = bbox[1],
            lat1 = bbox[2],
            lng2 = bbox[3],
            lat2 = bbox[4]
          )
        
        # Importa parquet associado, se existir
        file_path <- file.path("https://leddiv.github.io/ms-atlantic-forest-networks-edge-lists/edge_list_filtered/", 
                               paste0("edgelist_filter", cell_id, ".parquet"))
        tryCatch({
          local_edge <- arrow::read_parquet(file_path)
          clicked_cell_data(local_edge)
          showNotification(paste("Network data loaded for cell", cell_id), type = "message")
        }, error = function(e) {
          clicked_cell_data(NULL)
          showNotification(paste("No network data available for cell", cell_id), type = "warning")
        })
      } else {
        showNotification("Selected cell not found", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error showing cell:", e$message), type = "error")
    })
  }
  
  # reset zoom ----
  observeEvent(input$reset_zoom, {
    leafletProxy("map") %>%
      fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2],
                lng2 = lim_bbox[3], lat2 = lim_bbox[4]) %>%
      clearGroup("selected_cell")
    
    clicked_cell_data(NULL)
    network_plot_obj(NULL)
  })
  
  # Reação ao botão "Find nearest cell" - BUSCA EM TODAS AS CÉLULAS
  observeEvent(input$find_cell_btn, {
    req(input$lat_input, input$lon_input)
    
    # Validação das coordenadas
    if (is.na(input$lat_input) || is.na(input$lon_input)) {
      showNotification("Please enter valid coordinates", type = "warning")
      return()
    }
    
    # Verifica se as coordenadas estão dentro de limites razoáveis
    if (input$lon_input < -57 || input$lon_input > -35 || input$lat_input < -35 || input$lat_input > -3) {
      showNotification("Please enter valid coordinates (Lon: -57 to -35 | Lat: -35 to -3)", type = "warning")
      return()
    }
    
    tryCatch({
      
      # Converte grid para data.frame com coordenadas (centroide)
      input_buffer <- sf::st_as_sf(tibble::tibble(
        lon = input$lon_input,
        lat = input$lat_input),
        coords = c("lon", "lat"),
        crs = 4326) %>%
        terra::vect() %>% 
        terra::buffer(100000) %>% 
        sf::st_as_sf()

      grid_buffer <- grid_5km[input_buffer, ]

      grid_coords <- grid_buffer %>%
        st_transform(4326) %>%
        mutate(lon = st_coordinates(st_centroid(grid_buffer))[,1],
               lat = st_coordinates(st_centroid(grid_buffer))[,2]) %>%
        st_drop_geometry()
      
      # Calcula distância de TODAS as células ao ponto digitado
      dist_to_point <- geosphere::distm(
        grid_coords[, c("lon", "lat")],
        matrix(c(input$lon_input, input$lat_input), ncol = 2)
      )
      
      # Identifica a célula mais próxima entre TODAS as células
      nearest_index <- which.min(dist_to_point)
      nearest_cell_id <- grid_coords$id[nearest_index]
      
      # Seleciona a célula mais próxima
      show_selected_cell(nearest_cell_id)
      
    }, error = function(e) {
      showNotification(paste("Error finding cell:", e$message), type = "error")
    })
  })
  
  # Função para criar o plot da rede
  create_network_plot <- function(local_edge) {
    
    # network
    G <- graph_from_edgelist(as.matrix(local_edge[,1:2]), directed = FALSE)
    E(G)$weights <- local_edge[,3]
    L <- layout.fruchterman.reingold(G)
    
    # species degree
    degree <- c(tapply(local_edge$int_freq, INDEX = local_edge$frug_sp, sum),
                tapply(local_edge$int_freq, INDEX = local_edge$plant_sp, sum))
    
    most.conn <- order(degree, decreasing = T)[1:10]
    
    # vertex colors
    colors <- c(rep('#DEAA79',length(unique(local_edge[,1]))),
                rep('#659287',length(unique(local_edge[,2]))))
    
    # edge color gradient
    CRP <- colorRampPalette(c('white',"lightgray", "darkgray", "black"))
    
    V(G)$label <- NA
    V(G)$label[most.conn] <- names(degree[most.conn])
    
    # Criar o plot e retornar o objeto
    plot(G, layout = L, vertex.color = colors, 
         vertex.size = 2+log(1+degree),
         vertex.label = NA,
         vertex.label.color = 'black',  
         vertex.label.cex = 1
    )
  }
  
  output$network_plot <- renderPlot({
    local_edge <- clicked_cell_data()
    validate(need(!is.null(local_edge), "Select a cell to view its network"))
    
    # Criar e armazenar o plot
    p <- create_network_plot(local_edge)
    network_plot_obj(p)
  })
  
  # Download handler para exportar os dados da rede como CSV
  output$download_network <- downloadHandler(
    filename = function() {
      cell_id <- if (!is.null(clicked_cell_data())) {
        # Extrai o ID da célula dos dados (assumindo que está em algum lugar dos dados)
        # Você pode precisar ajustar isso dependendo da estrutura dos seus dados
        "network_data.csv"
      } else {
        "network_data.csv"
      }
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
  
  # Download handler para exportar a imagem da rede como PNG
  output$download_network_image <- downloadHandler(
    filename = function() {
      "network_plot.png"
    },
    content = function(file) {
      local_edge <- clicked_cell_data()
      if (!is.null(local_edge)) {
        # Configurar dispositivo PNG
        png(file, width = 20, height = 20, units = "cm", res = 300)
        # Criar o plot
        create_network_plot(local_edge)
        # Fechar dispositivo
        dev.off()
      } else {
        # Criar um PNG vazio se não houver dados
        png(file, width = 20, height = 20, units = "cm", res = 300)
        plot(1, type = "n", xlab = "", ylab = "", main = "No network data available")
        dev.off()
      }
    }
  )
}

shinyApp(ui, server)