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

# import data
grid <- st_read("data/grid.gpkg")
lim <- st_read("data/lim.gpkg")

# colors
eco_colors <- c(
  "Alto Paraná Atlantic forests"="#51723b", 
  "Araucaria moist forests"="#89a194", 
  "Atlantic Coast restingas"="#ca9cc6", 
  "Bahia coastal forests" = "#a14016", 
  "Bahia interior forests" = "#cc883a", 
  "Brazilian Atlantic dry forests"="#cfc89a",
  "Caatinga Enclaves moist forests"="#8e1418", 
  "Pernambuco coastal forests"="#45769e", 
  "Pernambuco interior forests"="#a3bde5",
  "Serra do Mar coastal forests"="#563d65",
  "Southern Atlantic Brazilian mangroves"="#ead862") 
eco_colors

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

# Extrai ecorregiões únicas
ecoregions <- sort(unique(grid$ECO_NAME))

# ui ----
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Ecological networks of the Atlantic Forest"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      selectizeInput(
        "ecoregion_select", 
        "Filter by ecoregion:", 
        choices = ecoregions,
        selected = ecoregions[1]
      ),
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
      downloadButton("download_network_image", "Download network image (.png)"),
      
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
  
  # Reactive value para armazenar grid filtrado
  filtered_grid <- reactiveVal(grid)
  
  # Atualiza o selectizeInput com as opções baseadas no grid filtrado
  observe({
    current_grid <- filtered_grid()
    updateSelectizeInput(
      session, 
      "cell_select", 
      choices = c("None" = NA, sort(current_grid$id)), 
      server = TRUE
    )
  })
  
  # Filtra grid baseado na ecorregião selecionada
  observeEvent(input$ecoregion_select, {
    filtered_grid(grid %>% filter(ECO_NAME == input$ecoregion_select))
  })
  
  # Reactive value to store clicked cell data
  clicked_cell_data <- reactiveVal(NULL)
  
  # Reactive value to store network plot
  network_plot_obj <- reactiveVal(NULL)
  
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
        fill = TRUE,
        fillColor = as.character(eco_colors),
        weight = 3,
        fillOpacity = 0.3,
        label = "Atlantic Forest",
        group = "lim"
      )
  })
  
  # Atualiza o mapa quando o grid filtrado muda
  observeEvent(filtered_grid(), {
    current_grid <- filtered_grid()
    
    # Obtém a cor correspondente para a ecorregião selecionada
    current_ecoregion <- input$ecoregion_select
    fill_color <- "black"
    
    leafletProxy("map") %>%
      clearGroup("grid") %>%
      addPolygons(
        data = current_grid,
        color = fill_color,
        weight = 1,
        fillColor = fill_color,
        fillOpacity = 0.6,
        label = ~paste("Cell ID:", id, "<br>Ecoregion:", ECO_NAME),
        group = "grid",
        layerId = ~as.character(id),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 3,
          bringToFront = TRUE
        )
      )
    
    # Se há células no grid filtrado, ajusta o zoom
    if (nrow(current_grid) > 0) {
      bbox <- current_grid %>% 
        st_geometry() %>% 
        st_transform(4326) %>% 
        st_bbox() %>% 
        as.numeric()
      
      leafletProxy("map") %>%
        fitBounds(lng1 = bbox[1], lat1 = bbox[2], 
                  lng2 = bbox[3], lat2 = bbox[4])
    }
  })
  
  # Observe cell clicks
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id) && click$group == "grid") {
      cell_id <- as.numeric(click$id)
      updateSelectizeInput(session, "cell_select", selected = cell_id)
      
      # Zoom to the clicked cell
      selected_cell <- filtered_grid() %>% filter(id == cell_id)
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
    current_grid <- filtered_grid()
    
    if (nrow(current_grid) > 0) {
      bbox <- current_grid %>% 
        st_geometry() %>% 
        st_transform(4326) %>% 
        st_bbox() %>% 
        as.numeric()
      
      leafletProxy("map") %>%
        clearGroup("selected_cell") %>%
        fitBounds(lng1 = bbox[1], lat1 = bbox[2], 
                  lng2 = bbox[3], lat2 = bbox[4])
    } else {
      leafletProxy("map") %>%
        clearGroup("selected_cell") %>%
        fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                  lng2 = lim_bbox[3], lat2 = lim_bbox[4])
    }
    
    updateSelectizeInput(session, "cell_select", selected = NA)
    clicked_cell_data(NULL)
    network_plot_obj(NULL)
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
    
    # vertex colors #CORRIGIR
    colors <- c(rep('#DEAA79',length(unique(local_edge[,1]))),
                rep('#659287',length(unique(local_edge[,2]))))
    
    # edge color gradient
    CRP <- colorRampPalette(c('white',"lightgray", "darkgray", "black"))
    # edge_col <- CRP(10)[base::cut(E(G)$weights, breaks = 10)]
    
    V(G)$label <- NA
    V(G)$label[most.conn] <- names(degree[most.conn])
    
    # Criar o plot e retornar o objeto
    plot(G, layout = L, vertex.color = colors, 
         vertex.size = 2+log(1+degree),
         vertex.label = NA,
         vertex.label.color = 'black',  
         vertex.label.cex = 1, 
         # edge.color = edge_col 
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
  
  # Download handler para exportar a imagem da rede como PNG
  output$download_network_image <- downloadHandler(
    filename = function() {
      cell_id <- input$cell_select
      paste0("network_plot_cell_", cell_id, ".png")
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
  
  # Reação à seleção direta de célula
  observeEvent(input$cell_select, {
    if (!is.na(input$cell_select)) {
      show_selected_cell(as.numeric(input$cell_select))
    } else {
      current_grid <- filtered_grid()
      
      if (nrow(current_grid) > 0) {
        bbox <- current_grid %>% 
          st_geometry() %>% 
          st_transform(4326) %>% 
          st_bbox() %>% 
          as.numeric()
        
        leafletProxy("map") %>%
          clearGroup("selected_cell") %>%
          fitBounds(lng1 = bbox[1], lat1 = bbox[2], 
                    lng2 = bbox[3], lat2 = bbox[4])
      } else {
        leafletProxy("map") %>%
          clearGroup("selected_cell") %>%
          fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                    lng2 = lim_bbox[3], lat2 = lim_bbox[4])
      }
      
      clicked_cell_data(NULL)
      network_plot_obj(NULL)
    }
  })
  
  # Reação ao botão "Find nearest cell"
  observeEvent(input$find_cell_btn, {
    req(input$lat_input, input$lon_input)
    
    current_grid <- filtered_grid()
    
    if (nrow(current_grid) > 0) {
      # Converte grid filtrado para coordenadas
      current_coords <- current_grid %>% 
        st_transform(4326) %>% 
        mutate(lon = st_coordinates(st_centroid(.))[,1],
               lat = st_coordinates(st_centroid(.))[,2]) %>% 
        st_drop_geometry()
      
      # Calcula distância de cada célula ao ponto digitado
      dist_to_point <- distm(
        current_coords[, c("lon", "lat")],
        matrix(c(input$lon_input, input$lat_input), ncol = 2)
      )
      
      # Identifica a célula mais próxima
      nearest_index <- which.min(dist_to_point)
      nearest_cell_id <- current_coords$id[nearest_index]
      
      # Atualiza o selectInput e chama o observeEvent correspondente
      updateSelectizeInput(session, "cell_select", selected = nearest_cell_id)
    } else {
      showNotification("No cells available in the current filter", type = "warning")
    }
  })
  
  # Função auxiliar para mostrar uma célula no mapa
  show_selected_cell <- function(cell_id) {
    current_grid <- filtered_grid()
    selected_cell <- current_grid %>% filter(id == cell_id)
    
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
    } else {
      showNotification("Selected cell not found in current filter", type = "warning")
    }
  }
}

shinyApp(ui, server)