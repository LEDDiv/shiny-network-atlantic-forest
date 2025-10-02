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
grid <- st_read("data/grid.gpkg") %>% 
  slice(1:100)
grid_large <- st_read("data/grid_large.gpkg")
# grid_large <- st_make_grid(lim, .25)[grid,] %>%
#   sf::st_as_sf() %>%
#   mutate(grid_large_id = 1:nrow(.))
# st_write(grid_large, "data/grid_large.gpkg", delete_dsn = TRUE)

# Calcula bbox do lim para usar no zoom
lim_bbox <- lim %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  as.numeric()

# Converte grid para data.frame com coordenadas (centroide) - TODAS as células
grid_coords <- grid %>% 
  st_transform(4326) %>% 
  mutate(lon = st_coordinates(st_centroid(grid))[,1],
         lat = st_coordinates(st_centroid(grid))[,2]) %>% 
  st_drop_geometry()

# Identificar qual grid de 100km cada célula pertence
grid_with_large <- grid %>%
  st_join(grid_large, join = st_intersects)

# ui ----
ui <- fluidPage(
  
  useShinyjs(),  # Adiciona shinyjs
  
  theme = shinytheme("flatly"),
  
  titlePanel("Ecological networks of the Atlantic Forest"),
  
  sidebarLayout(
    position = "right",
    
    sidebarPanel(
      
      width = 5,
      heigth = 5,
      
      selectizeInput(
        "grid_large_select", 
        "Filter by large grid:", 
        choices = sort(unique(grid_with_large$grid_large_id)),
        selected = NULL,
        options = list(
          placeholder = 'Select a large grid or type to search',
          onInitialize = I('function() { this.setValue(""); }')
        )),
      
      selectizeInput(
        "cell_select", 
        "Find cell by ID:", 
        choices = NULL,
        options = list(
          placeholder = 'Select a cell or type to search',
          onInitialize = I('function() { this.setValue(""); }')
        )),
      
      actionButton("reset_zoom", "Reset Zoom", icon = icon("search-minus")),
      
      tags$hr(),
      h5("Find cell by coordinates (it takes a while)"),
      numericInput("lat_input", "Latitude:", value = -25, step = 0.01),
      numericInput("lon_input", "Longitude:", value = -44, step = 0.01),
      actionButton("find_cell_btn", "Find nearest cell"),
      
      tags$hr(),
      downloadButton("download_network", "Download network data (.csv)"),
      downloadButton("download_network_image", "Download network image (.png)"),
      
      tags$hr(),
      h4("Network visualization"),
      plotOutput("network_plot", height = "500px", width = "700px")),
    
    mainPanel(
      width = 7,
      leafletOutput("map", height = "85vh")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  # Reactive value para armazenar grid filtrado (inicialmente vazio)
  filtered_grid <- reactiveVal(NULL)
  
  # Reactive value para armazenar o grid 100km selecionado
  selected_grid_large <- reactiveVal(NULL)
  
  # Reactive value to store clicked cell data
  clicked_cell_data <- reactiveVal(NULL)
  
  # Reactive value to store network plot
  network_plot_obj <- reactiveVal(NULL)
  
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
      
      addPolygons(
        data = grid_large,
        color = "red",
        weight = 2,
        fillColor = "transparent",
        fillOpacity = 0,
        label = ~paste("100km Grid ID:", grid_large_id),
        group = "grid_large",
        layerId = ~paste0("grid_large_", grid_large_id),
        highlightOptions = highlightOptions(
          color = "yellow",
          weight = 3,
          bringToFront = TRUE
        )
      ) %>%
      
      fitBounds(lng1 = lim_bbox[1], lat1 = lim_bbox[2], 
                lng2 = lim_bbox[3], lat2 = lim_bbox[4])
  })
  
  # Função para atualizar o grid baseado na seleção do grid_large
  update_grid_selection <- function(selected_grid_id) {
    req(selected_grid_id)
    
    # Armazena o grid 100km selecionado
    selected_grid_large(selected_grid_id)
    
    # Filtrar células que pertencem ao grid de 100km selecionado
    filtered_cells <- grid_with_large %>% 
      filter(grid_large_id == selected_grid_id)
    
    if (nrow(filtered_cells) > 0) {
      filtered_grid(filtered_cells)
      
      # Atualizar mapa - mostrar apenas células do grid selecionado
      leafletProxy("map") %>%
        clearGroup("grid") %>%
        clearGroup("selected_grid_large") %>%
        addPolygons(
          data = filtered_cells,
          color = "gray",
          weight = 1,
          fillColor = "gray",
          fillOpacity = 0.6,
          label = ~paste("Cell ID:", id, "<br>100km Grid:", grid_large_id),
          group = "grid",
          layerId = ~paste0("cell_", id),  # IMPORTANTE: prefixo para identificar células
          highlightOptions = highlightOptions(
            color = "white",
            weight = 3,
            bringToFront = TRUE
          )
        ) %>%
        
        # Destacar o grid 100km selecionado
        addPolygons(
          data = grid_large %>% filter(grid_large_id == selected_grid_id),
          color = "yellow",
          weight = 3,
          fillColor = "transparent",
          fillOpacity = 0,
          group = "selected_grid_large",
          label = ~paste("Selected 100km Grid:", grid_large_id)
        )
      
      # Zoom para as células filtradas
      bbox_filtered <- filtered_cells %>% 
        st_union() %>% 
        st_transform(4326) %>% 
        st_bbox() %>% 
        as.numeric()
      
      leafletProxy("map") %>%
        fitBounds(
          lng1 = bbox_filtered[1],
          lat1 = bbox_filtered[2],
          lng2 = bbox_filtered[3],
          lat2 = bbox_filtered[4]
        )
      
      # Atualizar choices do selectize de células
      updateSelectizeInput(
        session, 
        "cell_select", 
        choices = c("None" = "", sort(filtered_cells$id)),
        selected = ""
      )
      
      # Limpar dados de célula clicada
      clicked_cell_data(NULL)
      network_plot_obj(NULL)
      
    } else {
      showNotification("No cells found in selected 100km grid", type = "warning")
    }
  }
  
  # Observar seleção do grid de 100km no dropdown
  observeEvent(input$grid_large_select, {
    req(input$grid_large_select)
    if (input$grid_large_select != "") {
      selected_grid_id <- as.numeric(input$grid_large_select)
      update_grid_selection(selected_grid_id)
    }
  })
  
  # Observe clicks no mapa - VERSÃO CORRIGIDA
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click

    if (!is.null(click$id)) {
      # Se clicou em um grid de 100km
      if (grepl("grid_large_", click$id)) {
        grid_large_id <- as.numeric(gsub("grid_large_", "", click$id))

        # Atualizar o select input para o grid clicado
        updateSelectizeInput(session, "grid_large_select", selected = grid_large_id)
      }
      
      # Se clicou em uma célula do grid (células menores)
      if (grepl("cell_", click$id)) {
        cell_id <- as.numeric(gsub("cell_", "", click$id))

        updateSelectizeInput(session, "cell_select", selected = cell_id)
        
        # Processar imediatamente a célula clicada
        show_selected_cell(cell_id)
      }
    }
  })
  
  # Reação à seleção direta de célula no dropdown
  observeEvent(input$cell_select, {
    req(input$cell_select)
    
    if (input$cell_select != "") {
      show_selected_cell(as.numeric(input$cell_select))
    } else {
      leafletProxy("map") %>%
        clearGroup("selected_cell")
      
      clicked_cell_data(NULL)
      network_plot_obj(NULL)
    }
  })
  
  # Função auxiliar para mostrar uma célula no mapa
  show_selected_cell <- function(cell_id) {
    tryCatch({
      # Encontra a célula no grid filtrado atual
      current_grid <- filtered_grid()
      if (is.null(current_grid)) {
        showNotification("Please select a 100km grid first", type = "warning")
        return()
      }
      
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
        file_path <- file.path("https://leddiv.github.io/ms-atlantic-forest-networks-edge-lists/edge_list/", 
                               paste0("edgelist_", cell_id, "_compressed.parquet"))
        tryCatch({
          local_edge <- arrow::read_parquet(file_path)
          clicked_cell_data(local_edge)
          showNotification(paste("Network data loaded for cell", cell_id), type = "message")
        }, error = function(e) {
          clicked_cell_data(NULL)
          showNotification(paste("No network data available for cell", cell_id), type = "warning")
        })
      } else {
        showNotification("Selected cell not found in current grid", type = "warning")
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
      clearGroup("selected_grid_large") %>%
      clearGroup("grid") %>%
      clearGroup("selected_cell")
    
    selected_grid_large(NULL)
    filtered_grid(NULL)
    updateSelectizeInput(session, "grid_large_select", selected = "")
    updateSelectizeInput(session, "cell_select", selected = "")
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
    if (input$lat_input < -90 || input$lat_input > 90 || input$lon_input < -180 || input$lon_input > 180) {
      showNotification("Please enter valid coordinates (Lat: -90 to 90, Lon: -180 to 180)", type = "warning")
      return()
    }
    
    tryCatch({
      # Calcula distância de TODAS as células ao ponto digitado
      dist_to_point <- geosphere::distm(
        grid_coords[, c("lon", "lat")],
        matrix(c(input$lon_input, input$lat_input), ncol = 2)
      )
      
      # Identifica a célula mais próxima entre TODAS as células
      nearest_index <- which.min(dist_to_point)
      nearest_cell_id <- grid_coords$id[nearest_index]
      
      # Encontra qual grid 100km a célula pertence
      cell_grid_large <- grid_with_large %>% 
        st_drop_geometry() %>%
        filter(id == nearest_cell_id) %>% 
        pull(grid_large_id) %>% 
        first()
      
      # Se encontrou um grid 100km válido
      if (!is.na(cell_grid_large)) {
        # Atualiza o grid 100km primeiro
        updateSelectizeInput(session, "grid_large_select", selected = cell_grid_large)
        
        # Depois seleciona a célula (será processado após o grid ser atualizado)
        shinyjs::delay(500, {
          updateSelectizeInput(session, "cell_select", selected = nearest_cell_id)
        })
      } else {
        showNotification("Could not find corresponding 100km grid for the nearest cell", type = "warning")
      }
      
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
}

shinyApp(ui, server)