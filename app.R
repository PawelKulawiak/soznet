library(shiny)
library(tidyverse)
library(igraph)
library(readxl)
library(ggraph)

### ui

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #sidebar {
        position: fixed; /* Fix sidebar to the left */
        top: 0; /* Start at the top */
        left: 0; /* Align to the left side */
        width: 300px; /* Set the sidebar width */
        height: 100vh; /* Full viewport height */
        overflow-y: auto; /* Add scroll to sidebar if needed */
        background-color: #f8f9fa; /* Light grey background */
        padding: 20px; /* Padding inside the sidebar */
        z-index: 1000; /* Ensure sidebar is above other content */
      }
      .container-fluid .row .col-sm-4 {
        padding: 0; /* Remove padding around the sidebar to fit fixed width */
      }
      #main-content {
        margin-left: 320px; /* Offset main content to the right of the sidebar */
        padding: 20px; /* Padding inside the main content area */
      }
      .container-fluid {
        overflow-x: hidden; /* Prevent horizontal overflow caused by fixed sidebar */
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(id="sidebar",
                 fileInput(
                   "file1",
                   HTML("Socio-Matrix hochladen<br>Excel-Datei"),
                   buttonLabel = HTML("Datei suchen..."),
                   placeholder = "",
                   accept = c(".xlsx")
                 ),
                 helpText("Socio-Matrix mit Nominierungen bitte hier hochladen"),
                 uiOutput("selectNodeUI"),
                 # Dynamic UI for selecting a node
                 a(
                   href = "Klasse B.xlsx",
                   HTML("Download<br>Socio-Matrix-Beispiel als Excel-Datei"),
                   download = NA,
                   target = "_blank"
                 ),
                 hr(),
                 p("Entwickelt von"),
                 a(href = "https://pawelkulawiak.github.io/site/",
                   HTML("Pawel R. Kulawiak<br>Universität Potsdam"))
    ),
    mainPanel(id="main-content",
              h3(HTML("SozNet: Soziale Netzwerkanalyse & Netzwerkvisualisierung")),
              h5(HTML("Beta-Version")),
              hr(),
              plotOutput("networkDirect", height = "500px"),
              hr(),
              plotOutput("networkCliqueDirect", height = "500px"),
              hr(),
              plotOutput("networkMutual", height = "500px"),
              hr(),
              plotOutput("networkCliqueMutual", height = "500px"),
              hr(),
              tableOutput("degree"),
              width = 6
    )
  )
)



### server

server <- function(input, output) {
  graphObj <- reactiveVal()
  dataObj <- reactiveVal()

  # Load the preselected sociomatrix and create the initial graph
  initialFriendships <- read_excel("www/Klasse B.xlsx", col_names = TRUE)
  
  initialVar <- read_excel("www/Klasse B.xlsx", col_names = TRUE, sheet = 2, skip = 1)
  initialVar[1] <- NULL
  
  initialAdjacencyMatrix <- initialFriendships |> select(-1) |> as.matrix()
  initialGraph <- graph_from_adjacency_matrix(initialAdjacencyMatrix, mode = "directed", diag = FALSE)
  
  V(initialGraph)$var1 <- initialVar[1] |> as_vector()
  V(initialGraph)$var2 <- initialVar[2] |> as_vector()
  
  graphObj(initialGraph)  # Set the initial graph
  dataObj(initialVar)
  
  output$selectNodeUI <- renderUI({
    req(graphObj()) # Ensure graph object is available
    selectInput(
      "selectedNode",
      HTML("Schüler:in auswählen<br>(für Darstellung der Clique)"),
      choices = V(graphObj())$name
    )
  })
  
  observe({
    req(input$file1)
    
    # Read the Excel file
    friendships <- read_excel(input$file1$datapath, col_names = T)
    
    Var <- read_excel(input$file1$datapath, col_names = TRUE, sheet = 2, skip = 1)
    Var[1] <- NULL
    
    # Create an adjacency matrix
    adjacencyMatrix <-
      friendships |>
      select(-1) |>
      as.matrix()
    
    # Create a graph from the adjacency matrix
    g <-
      graph_from_adjacency_matrix(adjacencyMatrix, mode = "directed", diag = FALSE)
    
    V(g)$var1 <- Var[1] |> as_vector()
    V(g)$var2 <- Var[2] |> as_vector()
    
    graphObj(g)
    dataObj(Var)
  })
  
  output$networkDirect <- renderPlot({

    req(graphObj()) # new
    g <- graphObj() # new
    
    req(dataObj()) # new
    Var <- dataObj() # new
    
    ggraph(g, layout = "kk") +
      geom_edge_link(
        color = "gray",
        arrow = arrow(type = "closed", angle = 10),
        end_cap = circle(0.3, "inches"),
        start_cap = circle(0.3, "inches")
      ) +
      geom_node_point(aes(color = var1, size = var2)) +
      scale_size(range = c(10,19)) +
      geom_node_label(aes(label = name), repel = F) +
      ggtitle("Alle Nominierungen (gesamte Klasse)") +
      theme_void() +
      labs(color = names(Var)[1], size = names(Var)[2])
  })
  
  output$networkMutual <- renderPlot({
    
    req(graphObj()) # new
    g <- graphObj() # new
    
    req(dataObj()) # new
    Var <- dataObj() # new
    
    # Plot the graph
    ggraph(g |> as.undirected(mode = "mutual") |>
             as.directed(mode = "mutual"),
           layout = "kk") +
      geom_edge_link(
        color = "gray",
        arrow = arrow(type = "closed", angle = 10),
        end_cap = circle(0.3, "inches"),
        start_cap = circle(0.3, "inches")
      ) +
      geom_node_point(aes(color = var1, size = var2)) +
      scale_size(range = c(10,19)) +
      geom_node_label(aes(label = name), repel = F) +
      ggtitle("Wechselseitige Nominierungen (gesamte Klasse)") +
      theme_void() +
      labs(color = names(Var)[1], size = names(Var)[2])
  })
  
  output$networkCliqueDirect <- renderPlot({
    
    req(graphObj()) # new
    g <- graphObj() # new
    
    req(dataObj()) # new
    Var <- dataObj() # new
    
    # Plot the graph
    req(input$selectedNode)
    
    # Find the selected node and its neighbors
    subGraphNodes <-
      c(input$selectedNode,
        neighbors(g, input$selectedNode, mode = "all") |> names())
    subG <- induced_subgraph(g, subGraphNodes)
    
    ggraph(subG, layout = "kk") +
      geom_edge_link(
        color = "gray",
        arrow = arrow(type = "closed", angle = 10),
        end_cap = circle(0.3, "inches"),
        start_cap = circle(0.3, "inches")
      ) +
      geom_node_point(aes(color = var1, size = var2)) +
      scale_size(range = c(10,19)) +
      geom_node_label(aes(label = name), repel = F) +
      ggtitle("Alle Nominierungen (Clique der ausgewählten Schüler:in)") +
      theme_void() +
      labs(color = names(Var)[1], size = names(Var)[2])
      #theme(legend.position = "none")
  })
  
  output$networkCliqueMutual <- renderPlot({
    
    req(graphObj()) # new
    g <- graphObj() # new
    
    req(dataObj()) # new
    Var <- dataObj() # new
    
    # Plot the graph
    req(input$selectedNode)
    
    g <-
      g |>
      as.undirected(mode = "mutual") |>
      as.directed(mode = "mutual")
    
    # Find the selected node and its neighbors
    subGraphNodes <-
      c(input$selectedNode,
        neighbors(g, input$selectedNode) |> names())
    subG <- induced_subgraph(g, subGraphNodes)
    
    ggraph(subG, layout = "kk") +
      geom_edge_link(
        color = "gray",
        arrow = arrow(type = "closed", angle = 10),
        end_cap = circle(0.3, "inches"),
        start_cap = circle(0.3, "inches")
      ) +
      geom_node_point(aes(color = var1, size = var2)) +
      scale_size(range = c(10,19)) +
      geom_node_label(aes(label = name), repel = F) +
      ggtitle("Wechselseitige Nominierungen (Clique der ausgewählten Schüler:in)") +
      theme_void() +
      labs(color = names(Var)[1], size = names(Var)[2])
      #theme(legend.position = "none")
  })
  
  output$degree <- renderTable({
    
    req(graphObj()) # new
    g <- graphObj() # new
    
    # Table (degree)
    cbind(
      degree(g, mode = "in") |>
        sort(decreasing = T) |>
        as.data.frame() %>%
        rename("Erhaltenen Nominierungen" = names(.)[1]) |>
        rownames_to_column(var = "Name"),
      
      degree(g, mode = "out") |>
        sort(decreasing = T) |>
        as.data.frame() %>%
        rename("Ausgesendete Nominierungen" = names(.)[1]) |>
        rownames_to_column(var = "Name"),
      
      degree(g |> as.undirected(mode = "mutual")) |>
        sort(decreasing = T) |>
        as.data.frame() %>%
        rename("Wechselseitige Nominierungen" = names(.)[1]) |>
        rownames_to_column(var = "Name")
    )
  }, digits = 0)
}

# Run the application
shinyApp(ui = ui, server = server)
