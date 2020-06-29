#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork) # library for visualization
library(igraph) # library for network analysis


source('global.R')
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    h1(
      tags$b("Collaboration Network of Researchers"),
      align = "center",
      style = "font-family: 'Times', serif"
    )
  ),
  
  navbarPage(
    tags$b("Visualization Mode"),
    tabPanel("Individual Network",
             fluidPage(
               column(
                 6,
                 fluidRow(
                   textInput(
                     "PI1",
                     label = h4("Google Scholar URL for Researcher 1"),
                     value = ""
                   ),
                   numericInput(
                     "nPI1",
                     label = h4("Number of neighbor authors to render"),
                     value = 5,
                     min = 1,
                     max = NA,
                     step = 1,
                     width = NULL
                   ),
                   
                 )
                 ,
                 fluidRow(visNetworkOutput("ca_graph1"))
               ),
               column(
                 6,
                 fluidRow(
                   textInput(
                     "PI2",
                     label = h4("Google Scholar URL for Researcher 2"),
                     value = ""
                   ),
                   numericInput(
                     "nPI2",
                     label = h4("Number of neighbor authors to render"),
                     value = 5,
                     min = 1,
                     max = NA,
                     step = 1,
                     width = NULL
                   ),
                   
                 )
                 ,
                 fluidRow(visNetworkOutput("ca_graph2"))
                 
               ),
               fluidRow(actionButton("renderbutton1", "START"),)
             )),
    
    tabPanel("Network Statistics",
             fluidPage(
               fluidRow(
                 column(6,
                        plotOutput("hist1"),
                        style = "font-family: 'Times', serif"),
                 column(6,
                        plotOutput("hist2"),
                        style = "font-family: 'Times', serif")
                 
               ),
               
               fluidRow(tableOutput("statTab"))
             )),
    tabPanel(
      "App Info",
      fluidPage(
        fluidRow(h1('Version 0.1'),
                 align = "left"),
        
        fluidRow(
          h4(
            "This app uses the data available on Google Scholar to create coauthor networks of researchers. The data were obtained using the function get_coauthors() in the 'scholar' library. Visualization is done using visNetwork."
          ),
          h4("Data processing routines were created by Truong Pham."),
          h4(
            "Because Google does not allow high-speed automatic scraping of their Google Scholar site, this demo version only has data for the ten following authors: Howard Stone, Frances Arnold, Serafim Kalliadasis, Stefan Karpitschka,
                                       Michael Cates, Jeffrey Morris (CCNY), Omar Matar, Gareth Mckinley, Eric Lauga and Michael Graham (Wisconsin)."
          ),
          align = "left"
        ),
        
        fluidRow(
          h1("Instructions:"),
          h4("Copy the Google Scholar URL of researcher you are interested in."),
          h4(
            "For example, the URL for Howard Stone is https://scholar.google.com/citations?user=GfNjESUAAAAJ&hl=en"
          ),
          h4("Paste the URL in the box on page Individual Network."),
          h4("Please email questions and suggestions to ptruongnp@gmail.com!"),
          align = "left"
        ),
        fluidRow(
          h1("Glossary for Network Statistics:"),
          h4(
            "Closeness centrality: How close a node is to the other nodes. This number is higher for more central nodes. A researcher with a higher closeness centrality has more influence in his/her network of coauthors."
          ),
          h4(
            "Network density: (Total number of edges)/(maximum possible number of edges). In denser networks of coauthors, people are more active in collaborating."
          ),
          align = "left"
        ),
      )
      
    )
    
  ),
  style = "font-family: 'Times', serif",
  align = "center"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #New inputs are read only when the action button is pressed
  inputList <- eventReactive(input$renderbutton1, {
    if (input$PI1 == "")
    {
      ID1 = NULL
    }
    ID1 = fmt_url(input$PI1)
    nca1 = input$nPI1
    if (input$PI2 == "")
    {
      ID2 = NULL
    }
    ID2 = fmt_url(input$PI2)
    nca2 = input$nPI2
    return(list(c(ID1, ID2), c(nca1, nca2)))
  })
  #author IDs
  authorID1 = reactive({
    inputList()[[1]][1]
  })
  authorID2 = reactive({
    inputList()[[1]][2]
  })
  #complete author networks
  author_network1 = reactive({
    network_extract(authorID1())
  })
  author_network2 = reactive({
    network_extract(authorID2())
  })
  #author names
  profile1 = reactive({
    author_network1()[1, 1]
  })
  profile2 = reactive({
    author_network2()[1, 1]
  })
  # coauthors when the number of coauthors is specified
  chosen_coauthors1 = reactive({
    coauthor_slice(author_network1(), n_coauthors = inputList()[[2]][1])
  })
  chosen_coauthors2 = reactive({
    coauthor_slice(author_network2(), n_coauthors = inputList()[[2]][2])
  })
  # processed coauthor network for author i: a portion of the complete network containing only the chosen coauthors
  canw1 = reactive({
    if (is.null(authorID1()))
      return(NULL)
    coauthor_process(
      author_network1() %>% select(., -3) %>% filter(., author %in% chosen_coauthors1()),
      n_coauthors = inputList()[[2]][1],
      caslice = chosen_coauthors1()
    )
  })
  canw2 = reactive({
    if (is.null(authorID2()))
      return(NULL)
    coauthor_process(
      author_network2() %>% select(., -3) %>% filter(., author %in% chosen_coauthors2()),
      n_coauthors = inputList()[[2]][2],
      caslice = chosen_coauthors2()
    )
  })
  # get network edges and nodes for authors 1 and 2
  nodeVis1 = reactive({
    if (is.null(canw1()))
      return(NULL)
    get_vis_nodes(canw1())
  })
  edgeVis1 = reactive({
    if (is.null(canw1()))
      return(NULL)
    get_vis_edges(canw1())
  })
  
  nodeVis2 = reactive({
    if (is.null(canw2()))
      return(NULL)
    get_vis_nodes(canw2())
  })
  edgeVis2 = reactive({
    if (is.null(canw2()))
      return(NULL)
    get_vis_edges(canw2())
  })
  
  # visualize network using visNetwork
  output$ca_graph1 <- renderVisNetwork({
    if (is.null(nodeVis1()) | is.null(edgeVis1()))
      return(NULL)
    visNetwork(
      nodeVis1(),
      edgeVis1(),
      main = profile1(),
      height = "150px",
      width = "100%"
    ) %>%
      visEdges(arrows = "from")
  })
  
  output$ca_graph2 <- renderVisNetwork({
    if (is.null(nodeVis2()) | is.null(edgeVis2()))
      return(NULL)
    visNetwork(
      nodeVis2(),
      edgeVis2(),
      main = profile2(),
      height = "150px",
      width = "100%"
    ) %>%
      visEdges(arrows = "from")
  })
  
  #get network edges and nodes, formatted for igraph
  
  canwIG1 = reactive({
    select(author_network1(), -3)
  })
  canwIG2 = reactive({
    select(author_network2(), -3)
  })
  edgeIG1 = reactive({
    if (is.null(canwIG1()))
      return(NULL)
    get_igraph_edges(canwIG1())
  })
  edgeIG2 = reactive({
    if (is.null(canwIG2()))
      return(NULL)
    get_igraph_edges(canwIG2())
  })
  nodeIG1 = reactive({
    if (is.null(canwIG1()))
      return(NULL)
    get_vis_nodes(canwIG1())
  })
  nodeIG2 = reactive({
    if (is.null(canwIG2()))
      return(NULL)
    get_vis_nodes(canwIG2())
  })
  # create igraph objects from igraph dfs
  can_igraph1 = reactive({
    if (is.null(edgeIG1()))
      return(NULL)
    simplify(graph_from_data_frame(
      d = edgeIG1(),
      vertices = nodeIG1(),
      directed = FALSE
    ))
  })
  can_igraph2 = reactive({
    if (is.null(edgeIG2()))
      return(NULL)
    simplify(graph_from_data_frame(
      d = edgeIG2(),
      vertices = nodeIG2(),
      directed = FALSE
    ))
  })
  
  #create historgrams for the number of coauthors for each author in the network
  output$hist1 <- renderPlot({
    if (is.null(can_igraph1()))
      return(NULL)
    hist(
      degree(can_igraph1()),
      col = "blue",
      family = "serif",
      main = profile1(),
      xlab = "# of direct collaborators",
      ylab = "# of authors"
    )
  })
  output$hist2 <- renderPlot({
    if (is.null(can_igraph2()))
      return(NULL)
    hist(
      degree(can_igraph2()),
      col = "blue",
      family = "serif",
      main = profile2(),
      xlab = "# of direct collaborators",
      ylab = "# of authors"
    )
  })
  # calculate network statistics: closeness centrality and density
  statCA1 = reactive({
    if (is.null(can_igraph1()))
      return(NULL)
    c(unname(closeness(
      graph = can_igraph1(), vids = V(can_igraph1())[1]
    )), edge_density(can_igraph1()))
  })
  statCA2 = reactive({
    if (is.null(can_igraph2()))
      return(NULL)
    c(unname(closeness(
      graph = can_igraph2(), vids = V(can_igraph2())[1]
    )), edge_density(can_igraph2()))
  })
  
  # output statistics table
  output$statTab <- renderTable({
    if (is.null(statCA1()) | is.null(statCA2()))
      return(NULL)
    df = data.frame(
      c("Closeness Centrality", "Edge Density"),
      statCA1(),
      statCA2(),
      stringsAsFactors = FALSE
    )
    names(df) = c("Measures", profile1(), profile2())
    df
  },
  rownames = TRUE,
  digits = 8)
  
}

# Run the application
shinyApp(ui = ui, server = server)
