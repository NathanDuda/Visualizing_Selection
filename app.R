# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(RIdeogram)

# Load data --------------------------------------------------------------------

species_dnds <- read.csv("C:/Users/17735/Downloads/Visualizing_Selection/species_dnds.tsv", sep="")
species_dnds <- species_dnds %>%
  filter(dnds > 1 & dnds < 2)

kar <- data.frame(
  Chr = c('X', 2, 3, 4, 5),
  Start = c(0, 0, 0, 0, 0),
  End = c(248956422, 242193529, 198295559, 190214555, 181538259)
)

# Assuming you have data for genes and their locations

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y", label = "Y-axis:",
        choices = c("species", "dnds"),
        selected = "species"
      ),
      
      selectInput(
        inputId = "x", label = "X-axis:",
        choices = c("species", "dnds"),
        selected = "dnds"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      dataTableOutput(outputId = "table"),  # Changed outputId to "table"
      plotOutput(outputId = "ideogram"),    # Changed to rideogramOutput
      br()
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = species_dnds, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  output$table <- renderDataTable({
    brushedPoints(species_dnds, brush = input$plot_brush) %>% select(species, dnds)
  })
  
  output$ideogram <- renderPlot({
    # Filter genes data based on selected points
    selected_genes <- brushedPoints(species_dnds, brush = input$plot_brush)
    filtered_genes_data <- species_dnds %>%
      filter(id %in% selected_genes$gene_id)  # Assuming gene_id is the identifier for genes
    
    # Plot rideogram
    ideogram(karyotype = kar, label = filtered_genes_data, label_type = 'marker')
    # You may need to adjust aesthetics and other settings based on your genes_data structure
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
