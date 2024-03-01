library(shiny)
library(ggplot2)
library(dplyr)
library(RIdeogram)
library(Biostrings)

# Load data --------------------------------------------------------------------

species_dnds <- read.csv("C:/Users/17735/Downloads/Visualizing_Selection/species_dnds.tsv", sep="")
species_dnds <- species_dnds %>%
  filter(dnds > 1 & dnds < 2)

genome <- readDNAStringSet("C:/Users/17735/Downloads/GCF_000001215.4_Release_6_plus_ISO1_MT_genomic.fna")
genome <- genome[!grepl("chromosome Y| chromosome 4| mitochondr", names(genome))]
genome <- genome[grepl("^NT|^NC", names(genome))]
names(genome) <- sub(".+\\s", "", names(genome))

kar <- data.frame(
  Chr = names(genome),
  Start = c(0, 0, 0, 0, 0),
  End = width(genome)
)

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
      ),
      fileInput("hyphy_output", "HyPhy Output File:"),
      fileInput("genome", "Genome Assembly")
    ),
    mainPanel(
      selectInput("species_select", "Select Species:", choices = NULL),
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      imageOutput("chromosome_png"),
      dataTableOutput(outputId = "table"), 
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
    brushedPoints(species_dnds, brush = input$plot_brush) %>% 
      select(species, id, dnds, dn, ds, chrom, start, end)
  })
  
  observeEvent(input$plot_brush, {
    # Filter genes data based on selected points
    selected_genes <- brushedPoints(species_dnds, brush = input$plot_brush)
    
    updateSelectInput(session, "species_select", choices = unique(selected_genes$species))
    
    filtered_genes_data <- species_dnds %>%
      filter(species %in% input$species_select & 
               id %in% selected_genes$id)
    
    if (nrow(filtered_genes_data) > 0) {
      filtered_genes_data <- filtered_genes_data %>%
        mutate(Shape = 'circle', color = 'blue') %>%
        select(Type = species, Shape, Chr = chrom, Start = start, End = end, color)
      
      ideogram(karyotype = kar, label = filtered_genes_data, label_type = 'marker')
      convertSVG("chromosome.svg", device = "png")
      
      output$chromosome_png <- renderUI({
        tags$img(src = "chromosome.png", height = '600', width = "500")
      })
    } else {
      # If filtered_genes_data is empty, render an empty image
      output$chromosome_png <- renderUI({
        tags$img(src = "chromosome.png", height = '600', width = "500")
      })
    }
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
