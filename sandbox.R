library(shiny)
shinyApp(
  ui = fluidPage(
    titlePanel("Nucleotide Analysis"),
    sidebarLayout(
      sidebarPanel(
        textInput("sequence", "Enter sequence", 
                  value='GAGCACCGGGCGGCGCTCAACACCGTCGTCGACATCAACGAGCGGTTCGGCGTCGGCCCC'),
        downloadButton("downloadData", "Download Data")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Frequency Plot", plotOutput("nucleotide_plot")),
          tabPanel("Nucleotide Table", tableOutput("nucleotide_table")),
          tabPanel("GC Content", tableOutput("gc_cnt"))
        )
      )
    )
  ),
  
  
  
  server = function(input, output) {
    gc_cnt <- reactive({
      sequence <- unlist(strsplit(input$sequence, ""))
      gc_count <- sum(sequence %in% c("G", "C"))
      total_count <- length(input$sequence)
      gc_content <- gc_count / total_count
      return(data.frame(GC_content=gc_content))
    })
    
    counts <- reactive({
      sequence <- unlist(strsplit(input$sequence, ""))
      count_a <- sum(sequence == "A")
      count_c <- sum(sequence == "C")
      count_g <- sum(sequence == "G")
      count_t <- sum(sequence == "T")
      return(data.frame(Nucleotide = c("A", "C", "G", "T"), Count = c(count_a, count_c, count_g, count_t)))
    })
    
    output$nucleotide_table <- renderTable({
      counts()
    })
    
    output$gc_cnt <- renderTable({
      gc_cnt()
    })
    
    output$nucleotide_plot <- renderPlot({
      barplot(counts()$Count, 
              names.arg = counts()$Nucleotide, 
              xlab = "Nucleotide", 
              ylab = "Count", 
              main = "Nucleotide Frequency")
    })
  }
)


