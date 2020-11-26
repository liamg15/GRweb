library(shiny) # Load packages
library(tidyverse)
library(colourpicker)
library(htmltools)
options(shiny.autoreload = TRUE)

hempmean <- data.frame(read_csv("hemp_thesis_mastersheet_april1.csv")) # Load data
headerImagePanel <- function(title, src) {
  div(
    style = "display: inline; position: relative",
    img(
      src = src,
      style = "width:1000%; max-width:1000%; position: absolute; z-index:-1;"
    ),
    h1(title, style = "display: inline;") # Creating parameters for the background picture
  )
}

ui <- 
  fluidPage(
  #Title
  titlePanel(HTML("<b>2019 Thesis Project: Ezyme Inhibition Assays</b>")), 
             headerImagePanel(HTML("<i> Cannabis sativa <i>"), "5df0806d04014.image.jpg"),

  
  #Add slider input
  sidebarLayout(
    sidebarPanel(
     sliderInput(
       # Slider to adjust the range of total cannabinoid content per sample 
              inputId = "Tot",
              label = "Cannabinoid Concentration (ug/ml)",
              value = c(300,500),# Set default range to 300-500 and parameters based on range of data
              min = 300, max = 1000, step = 50),

     #Create spaces for plots in sidebar
  plotOutput("THC"),
    plotOutput("CBD"),
  textOutput("reportnCOX1"),
  textOutput("reportnCOX2"),
  textOutput("reportuhemp"),
  textOutput("reporthhemp"),
  textOutput("reportucannabis"),
  textOutput("reporthcannabis")
  ),
  
  #Create spaces for plots in main panel
  mainPanel(
    plotOutput("COX1"),
    plotOutput("COX2")
    
    )
  )    


)

server <- function(input, output) {
  
  # Presetting input to Total.Cannabinoids and filter for enzyme cyclooxygenase I (COX-1)
  hemp1 <- reactive({
    hempmean %>% 
      filter(Total.Cannabinoids <= input$Tot[2],
             Total.Cannabinoids >= input$Tot[1]) %>% 
      filter(Enzyme == "COX-1")
  })
  
  # Presetting input to Total.Cannabinoids and filter for enzyme cyclooxygenase II (COX-2)
  hemp2 <- reactive({
    hempmean %>% 
        filter(Total.Cannabinoids <= input$Tot[2],
               Total.Cannabinoids >= input$Tot[1]) %>% 
        filter(Enzyme == "COX-2") 
  })
  
  # Just filtering for range inputs
  hemp3 <- reactive({
    hempmean %>% 
      filter(Total.Cannabinoids <= input$Tot[2],
           Total.Cannabinoids >= input$Tot[1])
  })
  
    #Create plot of percent inhibition for COX-1 by cannabinoid content
  output$COX1 <-
  renderPlot({
    
    ggplot(hemp1(), aes(Total.Cannabinoids, Mean)) +
      
    geom_point(aes(colour = Sample), size = 4) +
      
      labs(title = "Cyclooxygenase I") +
      
      ylab("Mean Percent Inhibition %") +
      
      xlab("Total Cannabinoids (ug/ml)") +
      
      theme_classic(base_size = 15) +
      
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "none") 
      
        })
  
    #Create plot of percent inhibition for COX-2 by cannabinoid content
  output$COX2 <-
    renderPlot({
    
      ggplot(hemp2(), aes(Total.Cannabinoids, Mean)) +
        
        geom_point(aes(colour = Sample), size = 4) +
        
        labs(title = "Cyclooxygenase II") +
        
        ylab("Mean Percent Inhibition %") +
        
        xlab("Total Cannabinoids (ug/ml)") +
        
        theme_classic(base_size = 15) +
        
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 15))
        
          })
  
    #Create a plot in sidebar to show the THC content of the samples within the range observed
  output$THC <-
    renderPlot({
       
      ggplot(hemp3(), aes(Sample, Total.THC)) +
        
        geom_boxplot(aes(colour = Sample)) +
        
        theme_classic(base_size = 20) +
        
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              legend.position = "none") +
        
        ylab("Total THC (ug/ml)")
    
      })
 
  output$CBD <-
    renderPlot({
      
      #Create a plot in sidebar to show the CDB content of the samples within the range observed
      ggplot(hemp3(), aes(Sample, Total.CBD)) +
        
        geom_boxplot(aes(colour = Sample)) +
        
        theme_classic(base_size = 20) +
        
        theme(axis.title.x=element_blank(),
              legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        
        ylab("Total CBD (ug/ml)")
      
          })
  # All of the lines below are to add sample size values to the sidebar
  output$reportnCOX1 <-
    renderText({
      
      numsamples <- nrow(hemp1())
      if(is.null(numsamples)) {
        numsamples <- 0
      }
      paste0("The sample size for COX-1 in this range is:", numsamples)
    })

  output$reportnCOX2 <-
    renderText({
  numsamples <- nrow(hemp2())
  if(is.null(numsamples)) {
    numsamples <- 0
  }
  paste0("The sample size for COX-2 in this range is:", numsamples)
  })
  
  output$reportuhemp <-
    renderText({
      hemp <- hempmean %>% 
        filter(Total.Cannabinoids <= input$Tot[2],
               Total.Cannabinoids >= input$Tot[1]) %>% 
        filter(Sample == "Hemp Unheated") # Sample size of unheated hemp (high CBD/low THC)
      numsamples <- nrow(hemp)
      if(is.null(numsamples)) {
        numsamples <- 0
      }
      paste0("The sample size of unheated hemp in this range is:", numsamples)
    })
  
  output$reporthhemp <-
    renderText({
      hemp <- hempmean %>% 
        filter(Total.Cannabinoids <= input$Tot[2],
               Total.Cannabinoids >= input$Tot[1]) %>% 
        filter(Sample == "Hemp Heated") #Sample size of heated hemp
      numsamples <- nrow(hemp)
      if(is.null(numsamples)) {
        numsamples <- 0
      }
      paste0("The sample size of heated hemp in this range is:", numsamples)
    })
  
  output$reportucannabis <-
    renderText({
      hemp <- hempmean %>% 
        filter(Total.Cannabinoids <= input$Tot[2],
               Total.Cannabinoids >= input$Tot[1]) %>% 
        filter(Sample == "Cannabis Unheated") # Sample size of unheated cannabis (low CBD/high THC)
      numsamples <- nrow(hemp)
      if(is.null(numsamples)) {
        numsamples <- 0
      }
      paste0("The sample size of unheated cannabis in this range is:", numsamples)
    })
  
  output$reporthcannabis <-
    renderText({
      hemp <- hempmean %>% 
        filter(Total.Cannabinoids <= input$Tot[2],
               Total.Cannabinoids >= input$Tot[1]) %>% 
        filter(Sample == "Cannabis Heated") # Sample size of heated cannabis
      numsamples <- nrow(hemp)
      if(is.null(numsamples)) {
        numsamples <- 0
      }
      paste0("The sample size of heated cannabis in this range is:", numsamples)
    })
  
}

shinyApp(ui = ui, server = server)