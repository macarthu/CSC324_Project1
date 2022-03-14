library(shiny)
library(ggplot2)
library(plotly)
#functions!

#Histogram functions:
#function for choosing data based on user-input
selecto <- function(x) {
  if(x == 1){
    return(trees)}
  else if(x == 2){
    return(plot0to25)}
  else if(x==3){
    return (plot26to50)}
  else if(x==4){
    return (plot51to72)
  }else if(x==5){
    return (unmarkedTrees)
  }else if(x==6){
    return (markedTrees)
  }
}

#Pie Chart functions:
#function for choosing data based on user-input
selectPieData <- function(x) {
  if(x == 1){
    return(piesSpecies)}
  else if(x == 2){
    return(pieSpeciesSansLoblolly)}
  else if(x == 3){
    return(piesCut)}
}
#function for choosing color Label based on user-input
selectLabel <- function(x) {
  if(x == 1 || x == 2){
    return("Species")}
  else if(x == 3){
    return("Cut (= 0) or Uncut (= 1)")}
}

#Scatterplot functions:
#Function for choosing axis data based on user-input
selectX <- function(x) {
  if(x == 1){
    return(trees$speciesName)}
  else if(x == 2){
    return(trees$Born)}
  else if(x==3){
    return (trees$si)}
  else if(x==4){
    return (trees$elevation)
  }else if(x==5){
    return (trees$age)
  }else if (x==6){
    return(trees$Year)
  }else if (x==7){
    return(trees$Cut)
  }else if (x==8){
    return(trees$Treeno)
  }
}

#function for choosing axis label based on user-input
selectLabel <- function(x) {
  if(x == 1){
    return("Species")}
  else if(x == 2){
    return("Year Born")}
  else if(x==3){
    return ("SI")}
  else if(x==4){
    return ("Elevation of Base (feet)")
  }else if(x==5){
    return ("Age of Tree (years)")
  }else if(x==6){
    return ("Year of measurement")
  }else if(x==7){
    return ("Marked for Cut? (0 = Y, 1 = N)")
  }else if (x==8){
    return("Tree Number")
  }
}

#function for choosing color based on user-input
selectColData <- function(x) {
  if(x == 1){
    return(trees$speciesName)}
  else if(x == 2){
    return(trees$Cut)}
}

#function for choosing color Label based on user-input
selectCol <- function(x) {
  if(x == 1){
    return("Species")}
  else if(x == 2){
    return("Cut (= 0) or Uncut (= 1)")}
}

# Define UI for application that explores tree data
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Age Histogram", fluid = TRUE,
             titlePanel(h3("Ages of Trees in designated Forest Service plots in Oregon, USA", align = "center")),
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 
                 # Input: Slider for the number of bins ----
                 sliderInput(inputId = "bins",
                             label = "Quantity of Grouping:",
                             min = 2,
                             max = 140,
                             value = 10),
                 
                 # Input: Which plot of trees will be graphed
                 radioButtons("radio", label = "Data Source",
                              choices = list("All" = 1,
                                             "Plots 1-25" = 2,
                                             "Plots 26-50" = 3,
                                             "Plots 51-72" = 4,
                                             "Trees Unmarked for Cutting" = 5,
                                             "Trees Marked for Cutting" = 6),
                              selected = 1
                 )
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Histogram ----
                 plotOutput(outputId = "histogram")
                 
               )
             )
             
             
    ),
    tabPanel("Pie Chart", fluid = TRUE,
      titlePanel(h3("Pie Charts", align = "center")),
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          
          selectInput("pieSelect", label = h2("Grouping for Pie Chart"),
                      choices = list("Species" = 1,
                                     "Species, without Loblollys" = 2,
                                     "Marked for Cut?" = 3),
                      selected = 1
          )
          
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: piechart ----
          plotlyOutput(outputId = "pieGuy", height = "100%")
          
        )
      )
      
    ),
    tabPanel("Correlation Exploration", fluid = TRUE,
      titlePanel(h3("Correlation Explorer: Find which variables are related!", align = "center")),
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          
          
          # Input: Which plot of trees will be graphed
          selectInput("select", label = h2("X-Axis Variable"),
                      choices = list("Year Born" = 2,
                                     "SI" = 3, "Elevation" = 4,
                                     "Age" = 5,
                                     "Year of Measurement" = 6,
                                     "Marked for Cut?" = 7,
                                     "Tree Number" = 8),
                      selected = 5
          ),
          
          selectInput("select2", label = h2("Y-Axis Variable"),
                      choices = list("Species" = 1,
                                     "Year Born" = 2,
                                     "SI" = 3,
                                     "Elevation" = 4,
                                     "Age" = 5,
                                     "Year of Measurement" = 6,
                                     "Marked for Cut?" = 7,
                                     "Tree Number" = 8),
                      selected = 1
          ),
          
          selectInput("select3", label = h2("Color Variable"),
                      choices = list("Species" = 1,
                                     "Marked for Cut?" = 2),
                      selected = 1
          ),
          
          sliderInput(inputId = "slider1", 
                      label = h3("Horizontal Jitter Value"), 
                      min = 1, 
                      max = 100, 
                      value = 5
          ),
          
          sliderInput(inputId = "slider2", 
                      label = h3("Vertical Jitter Value"), 
                      min = 1, 
                      max = 100, 
                      value = 5
          )
          
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Scatterplot ----
          plotOutput(outputId = "scatter")
          
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$histogram <- renderPlot({
    xDat    <- selecto(input$radio)$age
    bins <- seq(min(xDat), max(xDat), length.out = input$bins + 1)
    
    
    
    hist(xDat, breaks = bins, col = "#013220", border = "green",
         xlab = "Age of Tree (years)",
         ylab = "Quantity of Trees",
         main = "Histogram of Tree Age")
  })
  
  output$pieGuy <- renderPlotly({
    
    lab    <- selectLabel(input$pieSelect)
    pieData        <- selectPieData(input$pieSelect)
    #
    
    plot_ly(type='pie', 
            labels = pieData$NAME,
            values = pieData$Amount,
            textinfo = "none",
            hoverinfo= 'label+percent' ,
            insidetextorientation = 'radial')
    
  })
  
  output$scatter <- renderPlot({
    
    x    <- selectX(input$select)
    y    <- selectX(input$select2)
    colLabel    <- selectCol(input$select3)
    colData     <- selectColData(input$select3)
    jitW <- input$slider1
    jitH <- input$slider2
    
    ggplot(trees, aes(x = x, y = y, colour = colData)) +
      geom_point() + labs(color = colLabel,
                          x = selectLabel(input$select),
                          y = selectLabel(input$select2),
                          title = paste("Scatterplot of", selectLabel(input$select), "vs.", selectLabel(input$select2), sep = " "))+
      geom_jitter(width = jitW * (max(x) - min(x))/(5000), height = jitH / 100)
    
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
