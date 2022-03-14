library(shiny)
library(ggplot2)
library(plotly)
#managing data
trees <- read.csv("/Users/harrymacarthur/Desktop/CSC 324/RDS-2021-0105/Data/hitchiti.csv")
#organizing trees by plot
plot0to25 <- filter(trees, plotnum <= 25)
plot26to50 <- filter(trees, plotnum > 25, plotnum <= 50)
plot51to72 <- filter(trees, plotnum > 50, plotnum <= 72)
#organizing trees by cut
unmarkedTrees <- filter(trees, Cut == 0)
markedTrees <- filter(trees, Cut == 1)

#list of all trees by species
hardwoods <- filter(trees, Species == 4)
eastern_red_cedar <- filter(trees, Species == 068)
pines <- filter(trees, Species == 100)
shortleaf_pine <- filter(trees, Species == 110)
loblolly_pine <- filter(trees, Species == 131)
maples <- filter(trees, Species == 310)
florida_maple <- filter(trees, Species == 311)
red_maple <- filter(trees, Species == 316)
flowering_dogwood <- filter(trees, Species == 491)
hawthorns <- filter(trees, Species == 500)
sweetgum <- filter(trees, Species == 611)
yellow_poplar <- filter(trees, Species == 621)
black_gum <- filter(trees, Species == 694)
eastern_hophornbeam <- filter(trees, Species == 701)
black_cherry <- filter(trees, Species == 762)
oaks <- filter(trees, Species == 800)
white_oak <- filter(trees, Species == 802)
southern_red_oak <- filter(trees, Species == 812)
water_oak <- filter(trees, Species == 827)
post_oak <- filter(trees, Species == 835)
elms <- filter(trees, Species == 970)

#creation of dataframe for piecharts for species
piesSpecies <- data.frame(NAME = c("Hardwood Species", "Eastern Red Cedar", "Unknown Pine Species", "Shortleaf Pine", "Loblolly Pine", "Maple Species",
                                   "Florida Maple", "Red Maple", "Flowering Dogwood", "Hawthorn Species", "Sweetgum", "Yellow Poplar", "Black Gum",  "Eastern Hophornbeam", "Black Cherry", 
                                   "Oak Species", "White Oak", "Southern Red Oak", "Water Oak", "Post Oak", "Elm"),
                          Amount = c(nrow(hardwoods), nrow(eastern_red_cedar), nrow(pines), nrow(shortleaf_pine), nrow(loblolly_pine), nrow(maples),
                                     nrow(florida_maple), nrow(red_maple), nrow(flowering_dogwood), nrow(hawthorns), nrow(sweetgum), nrow(yellow_poplar), nrow(black_gum), nrow(eastern_hophornbeam), nrow(black_cherry),
                                     nrow(oaks), nrow(white_oak), nrow(southern_red_oak), nrow(water_oak), nrow(post_oak), nrow(elms))) 
#removing list behavior
piesSpecies$NAME <- unlist(piesSpecies$NAME)
piesSpecies$Amount <- unlist(piesSpecies$Amount)

#creating pieSpecies with no loblollys
pieSpeciesSansLoblolly <- filter(piesSpecies, NAME != "Loblolly Pine")

#creation of dataframe for piecharts for cut marking
piesCut <- data.frame(NAME = c("Marked for Cutting", "Unmarked"),
                      Amount = c(nrow(markedTrees), nrow(unmarkedTrees)))
#removing list behavior
piesCut$NAME <- unlist(piesCut$NAME)
piesCut$Amount <- unlist(piesCut$Amount)

#functions!

#data functions
#function to categorize all tree species in data set
#species number and name was taken from the metadata file for the data set
nameSpecies <- function(x){
  if(x == 4){
    return("Hardwood Species")
  }else if(x == 68){
    return("Eastern Red Cedar")
  }else if(x == 100){
    return("Unknown Pine Species")
  }else if(x == 110){
    return("Shortleaf Pine")
  }else if(x == 131){
    return("Loblolly Pine")
  }else if(x == 310){
    return("Maple Species")
  }else if(x == 311){
    return("Florida Maple")
  }else if(x == 316){
    return("Red Maple")
  }else if(x == 491){
    return("Flowering Dogwood")
  }else if(x == 500){
    return("Hawthorn Species")
  }else if(x == 611){
    return("Sweetgum")
  }else if(x == 621){
    return("Yellow Poplar")
  }else if(x == 694){
    return("Black Gum")
  }else if(x == 701){
    return("Eastern Hophornbeam")
  }else if(x == 762){
    return("Black Cherry")
  }else if(x == 800){
    return("Oak Species")
  }else if(x == 802){
    return("White Oak")
  }else if(x == 812){
    return("Southern Red Oak")
  }else if(x == 827){
    return("Water Oak")
  }else if(x == 835){
    return("Post Oak")
  }else if(x == 970){
    return("Elm")
  }else{
    return("Unknown")
  }
}
trees$speciesName <- ""
for(i in 1:(nrow(trees))){
  trees$speciesName[i] <- nameSpecies(trees$Species[i])
}

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
