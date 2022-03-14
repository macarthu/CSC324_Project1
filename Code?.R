library(tidyverse)
library(ggplot2)
library(plotly)
trees <- read.csv("/Users/harrymacarthur/Desktop/CSC 324/RDS-2021-0105/Data/hitchiti.csv")

#organizing trees by plot
plot0to25 <- filter(trees, plotnum <= 25)
plot26to50 <- filter(trees, plotnum > 25, plotnum <= 50)
plot51to72 <- filter(trees, plotnum > 50, plotnum <= 72)



#organizing trees by cut
unmarkedTrees <- filter(trees, Cut == 0)
markedTrees <- filter(trees, Cut == 1)

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
#function to apply the nameSpecies Function to a dataframe
renameSpecies <- function() {
  for(i in 1:(nrow(trees))){
    trees$speciesName[i] <- nameSpecies(trees$Species[i])
  }
}

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



for(i in 1:(nrow(trees))){
  countSpecies(trees$Species[i])
}

#temp values for a failed experiment, keeping for reminiscing
# x1= 0
# x2= 0
# x3= 0
# x4 = 0
# x5 = 0
# x6 = 0
# x7 = 0 
# x8 = 0 
# x9 = 0
# x10 = 0 
# x11 = 0 
# x12 = 0 
# x13 = 0 
# x14 = 0 
# x15 = 0 
# x16 = 0 
# x17 = 0 
# x18 = 0 
# x19 = 0
# x20 = 0
# x21 = 0

#failed function to tally species, bad rabbit hole
countSpecies <- function(x){
  for(i in 1:(nrow(trees))){
  if(x == 4){
    x1 = x1 + 1
  }else if(x == 68){
    x2 = x2 + 1
  }else if(x == 100){
    x3 = x3 + 1
  }else if(x == 110){
    x4 = x4 + 1
  }else if(x == 131){
    x5 = x5 + 1
  }else if(x == 310){
    x6 = x6 + 1
  }else if(x == 311){
    x7 = x7 + 1
  }else if(x == 316){
    x8 = x8 + 1
  }else if(x == 491){
    x9 = x9 + 1
  }else if(x == 500){
    x10 = x10 + 1
  }else if(x == 611){
    x11 = x11 + 1
  }else if(x == 621){
    x12 = x12 + 1
  }else if(x == 694){
    x13 = x13 + 1    
  }else if(x == 701){
    x14 = x14 + 1
  }else if(x == 762){
    x15 = x15 + 1
  }else if(x == 800){
    x16 = x16 + 1
  }else if(x == 802){
    x17 = x17 + 1
  }else if(x == 812){
    x18 = x18 + 1
  }else if(x == 827){
    x19 = x19 + 1
  }else if(x == 835){
    x20 = x20 + 1
  }else if(x == 970){
    x21 = x21 + 1
  }
  }
  listy <- list(rep(0, 21))
  listy[1] = x1
  listy[2] = x2
  listy[3] = x3
  listy[4] = x4
  listy[5] = x5
  listy[6] = x6
  listy[7] = x7
  listy[8] = x8
  listy[9] = x9
  listy[10] = x10
  listy[11] = x11
  listy[12] = x12
  listy[13] = x13
  listy[14] = x14
  listy[15] = x15
  listy[16] = x16
  listy[17] = x17
  listy[18] = x18
  listy[19] = x19
  listy[20] = x20
  listy[21] = x21
}

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

#function to sort select panel inputs
selecto <- function(x) {
  if(x == 1){
    return(plot0to25)}
  else if(x==2){
    return (plot26to50)}
  else if(x==3){
    return (plot50to72)
  }
  else{
    return(trees$age)
  }
}

# ggplot(data = data1) + 
#  geom_point(mapping = aes(x = Country, y = Elevation, color = Texture))
# location and soiurce type : ggplot(data = trees) +   geom_point(mapping = aes(x = latitude, y = longitude, color = source))
