library(data.table)
library(stringr)
library(tidyverse)
library(highcharter)

dt <- fread('/Users/deano/Documents/R/flexidash/spain_exp_player_stats/exp_player_stats_spain.csv')

remove.separator <- function(x, sep){
  # remove + / - separator from vector
  result <- unlist(strsplit(x,split=sep, fixed=TRUE))
  result <- result[[1]]
  return(result)
}

# remove + and - from xG
dt$xG <- mapply(remove.separator, dt$xG, '+')
dt$xG <- mapply(remove.separator, dt$xG, '-')

# remove + and - from xA
dt$xA <- mapply(remove.separator, dt$xA, '+')
dt$xA <- mapply(remove.separator, dt$xA, '-')
dt

dt$xG <- as.numeric(dt$xG)
dt$xA <- as.numeric(dt$xA)

# add G90
dt$G90 <- (dt$G / dt$Min) * 90

# add diff.GxG
dt$diff.GxG <- dt$G - dt$xG

hchart(dt$diff.GxG, type = "column")


barca <- dt[team=='Barcelona',]



# hchart(barca$diff.GxG, colorByPoint = TRUE, name = barca$PLayer)

# barca %>%
  # hchart(type = "column", hcaes(x = Player, value = diff.GxG)) 


barca$G.percent <- barca[, .(G.percent = G / sum(G))]
barca


dt

### graph
barca %>%
  hchart(type = "treemap", hcaes(x = Player, value = G, color = G)) 



# install.packages('highcharter', dependencies = TRUE)
glimpse(pokemon)

pokemon%>%
  count(type_1)%>%
  arrange(n)%>%
  hchart(type = "bar", hcaes(x = type_1, y = n))

pokemon%>%
  count(type_1)%>%
  arrange(n)%>%
  hchart(type = "treemap", hcaes(x = type_1, value = n, color = n))

z <- pokemon%>%
  count(type_1)%>%
  arrange(n)
str(z)

library("viridisLite")
#### this works!!!
barca %>%
  hchart(type = "treemap", hcaes(x = Player, value = G, color = G)) 

barca %>%
  hchart(type = "treemap", hcaes(x = Player, value = G.percent, color = G.percent)) 
