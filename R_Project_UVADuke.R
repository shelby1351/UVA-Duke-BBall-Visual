## Importing libaraies and data
library(tidyverse)
library(network)
library(sna)
library(ggnetwork)
library(hms)
library(igraph)
library(lubridate)

#setwd()
load("UVA_Duke_020722.RData")
Data <- UVA_Duke_020722

##### Scatter plot of Shots Missed/Made over  ######
##### Basketball Court for Each Team Per Half ######
my_theme <- theme_bw() + theme(axis.text = element_text(size = 10), 
                               axis.title = element_text(size = 12))

# downloading file that outlines basketball court for plot background
source("draw_court.R")

# Plot Creation
plot1 <- draw_court() + geom_point(Data, mapping=aes(x = shot_x, y = shot_y,
                                                     color = shot_team, 
                                                     shape = shot_outcome, 
                                                     alpha = 0.9)) + 
  facet_wrap(~half) + 
  labs(title = "Shot Outcome and Court Placement By Team per Half") + my_theme

plot1


## Temporal Plot Showing the Score and Score Difference ###### 
##### for each Team over Time in the Basketball Game ##########

# Selecting necessary columns: home score, away score, time remaining 
BBall <- Data %>% select('home_score', 'away_score','secs_remaining')

# Creating time in bball game column
BBall$secs_ingame <- 2400 - BBall$secs_remaining #column of seconds in game
today<-as.POSIXct('2012-01-23 00:00:00 EST')
BBall$Timer <- today + (BBall$secs_in) #secs_ingame converted DateTime & minutes
BBall$Timer <- as.POSIXct(BBall$Timer, format("%H:%M:%S")) #Removing date

# Adding score difference column and pivoting
BBall1 <- BBall %>% select(home_score, away_score, Timer) %>% 
  rename(Duke = "home_score", UVA = "away_score") %>% 
  mutate(score_diff = UVA - Duke) %>% 
  pivot_longer(1:2, names_to = "Value", values_to = 'Score')

# Plot Creation
plot2 <- ggplot(BBall1) +
  geom_line(aes(x = Timer, y = Score, col = Value)) +
  scale_color_manual("", values = c("UVA" = "Orange","Duke" = "steelblue3")) + 
  geom_rect(aes(xmin = lag(Timer), xmax = Timer, ymin = 0, ymax = max(Score), 
                fill = score_diff), alpha = 0.6) + 
  scale_fill_gradient2("Score Difference", low = "royalblue", mid = "white", 
                       high = "orange", midpoint = 0) + 
  labs(x = "Game Time (hh:mm:ss)", y = "Number of Points",
       title = "UVA vs Duke Points Score throughout Game") +
  my_theme

plot2

