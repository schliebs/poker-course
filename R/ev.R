library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(extrafont)
library(ggforce)
library(proto)
library(ggimage)

# EV --------

ev <- function(bet,pot,win) {win * (pot+bet) + (1-win) * (-bet)}

ev(5,100,0.25)

pot <- 380
size = 1.5

ggplot(data = data.frame(x = c(-5,5)), mapping = aes(x = x))+
 stat_function(fun = ev,
               mapping = aes(color = "0.25"),
               size = size,
               args = list(pot = pot,
                           win = 0.25)) + 
  stat_function(fun = ev,
                mapping = aes(color = "0.332"),
                size = size,
                args = list(pot = pot,
                            win = 0.332)) + 
  stat_function(fun = ev,
                mapping = aes(color = "0.4"),
                size = size,
                args = list(pot = pot,
                            win = 0.4)) + 
  stat_function(fun = ev,
                mapping = aes(color = "0.15"),
                size = size,
                args = list(pot = pot,
                            win = 0.15)) + 
  scale_color_manual(name = "Win %",
                     values = c("0.15" = "red",
                                "0.25" = "blue",
                                "0.332" = "green",
                                "0.4" = "purple"))+
  geom_hline(yintercept = 0,linetype = "dotted",color = "blue",size = 1.5) +
  xlim(0,500) + 
  ylim(-100,100) + 
  labs(x = "Bet to call",
       y = "EV of calling",
       title = "EV of calling bet, depending on Win% and pot-size") + 
  theme_ipsum(grid = "XY") #+ ggsave(filename = "ev1.pdf")




0.332*750 + (1-0.332) * (-370)



