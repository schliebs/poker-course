library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(extrafont)
library(ggforce)
library(proto)
library(ggimage)
# Table -----

add2list <- function(listtt,name,..){
  
  added <- listtt %>% ..
  
  li <- 
    listtt %>%
    append(list(added))
  
  existnames <- names(li)[-length(li)]
  replace <- rep(NA,length(li)-1) #seq_along(li)
  putthere <- if(is.null(existnames)) replace else names(li)[-length(li)]
  
  li %>%
    purrr::set_names(nm = c(putthere,name))
}


#####

cards <- 
  list.files("cards") %>% 
  .[!map_lgl(.x = .,~str_detect(.x,"2.png"))] %>% 
  .[!map_lgl(.x = .,~str_detect(.x,"joker"))] %>% 
  .[!map_lgl(.x = .,~str_detect(.x,"blank"))] %>% 
  map_chr(.,~str_remove_all(.x,".png"))
cards

sc <- function(n = 17){sample(cards,size = n,replace = F)}
sc()

shuffle <- function(hands = 
                      list(
                        p1 = c("blank","blank"),
                        p2 = c("blank","blank"),
                        p3 = c("blank","blank"),
                        p4 = c("blank","blank"),
                        p5 = c("blank","blank"),
                        p6 = c("blank","blank")
                        ),
                    shuffle = FALSE){
  
  if(shuffle == TRUE){
    cards <- sc(17)
    
    hands <- 
      map(.x = c(1,3,5,7,9,11),
                 .f = function(x){
                   cards[c(x,x+1)]
                   }) %>% 
      set_names(c(paste0("p",1:6))) %>%
      c(board = list(cards[13:17]))
    }
  
}

cp <- function(card = "10_of_clubs"){paste0("cards/",card,".png")}

pokerhand <- function(hands = list(p1 = c("blank","blank"),
                                   p2 = c("blank","blank"))){
  
  '
  hands = list(p1 = c("10_of_clubs","10_of_clubs"),
               p2 = c("10_of_clubs","10_of_clubs"))
  '
  
  sf <- 10000
  # create data
  set.seed(1)
  n <- 10000
  x1 <- rnorm(n, mean = 0,sd = sf)
  y1 <- 0 + 0 * x1 + rnorm(n,mean = 0,sd = sf)
  
  df <- data.frame(x = c(x1), y = c(y1))
  
  players <- 
    data.frame(x = c(-2.5,-1,1,2.5,1,-1)*sf,
               y = c(0,2.5,2.5,0,-2.5,-2.5)*sf)
  
  images <- 
    data.frame(x = c(-1.45,-0.55,
                     0.55,1.45,
                     2.7,3.6,
                     0.55,1.45,
                     -1.45,-0.55,
                     -3.6,-2.7
                     )*sf,
               y = c(3,3,
                     3,3,
                     0,0,
                     -3,-3,
                     -3,-3,
                     0,0)*sf,
               image = c(cp(hands$p1[1]),cp(hands$p1[2]),
                         cp(hands$p2[1]),cp(hands$p2[2]),
                         cp(hands$p3[1]),cp(hands$p3[2]),
                         cp(hands$p4[1]),cp(hands$p4[2]),
                         cp(hands$p5[1]),cp(hands$p5[2]),
                         cp(hands$p6[1]),cp(hands$p6[2])))
  
  ggplot(data = df, aes(x = x, y = y)) +
    stat_ellipse(geom = "polygon", alpha = 1/2) + 
    geom_point(data = players,aes(x = x,y = y)) + 
    geom_image(mapping = aes(image = image), 
               data = images,size = 0.10) + 
    scale_x_continuous(limits = c(-4*sf,4*sf)) + 
    scale_y_continuous(limits = c(-4*sf,4*sf)) 
  
  
}


# cards -----

#Byron Knoll: http://code.google.com/p/vector-playing-cards/


# create data
set.seed(20130226)
n <- 200
x1 <- rnorm(n, mean = 2)
y1 <- 1.5 + 0.4 * x1 + rnorm(n)
x2 <- rnorm(n, mean = -1)
y2 <- 3.5 - 1.2 * x2 + rnorm(n)
class <- rep(c("A", "B"), each = n)
df <- data.frame(x = c(x1, x2), y = c(y1, y2), colour = class)

library(ggplot2)

ggplot(data = df, aes(x = x, y = y, colour = class)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = class)) + 
  geom_point(aes(x,y))




