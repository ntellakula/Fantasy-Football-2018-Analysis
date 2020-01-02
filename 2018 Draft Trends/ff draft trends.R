##### Functions and Libraries
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)

#function for entire position prices by year
yearly_graphic <- function(dollar_switch, #whether or not $1 players to be included
                           pos, #2-letter abbreviation for position
                           color, #color for bar graph
                           full_pos #full position title to include in graphic title
) {
  
  #max y limit on graphic
  temp <- rep(0, length(list_of_years))
  for (i in 1:length(list_of_years)) {
    temp[i] <- source %>% 
      filter(Year == list_of_years[i]) %>% 
      filter(Position == eval(substitute(pos))) %>% 
      filter(Price > 1) %>% 
      pull(Price) %>% 
      mean()
  }
  y_max <- max(temp)
  
  if (dollar_switch == 0) {
    #each year, mean price per year
    prices <- rep(0, length(list_of_years))
    for (i in 1:length(list_of_years)) {
      prices[i] <- source %>% 
        filter(Year == list_of_years[i]) %>% 
        filter(Position == eval(substitute(pos))) %>% 
        pull(Price) %>% 
        mean()
    }
    
    #actual image code
    graphic <- tibble(list_of_years, prices) %>% 
      ggplot(aes(x = list_of_years, y = prices)) +
      geom_bar(stat = "identity", colour = "black",
               fill = eval(substitute(color)), alpha = 0.7) +
      theme_bw() +
      scale_x_continuous(labels = as.character(list_of_years),
                         breaks = list_of_years) +
      labs(x = "Year",
           y = "Price",
           title = paste("Average",
                         eval(substitute(full_pos)),
                         "Price Per Year")) +
      ylim(0, y_max)
  } else {
    prices <- rep(0, length(list_of_years))
    for (i in 1:length(list_of_years)) {
      prices[i] <- source %>% 
        filter(Year == list_of_years[i]) %>% 
        filter(Position == eval(substitute(pos))) %>% 
        filter(Price > 1) %>% 
        pull(Price) %>% 
        mean()
    }
    graphic <- tibble(list_of_years, prices) %>% 
      ggplot(aes(x = list_of_years, y = prices)) +
      geom_bar(stat = "identity", colour = "black",
               fill = eval(substitute(color)), alpha = 0.7) +
      theme_bw() +
      scale_x_continuous(labels = as.character(list_of_years),
                         breaks = list_of_years) +
      labs(x = "Year",
           y = "Price",
           title = paste("Average",
                         eval(substitute(full_pos)),
                         "Price Per Year (No $1 Players)")) +
      ylim(0, y_max)
  }
  return(graphic)
}

#function for players drafted multiple years (usually 5 or 6 out of 6)
longitudinal_graphic <- function(name, #string for player name
                                 the_color, #color
                                 player_list #vector of player names
) {
  y_max <- source %>% 
    filter(Player %in% player_list) %>% 
    pull(Price) %>% 
    max()
  source %>% 
    filter(Player == eval(substitute(name))) %>% 
    ggplot(aes(x = Year, y = Price)) +
    geom_bar(stat = "identity", colour = "black",
             fill = the_color, alpha = 0.8) +
    theme_bw() +
    labs(x = "Year",
         y = "Price",
         title = eval(substitute(name))) +
    scale_x_continuous(labels = as.character(list_of_years),
                       breaks = list_of_years) +
    ylim(0, y_max)
}

#function to output vector of prices: average of position by year
prices_vector <- function(dollar_switch, #whether or not $1 players to be included
                          pos #2-letter abbreviation for position
) {
  #initialize empty vector
  prices <- rep(0, length(list_of_years))
  
  #implement binary switch
  if (dollar_switch == 0) {
    for (i in 1:length(list_of_years)) {
      prices[i] <- source %>% 
        filter(Position == eval(substitute(pos))) %>% 
        filter(Year == list_of_years[i]) %>% 
        pull(Price) %>% 
        mean()
    }
  } else {
    for (i in 1:length(list_of_years)) {
      prices[i] <- source %>% 
        filter(Position == eval(substitute(pos))) %>% 
        filter(Year == list_of_years[i]) %>% 
        filter(Price > 1) %>% 
        pull(Price) %>% 
        mean()
    }
  }
  return(as.numeric(format(round(prices, 1), nsmall = 1)))
}

##### Importing the Data
#import the data
source <- read_excel("C:/Users/NTellaku/Documents/R/ff/10 Guys 1 Cup/Drafted Players.xlsx",
                     sheet = "All Years")

#list of all years in the auction draft thus far
list_of_years <- unique(source$Year)


##### Table 1: Dollar Players
table1 <- t(full_join(source %>%
                        group_by(Year) %>% 
                        filter(Price == 1) %>% 
                        summarise(dp = n()),
                      #first counting the $1 players, then all the drafted playres
                      source %>%
                        group_by(Year) %>% 
                        summarise(tot = n()),
                      by = "Year") %>% 
              #percentage of dollar playres
              mutate(pct = format(round(dp / tot * 100, 2), nsmall = 2),
                     pct2 = paste(pct, "%", sep = "")) %>% 
              select(-tot, -pct, -Year) %>% 
              rename("Dollar Players" = dp,
                     "% Dollar Players" = pct2))
colnames(table1) <- list_of_years


##### Table 2: Average Price of Position by Year
qb_prices <- prices_vector(0, "QB")
rb_prices <- prices_vector(0, "RB")
rb_prices_no1 <- prices_vector(1, "RB")
wr_prices <- prices_vector(0, "WR")
wr_prices_no1 <- prices_vector(1, "WR")
te_prices <- prices_vector(0, "TE")
te_prices_no1 <- prices_vector(1, "TE")


#creating the average dollar value per position into a table
table2 <- as_tibble(t(tibble(qb_prices,
                             rb_prices,
                             rb_prices_no1,
                             wr_prices,
                             wr_prices_no1,
                             te_prices,
                             te_prices_no1)))
colnames(table2) <- list_of_years
rownames(table2) <- c("QB", "RB", "RB > 1", "WR",
                      "WR > 1", "TE", "TE > 1")



##### Graphics
yearly_colors <- brewer.pal(4, "Set1")
qb_yearly <- yearly_graphic(0, "QB", yearly_colors[1], "Quarterback")
qb_yearly

rb_yearly <- yearly_graphic(0, "RB", yearly_colors[2], "Running Back")
rb_yearly_no1 <- yearly_graphic(1, "RB", yearly_colors[2], "Running Back")
grid.arrange(rb_yearly, rb_yearly_no1, ncol = 2)

wr_yearly <- yearly_graphic(0, "WR", yearly_colors[3], "Wide Receiver")
wr_yearly_no1 <- yearly_graphic(1, "WR", yearly_colors[3], "Wide Receiver")
grid.arrange(wr_yearly, wr_yearly_no1, ncol = 2)

te_yearly <- yearly_graphic(0, "TE", yearly_colors[4], "Tight End")
te_yearly_no1 <- yearly_graphic(1, "TE", yearly_colors[4], "Tight End")
grid.arrange(te_yearly, te_yearly_no1, ncol = 2)


##### Repeat Drafted Players
##### Kickers
#vector of players
common_k <- plyr::count(source %>%
                          filter(Position == "K") %>%
                          select(Player)) %>% 
  filter(freq >= 5) %>% 
  pull(Player)

#conditional to get the colors for the players
color_k <- brewer.pal(2, "Accent")

#initializing an empty list
graphics_list_k <- list()

#loop through all the players and store the images in the list
for (i in 1:length(common_k)) {
  graphics_list_k[[i]] <- longitudinal_graphic(common_k[i],
                                               color_k[i],
                                               common_k)
}
grid.arrange(graphics_list_k[[1]], graphics_list_k[[2]], ncol = 2)


##### Quarterbacks
#vector of players
common_qb <- plyr::count(source %>%
                           filter(Position == "QB") %>%
                           select(Player)) %>% 
  filter(freq >= 5) %>% 
  pull(Player)

#conditional to get the colors for the players
color_qb <- brewer.pal(7, "Accent")

#initializing an empty list
graphics_list_qb <- list()

#loop through all the players and store the images in the list
for (i in 1:length(common_qb)) {
  graphics_list_qb[[i]] <- longitudinal_graphic(common_qb[i],
                                                color_qb[i],
                                                common_qb)
}
grid.arrange(graphics_list_qb[[1]], graphics_list_qb[[2]],
             graphics_list_qb[[3]], graphics_list_qb[[4]],
             graphics_list_qb[[5]], graphics_list_qb[[6]],
             graphics_list_qb[[7]],
             ncol = 3)


##### Tight Ends
#vector of players
common_te <- plyr::count(source %>%
                           filter(Position == "TE") %>%
                           select(Player)) %>% 
  filter(freq >= 5) %>% 
  pull(Player)

#conditional to get the colors for the players
color_te <- brewer.pal(7, "Accent")

#initializing an empty list
graphics_list_te <- list()

#loop through all the players and store the images in the list
for (i in 1:length(common_te)) {
  graphics_list_te[[i]] <- longitudinal_graphic(common_te[i],
                                                color_te[i],
                                                common_te)
}
grid.arrange(graphics_list_te[[1]], graphics_list_te[[2]],
             graphics_list_te[[3]], graphics_list_te[[4]],
             graphics_list_te[[5]], graphics_list_te[[6]],
             graphics_list_te[[7]], ncol = 3)


##### Running Backs
#vector of players
common_rb <- plyr::count(source %>%
                           filter(Position == "RB") %>%
                           select(Player)) %>% 
  filter(freq >= 5) %>% 
  pull(Player)

#conditional to get the colors for the players
color_rb <- colorRampPalette(brewer.pal(8, "Dark2"))(11)

#initializing an empty list
graphics_list_rb <- list()

#loop through all the players and store the images in the list
for (i in 1:length(common_rb)) {
  graphics_list_rb[[i]] <- longitudinal_graphic(common_rb[i],
                                                color_rb[i],
                                                common_rb)
}

zeke <- source %>% 
  filter(Player == "Ezekiel Elliott") %>% 
  ggplot(aes(x = Year, y = Price)) +
  geom_bar(stat = "identity", colour = "black",
           fill = "#C9472D", alpha = 0.8) +
  theme_bw() +
  labs(x = "Year",
       y = "Price",
       title = "Ezekiel Elliott") +
  scale_x_continuous(labels = as.character(list_of_years),
                     breaks = list_of_years) +
  ylim(0, 72)

dj <- source %>% 
  filter(Player == "David Johnson") %>% 
  ggplot(aes(x = Year, y = Price)) +
  geom_bar(stat = "identity", colour = "black",
           fill = "#4072AF", alpha = 0.8) +
  theme_bw() +
  labs(x = "Year",
       y = "Price",
       title = "David Johnson") +
  scale_x_continuous(labels = as.character(list_of_years),
                     breaks = list_of_years) +
  ylim(0, 72)

grid.arrange(graphics_list_rb[[1]], graphics_list_rb[[2]], dj,
             graphics_list_rb[[3]], graphics_list_rb[[4]], zeke,
             graphics_list_rb[[5]], ncol = 3)
grid.arrange(graphics_list_rb[[6]],
             graphics_list_rb[[7]], graphics_list_rb[[8]],
             graphics_list_rb[[9]], graphics_list_rb[[10]],
             graphics_list_rb[[11]], ncol = 3)


##### Wide Receivers
common_wr <- plyr::count(source %>%
                           filter(Position == "WR") %>%
                           select(Player)) %>% 
  filter(freq >= 5) %>% 
  pull(Player)

#conditional to get the colors for the players
color_wr <- colorRampPalette(brewer.pal(8, "Accent"))(24)

#initializing an empty list
graphics_list_wr <- list()

#loop through all the players and store the images in the list
for (i in 1:length(common_wr)) {
  graphics_list_wr[[i]] <- longitudinal_graphic(common_wr[i],
                                                color_wr[i],
                                                common_wr)
}

grid.arrange(graphics_list_wr[[1]], graphics_list_wr[[2]],
             graphics_list_wr[[3]], graphics_list_wr[[4]],
             graphics_list_wr[[5]], graphics_list_wr[[6]],
             graphics_list_wr[[7]], graphics_list_wr[[8]],
             graphics_list_wr[[9]], ncol = 3)
grid.arrange(graphics_list_wr[[10]], graphics_list_wr[[11]],
             graphics_list_wr[[12]], graphics_list_wr[[13]],
             graphics_list_wr[[14]], graphics_list_wr[[15]],
             graphics_list_wr[[16]], graphics_list_wr[[17]],
             graphics_list_wr[[18]], ncol = 3)
grid.arrange(graphics_list_wr[[19]], graphics_list_wr[[20]],
             graphics_list_wr[[21]], graphics_list_wr[[22]],
             graphics_list_wr[[23]], graphics_list_wr[[24]], ncol = 3)
