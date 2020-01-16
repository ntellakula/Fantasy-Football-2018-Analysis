library(tidyverse)

nikhil <- c(130, 101, 83, 127, 90, 125, 93, 97, 105)
mount <- c(76, 69, 97, 70, 85, 111, 98, 63, 77)
alec <- c(71, 84, 59, 103, 99, 85, 140, 84, 99)
sirpi <- c(102, 87, 69, 53, 79, 96, 75, 99, 64)
oatman <- c(107, 78, 89, 115, 96, 139, 91, 118, 91)
babcock <- c(87, 100, 106, 85, 93, 91, 118, 89, 124)
jordan <- c(139, 98, 118, 137, 80, 67, 98, 79, 99)
casey <- c(97, 103, 93, 142, 113, 123, 120, 149, 134)
badillo <- c(98, 128, 81, 97, 71, 76, 77, 111, 104)
kooper <- c(89, 90, 74, 98, 102, 124, 77, 95, 102)

name <- c("Nikhil", "Mount", "Alec", "Sirpi", "Oatman", "Babcock", 
          "Jordan", "Casey", "Badillo", "Kooper")

temp <- as.tibble(rbind(nikhil, mount, alec, sirpi, oatman,
                        babcock, jordan, casey, badillo, kooper))

temp <- temp %>% mutate(mean = apply(temp, 1, mean), sd = apply(temp, 1, sd), 
                        pfor = apply(temp, 1, sum), name = name)

wins <- c(7, 3, 5, 2, 6, 3, 5, 6, 5, 3)
loss <- length(nikhil) - wins
pagt <- c(865, 876, 790, 850, 830, 926, 868, 964, 839, 937)

league_info <- as.tibble(cbind(temp$name, wins, loss, temp$pfor, pagt,
                               temp$mean, temp$sd)) %>% 
  rename("Team" = V1, "Wins" = wins, "Losses" = loss, "Points For" = V2,
         "Points Against" = pagt, "Mean" = V3, "Standard Deviation" = V4) %>% 
  mutate(new_wins = wins, new_losses = loss)


#Function to see who wins the matchup
matchup <- function(sims, team1, team2) {
  
  #sims: number of simulations
  #team1: The first team in the matchup
  #team2: The second team in the matchup
  
  home <- rnorm(sims, as.numeric(league_info %>%
                                filter(Team == team1) %>%
                                select(Mean)),
                as.numeric(league_info %>%
                             filter(Team == team1) %>%
                             select("Standard Deviation")))
  away <- rnorm(sims, as.numeric(league_info %>%
                                filter(Team == team2) %>%
                                select(Mean)),
                as.numeric(league_info %>%
                             filter(Team == team2) %>%
                             select("Standard Deviation")))
  
  #for loop to determine who has more higher scores
  
  result = 0 #initialize
  for (i in c(1:sims)) {
    if (home[i] > away[i]) {
      result[i] = 1
    } else {
      result[i] = 0
    }
  }
  
  #checking if simulations is odd or even for conditional, want odd
  if ((sims %% 2) == 0) {
    sims = sims + 1
  }
  
  #conditional to add the win and loss statistic
  if (sum(result) >= ceiling(sims / 2)) {
    a <- league_info %>% 
      filter(Team == team1) %>% 
      mutate(new_wins = new_wins + 1)
    
    b <- league_info %>%
      filter(Team == team2) %>%
      mutate(new_losses = new_losses + 1)
  } else {
    a <- league_info %>%
      filter(Team == team1) %>%
      mutate(new_losses = new_losses + 1)
    
    b <- league_info %>%
      filter(Team == team2) %>%
      mutate(new_wins = new_wins + 1)
  }
  c <- rbind(a, b)
  return(c)
}


#Running the program multiple times
output <- vector("list", 1000)

for (i in 1:1000) {
  
  league_info <- as.tibble(cbind(temp$name, wins, loss, temp$pfor, pagt,
                                 temp$mean, temp$sd)) %>% 
    rename("Team" = V1, "Wins" = wins, "Losses" = loss, "Points For" = V2,
           "Points Against" = pagt, "Mean" = V3, "Standard Deviation" = V4) %>% 
    mutate(new_wins = wins, new_losses = loss)
  
  #Week 9
  #league_info <- rbind(league_info,
  #                     matchup(1, "Nikhil", "Jordan"),
  #                     matchup(1, "Sirpi", "Oatman"),
  #                     matchup(1, "Casey", "Babcock"),
  #                     matchup(1, "Badillo", "Kooper"),
  #                     matchup(1, "Mount", "Alec")) %>% 
  #  slice((n() - 9):n())
  
  #Week 10
  league_info <- rbind(league_info,
                       matchup(1, "Nikhil", "Oatman"),
                       matchup(1, "Casey", "Sirpi"),
                       matchup(1, "Babcock", "Badillo"),
                       matchup(1, "Jordan", "Mount"),
                       matchup(1, "Kooper", "Alec")) %>% 
    slice((n() - 9):n())
  
  #Week 11
  league_info <- rbind(league_info,
                       matchup(1, "Nikhil", "Sirpi"),
                       matchup(1, "Badillo", "Casey"),
                       matchup(1, "Mount", "Oatman"),
                       matchup(1, "Alec", "Babcock"),
                       matchup(1, "Kooper", "Jordan")) %>% 
    slice((n() - 9):n())
  
  #Week 12
  league_info <- rbind(league_info,
                       matchup(1, "Nikhil", "Mount"),
                       matchup(1, "Badillo", "Sirpi"),
                       matchup(1, "Alec", "Casey"),
                       matchup(1, "Oatman", "Kooper"),
                       matchup(1, "Babcock", "Jordan")) %>% 
    slice((n() - 9):n())
  
  pfor <- as.numeric(league_info$`Points For`)
  
  league_info <- league_info %>% 
    mutate(pfor) %>% 
    select(-"Points For") %>% 
    rename("Points For" = pfor) %>% 
    arrange(desc(new_wins), desc(`Points For`)) %>% 
    mutate(rank = c(1:10))
  
  league_extra <- league_info %>% 
    select("Team", rank) %>% 
    arrange(Team, rank)
  
  output[i] <- league_extra %>% select(rank)
}

#slicing the list to just the number each player gets a rank
monte <- as.data.frame(output)
colnames(monte) <- rep("Rank", 1000)
monte$newcolumn <- league_extra$Team
monte <- monte[c(1001, 1:1000)]
monte <- t(monte)
colnames(monte) <- league_extra$Team
monte <- as.tibble(monte) %>% slice(2:n())

alec_rank <- t(as.data.frame(table(monte$Alec)))
colnames(alec_rank) <- alec_rank[1, ]
alec_rank <- as.data.frame(alec_rank)
alec_rank <- alec_rank[2, ]
row.names(alec_rank) <- "Alec"

babcock_rank <- t(as.data.frame(table(monte$Babcock)))
colnames(babcock_rank) <- babcock_rank[1, ]
babcock_rank <- as.data.frame(babcock_rank)
babcock_rank <- babcock_rank[2, ]
row.names(babcock_rank) <- "Babcock"

badillo_rank <- t(as.data.frame(table(monte$Badillo)))
colnames(badillo_rank) <- badillo_rank[1, ]
badillo_rank <- as.data.frame(badillo_rank)
badillo_rank <- badillo_rank[2, ]
row.names(badillo_rank) <- "Badillo"

casey_rank <- t(as.data.frame(table(monte$Casey)))
colnames(casey_rank) <- casey_rank[1, ]
casey_rank <- as.data.frame(casey_rank)
casey_rank <- casey_rank[2, ]
row.names(casey_rank) <- "Casey"

jordan_rank <- t(as.data.frame(table(monte$Jordan)))
colnames(jordan_rank) <- jordan_rank[1, ]
jordan_rank <- as.data.frame(jordan_rank)
jordan_rank <- jordan_rank[2, ]
row.names(jordan_rank) <- "Jordan"

kooper_rank <- t(as.data.frame(table(monte$Kooper)))
colnames(kooper_rank) <- kooper_rank[1, ]
kooper_rank <- as.data.frame(kooper_rank)
kooper_rank <- kooper_rank[2, ]
row.names(kooper_rank) <- "Kooper"

mount_rank <- t(as.data.frame(table(monte$Mount)))
colnames(mount_rank) <- mount_rank[1, ]
mount_rank <- as.data.frame(mount_rank)
mount_rank <- mount_rank[2, ]
row.names(mount_rank) <- "Mount"

nikhil_rank <- t(as.data.frame(table(monte$Nikhil)))
colnames(nikhil_rank) <- nikhil_rank[1, ]
nikhil_rank <- as.data.frame(nikhil_rank)
nikhil_rank <- nikhil_rank[2, ]
row.names(nikhil_rank) <- "Nikhil"

oatman_rank <- t(as.data.frame(table(monte$Oatman)))
colnames(oatman_rank) <- oatman_rank[1, ]
oatman_rank <- as.data.frame(oatman_rank)
oatman_rank <- oatman_rank[2, ]
row.names(oatman_rank) <- "Oatman"

sirpi_rank <- t(as.data.frame(table(monte$Sirpi)))
colnames(sirpi_rank) <- sirpi_rank[1, ]
sirpi_rank <- as.data.frame(sirpi_rank)
sirpi_rank <- sirpi_rank[2, ]
row.names(sirpi_rank) <- "Sirpi"

final <- as.tibble(Reduce(function(x, y) merge(x, y, all = TRUE),
                          list(alec_rank, babcock_rank, badillo_rank,
                               casey_rank, jordan_rank, kooper_rank,
                               mount_rank, nikhil_rank, oatman_rank,
                               sirpi_rank))) %>%
  select(8, 6, 7, 5, 10, 9, 1, 2, 3, 4)


the_names <- c("Alec", "Babcock", "Badillo", "Jordan", "Kooper", "Mount",
               "Sirpi", "Casey", "Nikhil", "Oatman")
rownames(final) <- the_names
