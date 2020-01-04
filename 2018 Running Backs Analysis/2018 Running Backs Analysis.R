##### Necessary Libraries #####
library(broom)
library(readxl)
library(tidyverse)

#import the raw file provided to you
source <- read_excel("C:/Users/NTellaku/Documents/R/ff/10 Guys 1 Cup/Drafted Players.xlsx",
                     sheet = "2018")

##### Creating Analytical Data Set #####
#how much points did each drafted Running Back score in standard scoring in 2018?
rbs_points <- c(225, 38, 160, 129, 173, 203, 93, 152, 121, 88, 49, 87, 196, 8,
                35, 130, 135, 279, 199, 63, 107, 181, 313, 184, 119, 60, 251,
                272, 101, 58, 294, 87, 101, 168, 59, 97, 111, 0, 84, 132, 159,
                147, 164, 58, 109, 144, 0, 92, 161, 75, 224)

rbs <- source %>% 
  filter(Position == "RB") %>% 
  select(Owner, Player, Price) %>% 
  mutate(Points = rbs_points) %>% 
  arrange(Price)

#flipping the values for Conner and Bell
rbs[17, 4] <- 0
rbs[50, 4] <- 224


##### Visualizations #####
ggplot(rbs, aes(x = Price, y = Points)) +
  geom_point(size = 2, shape = 17) +
  theme_classic() +
  labs(title = "Price vs. Points",
       subtitle = "2018 Running Backs, switching Conner and Bell's Points",
       x = "Price ($)") +
  geom_smooth(method = "lm",
              formula = y ~ splines::bs(x, 4),
              se = F)

ggplot(rbs, aes(x = Price, y = Points)) +
  geom_point(size = 2, shape = 17, aes(color = Owner)) +
  theme_classic() +
  labs(title = "Price vs. Points",
       subtitle = "2018 Running Backs, switching Conner and Bell's Points",
       x = "Price ($)") +
  geom_smooth(method = "lm",
              formula = y ~ splines::bs(x, 4),
              se = F)

##### Analysis #####
#create the model separately
model <- lm(Points ~ splines::bs(Price, 4), data = rbs)

#add predicted values from model to original data set
rb_table <- augment(model, rbs) %>% 
  mutate(Expected = round(Points - .resid)) %>% 
  mutate(Residual = round(.resid)) %>% 
  select(Owner, Player, Price, Expected, Points, Residual) %>% 
  rename("Actual Points" = Points,
         "Expected Points" = Expected)

total_residual <- rb_table %>%
  group_by(Owner) %>%
  summarize("Total Residual" = sum(Residual)) %>% 
  arrange(`Total Residual`)
