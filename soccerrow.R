library(dplyr)
library(DT)
library(rsample)
library(Metrics)
library(rpart)
library(randomForest)
library(nnet)
library(caret)

#Part 1
#Loading Data and assingin it to data
data <- read.csv("all_matches.csv")

data <- data %>%
  filter(tournament == 'World Cup')
#Extracting countries 
home <- unique(data['home_team'])
away <- unique(data['away_team'])

#Perparing countries to join them into one variable called countries
home <- home %>%
  rename(country= home_team)

away <- away %>%
  rename(country= away_team)

#Joining the home countries and away countries
countries <- rbind(home, away)

#Making sure I don't get duplicated entries
countries <- unique(countries$country)

#Turning the data into a data frame
countries <- as.data.frame(countries)

#Check if I still have any duplicated entries
sum(duplicated(countries))


#Getting random 32 countries to later on split between home and away columns in a data frame
sample_countries <- sample(countries$countries, size = 32, replace = FALSE)

#Creating a random team vs team
home_team <- sample_countries[1:16]
away_team <- sample_countries[17:32]

#Join the data into a data frame
contest <- data.frame(A= home_team, B = away_team)

#View the contest standings 
# first 32 teams competing against each other
datatable(contest)


#Part 2 loading the data
data <- read.csv("all_matches.csv")


data$winner <- ifelse(data$home_score > data$away_score, 1, 0)

world_cup <- data %>%
  select(-date, - neutral, -home_score, -away_score, -neutral, -tournament, -country)

world_cup <- world_cup %>%
  rename(A= home_team, B= away_team)

world_cup2 <- world_cup %>%
  select(B, A, winner) %>%
  mutate(
    winner = ifelse(winner == 0, 1, 0)
  )

world <- rbind(world_cup, world_cup2)

#After satisfaction with the results, will proceed with the full data

#32 teams
model <- glm(winner ~ A + B, world, family = binomial)

predictions <- predict(model, contest, type = "response")

contest$predictions <- ifelse(predictions >= .5, 1, 0)

contest <- contest %>%
  filter(predictions == 1)

#spliting data
half_rows <- floor(nrow(contest) / 2)
# Randomly shuffle the indices
shuffled_indices <- sample(seq_len(nrow(contest)))
indices1 <- shuffled_indices[1:half_rows]
indices2 <- shuffled_indices[(half_rows + 1):nrow(contest)]

winners1 <- contest$A[indices1]
winners2 <- contest$A[indices2]

if (length(winners1) > length(winners2)) {
  # Drop the extra elements from 'winners1'
  winners1 <- winners1[1:length(winners2)]
} else if (length(winners2) > length(winners1)) {
  # Drop the extra elements from 'winners2'
  winners2 <- winners2[1:length(winners1)]
}

contest <- data.frame(A= winners1, B= winners2)

predictions <- predict(model, contest, type = "response")
contest$predictions <- ifelse(predictions >= .5, 1, 0)
datatable(contest)

contest <- contest %>%
  filter(predictions == 1)

#spliting data
half_rows <- floor(nrow(contest) / 2)
# Randomly shuffle the indices
shuffled_indices <- sample(seq_len(nrow(contest)))
indices1 <- shuffled_indices[1:half_rows]
indices2 <- shuffled_indices[(half_rows + 1):nrow(contest)]

winners1 <- contest$A[indices1]
winners2 <- contest$A[indices2]

if (length(winners1) > length(winners2)) {
  # Drop the extra elements from 'winners1'
  winners1 <- winners1[1:length(winners2)]
} else if (length(winners2) > length(winners1)) {
  # Drop the extra elements from 'winners2'
  winners2 <- winners2[1:length(winners1)]
}

contest <- data.frame(A= winners1, B= winners2)

predictions <- predict(model, contest, type = "response")
contest$predictions <- predictions

if(nrow(contest) == 1){
  winner = ifelse(contest$predictions > 0.5, contest$A, contest$B)
} else {
  
  contest$predictions <- ifelse(predictions >= .5, 1, 0)
  datatable(contest)
  
  contest <- contest %>%
    filter(predictions == 1)
  
  #spliting data
  half_rows <- floor(nrow(contest) / 2)
  # Randomly shuffle the indices
  shuffled_indices <- sample(seq_len(nrow(contest)))
  indices1 <- shuffled_indices[1:half_rows]
  indices2 <- shuffled_indices[(half_rows + 1):nrow(contest)]
  
  winners1 <- contest$A[indices1]
  winners2 <- contest$A[indices2]
  
  if (length(winners1) > length(winners2)) {
    # Drop the extra elements from 'winners1'
    winners1 <- winners1[1:length(winners2)]
  } else if (length(winners2) > length(winners1)) {
    # Drop the extra elements from 'winners2'
    winners2 <- winners2[1:length(winners1)]
  }
  
  contest <- data.frame(A= winners1, B= winners2)
  
  predictions <- predict(model, contest, type = "response")
  contest$predictions <- predictions
  
}


cat('the winners is: ', contest$A)


