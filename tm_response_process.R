
# Load Data and Packages --------------------------------------------------

### Load necessary packages:

library(tidyverse)


### Load test responses from the trained models:

setwd("Desktop/Statistics and Machine Learning/Text Mining/Final Project")
games <- read.csv("game_responses.csv")
scores <- read.csv("score_responses.csv")


# Extract Information from the Model Responses ----------------------------

### Build helper function to parse information from games:

game_info <- function(strings){
  n <- length(strings)
  winners <- rep("", n)
  scores <- rep("", n)
  
  for (i in 1:n){
    string <- str_remove_all(strings[i], "[.]")
    string <- str_remove_all(string, "The ")
    winners[i] <- str_remove(string, " won.*")
    scores[i] <- str_remove(string, ".*was ")
  }
  
  return(list(winners, scores))
}


### Build helper function to return the question asked in scores:

score_questions <- function(strings){
  n <- length(strings)
  questions <- rep("", n)
  
  for (i in 1:n){
    questions[i] <- str_remove(strings[i], ".*question: ")
  }
  
  return(questions)
}


### Return and information from model responses:

# True game responses
real_info <- game_info(games$score)
real_winner <- real_info[[1]]
real_score <- real_info[[2]]

# Generated game responses
pred_info <- game_info(games$response)
pred_winner <- pred_info[[1]]
pred_score <- pred_info[[2]]

# Score questions
score_qs <- score_questions(scores$document)
scores$questions <- score_qs


### Build data frames for final analysis:

game_df <- data.frame(responses$game_id, responses$score, responses$response, 
                      pred_winner, real_winner, pred_score, real_score) %>% 
  rename(game_id = responses.game_id,
         score = responses.score,
         response = responses.response)

score_df <- scores %>% 
  select(game_id, questions, response, runs)


### Export data frames for analysis:

write.csv(game_df, "game_analysis.csv")
write.csv(score_df, "score_analysis.csv")
