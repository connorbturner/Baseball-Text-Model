
# Load Data and Packages --------------------------------------------------

### Load necessary packages:

library(tidyverse)

### Load regular-season pitch-by-pitch data from 2018-2023 (2020 excluded):

setwd("Desktop/")
pitch_data <- read.csv("pitch_data.csv")


# Build Data to Create Documents ------------------------------------------

### Filter the data for all games:

game_data <- pitch_data %>% 
  
  ### Remove unused columns and years from the data:
  
  select(game_pk, game_date, game_year, home_team, away_team, inning_topbot, 
         inning, at_bat_number, pitch_number, outs_when_up, events, 
         description, des, post_away_score, post_home_score) %>% 
  filter(game_year >= 2021) %>% 
  
  
  ### Remove all but the last pitch in each at-bat:
  
  arrange(game_pk, at_bat_number, pitch_number) %>% 
  group_by(game_pk, at_bat_number) %>% 
  filter(pitch_number == n()) %>% 
  ungroup %>% 
  
  
  ### Rename columns and format final dataset:
  
  rename(game_id = game_pk,
         home = home_team, 
         away = away_team,
         inn = inning,
         at_bat = at_bat_number,
         outs = outs_when_up, 
         home_score = post_home_score,
         away_score = post_away_score) %>% 
  mutate(inning = paste(inning_topbot, inn)) %>% 
  select(game_id, game_date, away, home, inning, at_bat, outs, events, 
         des, away_score, home_score) %>% 
  
  
  ### Filter out games with unexplained score changes:
  
  filter(away_score != lag(away_score) | home_score != lag(home_score)) %>% 
  filter(!(away_score == 0 & home_score == 0)) %>% 
  group_by(game_id) %>% 
  filter(all(grepl("homers | home run | scores", des))) %>% 
  ungroup()


# Generate Documents with Game Information --------------------------------

### Map team abbreviations to team names:

team_map <- c("AZ" = "Arizona Diamondbacks", "ATL" = "Atlanta Braves", 
              "BAL" = "Baltimore Orioles", "BOS" = "Boston Red Sox", 
              "CIN" = "Cincinnati Reds", "CLE" = "Cleveland Guardians",
              "CHC" = "Chicago Cubs", "COL" = "Colorado Rockies", 
              "CWS" = "Chicago White Sox", "DET" = "Detroit Tigers",
              "HOU" = "Houston Astros", "KC" = "Kansas City Royals",
              "LAA" = "Los Angeles Angels", "LAD" = "Los Angeles Dodgers", 
              "MIA" = "Miami Marlins", "MIL" = "Milwaukee Brewers", 
              "MIN" = "Minnesota Twins", "NYM" = "New York Mets", 
              "NYY" = "New York Yankees", "OAK" = "Oakland Athletics", 
              "PHI" = "Philadelphia Phillies", "PIT" = "Pittsburgh Pirates",
              "SD" = "San Diego Padres", "SEA" = "Seattle Mariners",
              "SF" = "San Francisco Giants", "STL" = "St Louis Cardinals",
              "TB" = "Tampa Bay Rays", "TEX" = "Texas Rangers", 
              "TOR" = "Toronto Blue Jays", "WSH" = "Washington Nationals")

name_map <- c("AZ" = "Diamondbacks", "ATL" = "Braves", 
              "BAL" = "Orioles", "BOS" = "Red Sox", 
              "CIN" = "Reds", "CLE" = "Guardians",
              "CHC" = "Cubs", "COL" = "Rockies", 
              "CWS" = "White Sox", "DET" = "Tigers",
              "HOU" = "Astros", "KC" = "Royals",
              "LAA" = "Angels", "LAD" = "Dodgers", 
              "MIA" = "Marlins", "MIL" = "Brewers", 
              "MIN" = "Twins", "NYM" = "Mets", 
              "NYY" = "Yankees", "OAK" = "Athletics", 
              "PHI" = "Phillies", "PIT" = "Pirates",
              "SD" = "Padres", "SEA" = "Mariners",
              "SF" = "Giants", "STL" = "Cardinals",
              "TB" = "Rays", "TEX" = "Rangers", 
              "TOR" = "Blue Jays", "WSH" = "Nationals")


### Define function to build documents:

game_doc_maker <- function(game_info){
  
  ### Load game information:
  
  df <- game_info
  home_team = team_map[unique(df$home)]
  away_team = team_map[unique(df$away)]
  home_name = name_map[unique(df$home)]
  away_name = name_map[unique(df$away)]
  
  
  ### Build event strings:
  
  df$des <- str_replace_all(df$des, "\\. ", ".")
  df$des <- str_replace_all(df$des, "  ", " ")
  df$des <- str_replace_all(df$des, "[.]", ";")
  df$des <- str_replace_all(df$des, ";$", "")
  df$des <- str_replace_all(df$des, "  ", " ")
  df$des <- str_replace_all(df$des, "\\([^)]+\\) ", "")
  df$des <- str_replace_all(df$des, "Jr;", "Jr")
  df$des <- str_replace_all(df$des, "A;", "A")
  df$des <- str_replace_all(df$des, "B;", "B")
  df$des <- str_replace_all(df$des, "C;", "C")
  df$des <- str_replace_all(df$des, "D;", "D")
  df$des <- str_replace_all(df$des, "J;", "J")
  df$des <- str_replace_all(df$des, "P;", "P")
  df$des <- str_replace_all(df$des, "R;", "R")
  df$des <- str_replace_all(df$des, "T;", "T")
  
  ### Prepare prompts:
  
  prefix <- "In baseball, a team scores a run when a player scores or homers 
  while their team is hitting. The team that scores the most runs wins the 
  game. The following is a description of all the scoring plays from a baseball 
  game: "
  prefix <- str_remove_all(prefix, "\\\n  ")
  suffix <- "Please answer the following question: "
  
  
  ### Build document:
  
  doc <- paste0(prefix, away_team, " at ", home_team, ".")
  
  for (inning in unique(df$inning)){
    
    events <-  df$des[df$inning == inning]
    
    if (grepl("Top", inning)){
      inn_string <- paste0(away_name, " hitting")
    } else {
      inn_string <- paste0(home_name, " hitting")
    }
    
    for (event in events){
      inn_string <- paste0(inn_string, "; ", event)
    }
    
    doc <- paste0(doc, " ", inn_string, ".")
  }
  
  doc <- paste0(doc, " ", suffix)
  
  
  ### Build score strings:
  
  final <- df[df$at_bat == max(df$at_bat),]
  away_score <- final$away_score[1]
  home_score <- final$home_score[1]
  
  if (final$away_score[1] > final$home_score[1]){
    score <- paste0("The ", away_team, " won. ", 
                    "The final score was ", away_name, " ", away_score, ", ", 
                    home_name, " ", home_score, ".")
  } else {
    score <- paste0("The ", home_team, " won. ", 
                    "The final score was ", away_name, " ", away_score, ", ", 
                    home_name, " ", home_score, ".")
  }
  
  question <- paste0("Which team won this game and how many runs",
                     " did each team score?")
  away_question <- paste0("How many runs did the ", away_name, 
                          " score in this game?")
  home_question <- paste0("How many runs did the ", home_name, 
                          " score in this game?")
  away_runs <- paste0("The ", away_name, " scored ", away_score, " runs.")
  home_runs <- paste0("The ", home_name, " scored ", home_score, " runs.")
  
  return(list(doc, score, question, away_question, home_question, away_score,
              home_score))
}


### Initialize data frames to hold documents:

game_id <- unique(game_data$game_id)
game_docs <- data.frame(game_id)
runs_docs <- data.frame(game_id)
game_docs$document <- NA
game_docs$score <- NA
runs_docs$document <- NA
runs_docs$runs <- NA



### Create and store documents for each game:

for (i in 1:length(game_docs$game_id)){
  id = game_docs$game_id[i]
  game <- game_doc_maker(game_data[game_data$game_id == id,])
  game_docs$document[i] <- paste0(game[[1]][1], game[[3]][1])
  game_docs$score[i] <- game[[2]][1]
  
  if (i %% 2 == 0){
    runs_docs$document[i] <- paste0(game[[1]][1], game[[4]][1])
    runs_docs$runs[i] <- game[[6]][1]
  } else {
    runs_docs$document[i] <- paste0(game[[1]][1], game[[5]][1])
    runs_docs$runs[i] <- game[[7]][1]
  }
}


### Split data randomly into training and testing sets:

n <- length(game_id)
set.seed(12345)
idx <- sample(1:n, 5221)
train_games <- game_id[idx]
test_games <- game_id[-idx]
train_game_docs <- game_docs[(game_id %in% train_games),]
test_game_docs <- game_docs[(game_id %in% test_games),]
train_runs_docs <- runs_docs[(game_id %in% train_games),]
test_runs_docs <- runs_docs[(game_id %in% test_games),]


### Export documents for analysis:

write.csv(train_game_docs, "train_game_docs.csv")
write.csv(test_game_docs, "test_game_docs.csv")
write.csv(train_runs_docs, "train_runs_docs.csv")
write.csv(test_runs_docs, "test_runs_docs.csv")
