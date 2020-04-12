getwd()
source("http://www.sthda.com/upload/rquery_cormat.r")
library(tidyverse) 
library(dplyr)
library(corrplot)
library(ggplot2)
library(caret)

df = read.csv(file = "players_20.csv", stringsAsFactors = FALSE)
df = tbl_df(df)
df <- select(df, sofifa_id, short_name, age, nationality, overall, club, value_eur, wage_eur, player_positions,pace,passing,dribbling,defending,physic,attacking_crossing,attacking_finishing,attacking_heading_accuracy,attacking_short_passing,attacking_volleys, skill_dribbling, skill_curve, skill_fk_accuracy, skill_long_passing, skill_ball_control, movement_acceleration, movement_sprint_speed, movement_agility, movement_reactions, movement_balance, power_shot_power, power_jumping, power_stamina, power_strength, power_long_shots, mentality_aggression, mentality_interceptions, mentality_positioning, mentality_vision, mentality_penalties, mentality_composure, defending_marking, defending_standing_tackle, defending_sliding_tackle, goalkeeping_diving, goalkeeping_handling, goalkeeping_kicking, goalkeeping_positioning, goalkeeping_reflexes)


# ls, st, rs, lw, lf, cf, rf, rw, lam, cam, ram, lm, lcm, cm, rcm, rm, lwb, ldm, cdm, rdm, rwb, lb, lcb, cb, rcb, rb

players_20 <- read.csv("players_20.csv")

selected <- c("short_name", "age", "club", "nationality","overall", "potential","height_cm","weight_kg","value_eur", "wage_eur", "preferred_foot", "team_position", "nation_position", "pace", 
                 "shooting", "passing", "dribbling", "defending", "physic", "attacking_crossing", "attacking_finishing", "attacking_heading_accuracy", "attacking_short_passing",   
                 "attacking_volleys", "skill_dribbling", "skill_curve", "skill_fk_accuracy",         
                 "skill_long_passing", "skill_ball_control", "movement_acceleration", "movement_sprint_speed",     
                 "movement_agility", "movement_reactions", "movement_balance", "power_shot_power",          
                 "power_jumping", "power_stamina", "power_strength", "power_long_shots",          
                 "mentality_aggression", "mentality_interceptions", "mentality_positioning", "mentality_vision",          
                 "mentality_penalties", "mentality_composure", "defending_marking", "defending_standing_tackle", 
                 "defending_sliding_tackle")

players_20 <- players_20 %>%
  select(selected) %>%
  filter(!is.na(club), !is.na(team_position), team_position != "SUB",team_position != "GK")

players_20$team_position <- gsub("[0-9]","",players_20$team_position) 

for (val in c(2,8,10)){
  players_20[[val]] <- as.character(players_20[[val]])
  players_20[[val]] <- as.numeric(players_20[[val]])
  
}

Pos <- data.frame("team_position" = c("LB", "CB","RB", "RM", "CM", "LM", "LW", "ST", "RW"
                                     ,"CAM","CDM","CF","LAM","LCB","LCM","LDM","LF","LS","RAM","RCB","RCM","RDM"),
                 "PositionNames" = c("Left Back","Center Back","Right Back",
                                      "Attack Mid","Midfield","Attack Mid","Left Wing","Forward",
                                      "Right Wing","Attack Mid","Defense Mid","Forward","Midfield",
                                      "Center Back","Attack Mid","Defense Mid","Attack Mid","Left Wing",
                                      "Attack Mid","Center Back","Attack Mid","Defense Mid"))

players_20$BMI <- players_20$weight_kg / ((players_20$height_cm/100) ^ 2)
players_20 <- players_20[players_20$team_position %in% Pos$team_position,]
players_20 <-  merge(x=players_20,y=Pos,by="team_position",all.x = TRUE)

#1 : defender, 2: mid, 3: attack
players_20 <- players_20 %>%
    mutate(Class = case_when(PositionNames == "Left Back" ~ '1',
                             PositionNames == "Center Back" ~ '1',
                             PositionNames == "Right Back" ~ '1',
                             PositionNames == "Attack Mid" ~ '3',
                             PositionNames == "Left Wing" ~ '2',
                             PositionNames == "Forward" ~ '3',
                             PositionNames == "Right Wing" ~ '2',
                             PositionNames == "Defense Mid" ~ '1',
                             PositionNames == "Midfield" ~ '2'))


properties <- players_20 %>% select('overall', 'pace', 'attacking_crossing',
       'attacking_finishing', 'attacking_heading_accuracy', 'attacking_short_passing', 'attacking_volleys', 'skill_dribbling',
       'skill_curve', 'skill_fk_accuracy', 'skill_long_passing', 'skill_ball_control', 'movement_acceleration',
       'movement_sprint_speed', 'movement_agility', 'movement_reactions', 'movement_balance', 'power_shot_power',
       'power_jumping', 'power_stamina', 'power_strength', 'power_long_shots', 'mentality_aggression',
       'mentality_interceptions', 'mentality_positioning', 'mentality_vision', 'mentality_penalties', 'mentality_composure',
       'defending_standing_tackle', 'defending_sliding_tackle', 'team_position', 'Class') 

#matric of correlation 
#rquery.cormat(properties, type="full")

#heatmap
#rquery.cormat(properties, graphType="heatmap")

#print all the correlation
#rquery.cormat(properties, type="flatten", graph=FALSE)

#which gives us the most important colomuns related to overall

model_data <- properties %>% select(overall,mentality_composure, skill_ball_control, power_shot_power, mentality_vision, skill_dribbling, skill_curve, Class)

train_sample <- createDataPartition(properties$Class,p = 0.8,list = FALSE)
train_data   <- properties[train_sample,] %>% select(-team_position) 
test_data    <- properties[-train_sample,] %>% select(-team_position) 

#nrow(train_data)
#nrow(test_data)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid_knn <- expand.grid(.k=seq(1,80,5))
fifa_knn <- train(Class~., data=train_data, method = "knn",
                   trControl = train_control,preProcess = c("center","scale"),
                   tuneGrid = grid_knn)

fifa_knn

#plot(fifa_knn)

fifa_knn_predict <- predict(fifa_knn,newdata = test_data)
confusionMatrix(factor(fifa_knn_predict),factor(test_data$Class))

grid_svm <- expand.grid(.cost=c(0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 1.75, 2))
fifa_svm_linear <- train(Class ~., data = train_data, method = "svmLinear2",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = grid_svm)

fifa_svm_linear 

#plot(fifa_svm_linear)  

fifa_svm_linear_predict <- predict(fifa_svm_linear,newdata = test_data)
confusionMatrix(factor(fifa_svm_linear_predict),factor(test_data$Class)) 

