# ## 2024 Big Data Bowl
# ### Ben Millman

### Loading Packages

library(tidyverse) 
library(nflverse)
library(ggplot2)
library(ggthemes)
library(fs)
library(hms)
library(naniar)
library(ggrepel)
library(plotly)
library(data.table)

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")


### Reading in and glancing at our data

games_df <- read.csv("/kaggle/input/nfl-big-data-bowl-2024/games.csv")
players_df <- read.csv("/kaggle/input/nfl-big-data-bowl-2024/players.csv")
plays_df <- read.csv("/kaggle/input/nfl-big-data-bowl-2024/plays.csv")
tackles_df <- read.csv("/kaggle/input/nfl-big-data-bowl-2024/tackles.csv")

tracking_files <- dir_ls(path = "/kaggle/input/nfl-big-data-bowl-2024/", 
                         regexp = "tracking_week")

full_tracking <- tracking_files %>%
  map_df(read_csv, id = "tracking_week")


head(games_df)
str(games_df)
# gameDate is chr need to make date <br>
# gameTimeEastern is chr need to make hms <br>
# homeTeamAbbr and vistior are chr need to make factor <br>


head(players_df)
str(players_df)
# height is chr need to make int <br>
# birthDate is chr need to make date <br>
# collegeName is chr need to make factor <br>
# position is chr need to make factor <br>
# displayname is chr need to make factor


head(plays_df)
str(plays_df)
# team columns are chr need to be factor <br>
# can make gameClock int <br>
# passResult needs to be factor <br>
# penalty columns need to be factor <br>
# formation needs to be factor


head(tackles_df)
str(tackles_df)
# All good!


head(full_tracking)
str(full_tracking)
length(unique(full_tracking$displayName))
# displayName needs to be factor <br>
# club needs to be factor <br>
# event needs to be factor


### Cleaning


# Let's start with games_df
clean_games <- games_df %>%
  mutate(gameDate = mdy(gameDate)) %>%
  mutate(gameTimeEastern = as_hms(gameTimeEastern)) %>%
  mutate(across(.cols = c(homeTeamAbbr, visitorTeamAbbr), as.factor))

str(clean_games)


# Now Players
clean_players <- players_df %>%
  mutate(height = sapply(strsplit(players_df$height,"-"),
                         function(x){12*as.numeric(x[1]) + as.numeric(x[2])})) %>%
  mutate(birthDate = ymd(birthDate)) %>%
  mutate(across(.cols = c(collegeName, position, displayName), as.factor))
str(clean_players)
    # Seems to be many players without listed birthdays and ~5 with different formats,
    # we may not even use this column in our analysis so we'll wait to clean further
    
    
# Now plays
clean_plays <- plays_df %>%
      mutate(gameClock = gsub(":", "", gameClock)) %>%
      mutate(gameClock = as.numeric(gameClock)) %>%
      mutate(across(.cols = c("possessionTeam", "defensiveTeam", "yardlineSide", "passResult",
                              "playNullifiedByPenalty", "offenseFormation", "foulName1", "foulName2",
                              "foulNFLId1", "foulNFLId2", "ballCarrierDisplayName"), as.factor))
str(clean_plays)
    
    
# Now Tracking
clean_tracking <- full_tracking %>%
      mutate(tracking_week = str_replace(tracking_week,"/kaggle/input/nfl-big-data-bowl-2024/tracking_week_", "")) %>%
      mutate(tracking_week = str_replace(tracking_week, ".csv", "")) %>%
      mutate(across(.cols = c("displayName", "club", "event", "tracking_week"), as.factor))
str(clean_tracking)
    
    
# Types are now correct 
    
    
    
    
    
    
### Tackling Metrics
# First I'd like to calculate distance to ball carrier for each player each frame.  
#I then create a logistic regression predicting tackle using distance, player name, and their interaction.
    
    
### Tackle Range
distance <- function(x1, y1, x2, y2) {
      sqrt((x1 - x2)^2 + (y1 - y2)^2)
    }
    
tracking_dist <- clean_tracking %>%
      left_join(clean_plays %>% dplyr::select(gameId, playId, ballCarrierId), by = c("gameId", "playId")) %>%
      group_by(gameId, playId, frameId) %>%
      mutate(ballCarrier = ifelse(ballCarrierId == nflId, 1, 0)) %>%
      mutate(
        ballCarrierX = ifelse(ballCarrier == 1, first(x), x[which(ballCarrier == 1)]),
        ballCarrierY = ifelse(ballCarrier == 1, first(y), y[which(ballCarrier == 1)])
      ) %>%
      mutate(
        distance_to_football = ifelse(ballCarrier == 1, 0, distance(x, y, ballCarrierX, ballCarrierY))
      )
    
c <- tackles_df %>%
      dplyr::select(gameId, playId, nflId, tackle)
    
tackle_tracking <- tracking_dist %>%
      semi_join(tackles_df, by = c("nflId", "gameId", "playId")) %>%
      left_join(c, by = c("nflId", "gameId", "playId")) %>%
      filter(!(event == "tackle" & tackle == 1 & distance_to_football > 3))
    
    
top_tacklers <- tackle_tracking %>%
      group_by(nflId) %>%
      summarize(tackle_count = sum(tackle)) %>%
      arrange(desc(tackle_count)) %>%
      top_n(600) 
    
left_out <- tackle_tracking %>%
      group_by(nflId) %>%
      summarize(tackle_count = sum(tackle)) %>%
      arrange(tackle_count) %>%
      top_n(200)
    
    
top_tackle_tracking <- tackle_tracking %>%
      filter(nflId %in% top_tacklers$nflId) %>%
      arrange(gameId, playId, nflId, frameId) %>%
      group_by(gameId, playId, nflId)
    
    
length(top_tackle_tracking$nflId)
    
    
#install.packages("biglm")
#install.packages("speedglm")
library(biglm)
library(speedglm)
    
top_tackle_curves <- speedglm(tackle ~ distance_to_football + displayName + distance_to_football*displayName
                                  , family = binomial(link = logit), 
                                  data = top_tackle_tracking,
                                  trace = TRUE)
    
saveRDS(top_tackle_curves, "tackle_curves.rds")
top_tackle_curves <- readRDS(file = "/kaggle/input/tackle-curves/tackle_curves (1).rds")
    
    
top_tacklers <- top_tackle_tracking %>%
      ungroup() %>%
      distinct(displayName) 
    
test.df <- expand.grid(displayName = top_tacklers$displayName, distance_to_football = 0:62)
test.df$pred <- predict(top_tackle_curves, test.df, type = "response")
    
    
p <- ggplot(data = test.df)+
      geom_line(aes(x = distance_to_football,
                    y = pred, 
                    group = displayName,
                    alpha = .3)) 
ggplotly(p, render = "iframe")
    
    
p <- tackle_tracking %>%
      dplyr::select(displayName, tackle, event) %>%
      group_by(displayName) %>%
      filter(event == "tackle") %>%
      summarize(totalOpps = n(),
                tacklesMade = sum(event == "tackle" & tackle == 1),
                tacklesMissed = sum(event == "tackle" & tackle == 0))
    
test.df <- test.df %>%
      left_join(p, by = "displayName")
    
test.df %>%
      summarize(mean = median(totalOpps))
    
top_range <- test.df %>%
      group_by(displayName) %>%
      filter(pred > .5) %>%
      summarize(range = max(distance_to_football)) %>%
      left_join(p, by = "displayName") %>%
      arrange(desc(range), desc(totalOpps)) %>%
      filter(totalOpps > 10) %>%
      top_n(10, range)
    
    
### Visualizing the Top Tacklers by Range
    
    
full_players <- load_rosters(2022)
    
best_at_0 <- test.df %>%
      filter(distance_to_football == 0 & totalOpps >10) %>%
      arrange(desc(pred)) %>%
      top_n(10, pred)
    
heads <- full_players %>%
      filter(full_name %in% best_at_0$displayName)
    
Tackle_perct_at_0 <- best_at_0 %>%
      merge(heads, by.x = "displayName", by.y = "full_name")
    
write.csv(Tackle_perct_at_0, "perctat0viz.csv")
    
top15at0 <- ggplot(data = Tackle_perct_at_0,
                       aes(x = reorder(gsis_id, pred),
                           y = pred,
                           color = team, 
                           label = displayName
                       ))+
      geom_point(size = 3) +
      geom_text_repel(nudge_x = .35, nudge_y = .01, color = "black", fontface = "bold") +
      geom_text_repel(aes(label = round(pred, digits = 2)), nudge_x = 0, nudge_y = .005, fontface = "bold", color = "black") +
      scale_x_discrete(expand = expansion(add = c(1,1))) +
      scale_color_nfl(type = "secondary") +
      theme_fivethirtyeight() +
      theme(axis.text.y = element_nfl_headshot(size = 1.8)) +
      coord_flip() +
      labs(title = "Predicted Tackle Probability at 0 yards",
           caption = "Ben Millman | NFL Big Data Bowl 2024")
    
top15at0
    
ggsave(plot = top15at0,
           "top15at0.jpg",
           dpi = 300)
    
    
range_plt <- top_range %>%
      merge(full_players, by.x = "displayName", by.y = "full_name")
    
range_plot <- ggplot(data = range_plt,
                         aes(x = reorder(gsis_id, range),
                             y = range,
                             color = team,
                             label = displayName)) +
      geom_point(size = 3) +
      geom_text_repel(nudge_x = .35, nudge_y = .1, color = "black", fontface = "bold") +
      geom_text_repel(aes(label = range), nudge_x = 0, nudge_y = .05, fontface = "bold", color = "black") +
      scale_color_nfl(type = "secondary") +
      theme_fivethirtyeight() +
      theme(axis.text.y = element_nfl_headshot(size = 1.3)) +
      coord_flip() +
      labs(title = "Top Players by Tackle Range",
           caption = "Ben Millman | NFL Big Data Bowl 2024")
    
range_plot
    
ggsave(plot = range_plot,
           "range_plot.jpg",
           dpi = 300)
    
    
### Tackle Value (yards saved)
    
    
# Create vector of labels
tackle_locations <- tackle_tracking %>%
      filter(event == "tackle" & tackle == 1) %>%
      dplyr::select(gameId, playId, frameId, ballCarrierX, ballCarrierY) 
    
    
wide_tracking <- clean_tracking %>%
      semi_join(tackle_locations, by = c("gameId", "playId")) %>%
      group_by(gameId, playId, frameId) %>%
      arrange(nflId) %>%
      mutate(player_number = row_number()) %>%
      ungroup() %>%
      dplyr::select(gameId, playId, frameId, x, y, s, a, dis, o, dir, player_number) %>%
      pivot_wider(names_from = player_number, values_from = c(x, y, s, a, dis, o, dir))
    
    
# Sort input data
wide_tracking <- wide_tracking[order(
      wide_tracking$gameId, 
      wide_tracking$playId, 
      wide_tracking$frameId),]
    
# Sort labels
tackle_locations <- tackle_locations[order(
      tackle_locations$gameId,
      tackle_locations$playId),]
    
  
#### Creating the input array and labels
dt <- as.data.table(wide_tracking)
  
feature_columns <- setdiff(names(dt), c("gameId", "playId", "frameId"))

dt_grouped <- dt[, .(all_features = list(as.matrix(.SD))), by = .(gameId, playId), .SDcols = feature_columns]

# Get the number of features per frame
number_of_features <- ncol(wide_tracking %>% dplyr::select(-gameId, -playId, -frameId))

# Initialize the 3D array with NAs for padding
number_of_plays <- nrow(dt_grouped)
input_array <- array(NA, dim = c(number_of_plays, 164, number_of_features))

# Populate the array with data
for (i in 1:number_of_plays) {
  play_length <- nrow(dt_grouped$all_features[[i]])
  input_array[i, 1:play_length, ] <- dt_grouped$all_features[[i]]
  # The rest will remain as NA
}

#Remove labels from labels
labels_matrix <- as.matrix(tackle_locations[, -c(1:3)])

dim(labels_matrix)
dim(input_array)


#### Splitting 
total_plays <- dim(input_array)[1]  # Total number of plays
train_ratio <- 0.7  # 70% for training

train_idx <- round(total_plays * train_ratio)

train_data <- input_array[1:train_idx, , ]
train_labels <- labels_matrix[1:train_idx, ]

test_data <- input_array[(train_idx + 1):total_plays, , ]
test_labels <- labels_matrix[(train_idx + 1): total_plays, ]


dim(train_data)
dim(train_labels)
dim(test_data)
dim(test_labels)

train_data[is.na(train_data)] <- 999
test_data[is.na(test_data)] <- 999

dim(train_data)


#### Model Time!
library(keras)

model <- keras_model_sequential() %>%
  layer_masking(mask_value = 999, input_shape = c(164, number_of_features)) %>%
  bidirectional(layer_lstm(units = 75, return_sequences = TRUE), input_shape = c(164, number_of_features)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = 50, return_sequences = FALSE) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'linear')

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c('mae')
)


model %>% fit(
  x = train_data, 
  y = train_labels, 
  epochs = 120,  
  batch_size = 64,  
  validation_split = 0.2,  
  callbacks = list(callback_early_stopping(patience = 5))  
)
model %>% save_model_hdf5("nn_model1.h5")

summary(model)


predictions <- model(test_data)
true_values <- test_labels  # Your actual labels

# Calculate MSE and MAE
mse <- mean((predictions - true_values)^2)
mae <- mean(abs(predictions - true_values))

print(paste("Mean Squared Error: ", mse))
print(paste("Mean Absolute Error: ", mae))

# Visual inspection
# Convert TensorFlow tensors to R arrays
true_values_array <- as.array(true_values)
predictions_array <- as.array(predictions)


true_x <- true_values_array[, 1]
true_y <- true_values_array[, 2]
pred_x <- predictions_array[, 1]
pred_y <- predictions_array[, 2]

final_df <- data.frame(true_x, true_y, pred_x, pred_y)

gg <- ggplot() +
  geom_point(data = final_df, 
             aes(x = true_x, y = true_y), 
             color = 'blue', 
             size = 2, 
             alpha = 0.6) +
  geom_point(data = final_df, 
             aes(x = pred_x, y = pred_y), 
             color = 'red', 
             size = 2, 
             alpha = 0.6) +
  labs(x = 'X Coordinate', y = 'Y Coordinate', title = 'True vs Predicted Coordinates') +
  theme_minimal()

print(gg)


test_keys <- wide_tracking %>%
  distinct(gameId, playId) %>%
  tail(2306) 

#Remove the player who made the tackle on each of the test plays
tracking_cols <- c("x", "y", "s", "a", "dis", "o", "dir")

tackle_tracking <- tackle_tracking[order(
  tackle_tracking$gameId, 
  tackle_tracking$playId, 
  tackle_tracking$frameId),]

test_tacklers <- tackle_tracking %>%
  filter(event == "tackle" & tackle == 1) %>%
  dplyr::select(nflId) %>%
  tail(2306)

joined <- clean_tracking %>% 
  left_join(test_tacklers, by = c("gameId", "playId")) %>%
  mutate(across(all_of(tracking_cols), ~ifelse(nflId.x == nflId.y & !is.na(nflId.y), 999, .)))

wide_tracking2 <- joined %>%
  group_by(gameId, playId, frameId.x) %>%
  arrange(nflId.x) %>%
  mutate(player_number = row_number()) %>%
  ungroup() %>%
  dplyr::select(gameId, playId, frameId.x, x, y, s, a, dis, o, dir, player_number) %>%
  pivot_wider(names_from = player_number, values_from = c(x, y, s, a, dis, o, dir))


# Sort input data
wide_tracking2 <- wide_tracking2[order(
      wide_tracking2$gameId, 
      wide_tracking2$playId, 
      wide_tracking2$frameId.x),]
    
    
wide_tracking2[is.na(wide_tracking2)] <- 999
    
new_test_tracking <-wide_tracking2 %>%
      semi_join(test_keys, by = c("gameId", "playId"))
    
dt_new <- as.data.table(new_test_tracking)
    
feature_columns <- setdiff(names(dt_new), c("gameId", "playId", "frameId.x"))
    
dt_new_grouped <- dt_new[, .(all_features = list(as.matrix(.SD))), by = .(gameId, playId), .SDcols = feature_columns]
    
# Determine the maximum number of frames for padding
new_max_frames <- max(sapply(dt_new_grouped$all_features, nrow))
    
# Get the number of features per frame
new_number_of_features <- ncol(new_test_tracking %>% dplyr::select(-gameId, -playId, -frameId.x))
    
# Initialize the 3D array with NAs for padding
new_number_of_plays <- nrow(dt_new_grouped)
new_input_array <- array(NA, dim = c(new_number_of_plays, 164, new_number_of_features))
    
# Populate the array with data
for (i in 1:new_number_of_plays) {
      new_play_length <- nrow(dt_new_grouped$all_features[[i]])
      new_input_array[i, 1:new_play_length, ] <- dt_new_grouped$all_features[[i]]
      # The rest will remain as NA, which you can later convert to zeros or another padding value
    }
new_input_array[is.na(new_input_array)] <- 999
    
dim(new_input_array)
    
#re predict tackle location
new_predictions <- model(new_input_array)
    
    
#new tackle location - old tackle location = yards saved
new_pred_array <- as.array(new_predictions)
    
new_pred_x <- new_pred_array[, 1]
new_pred_y <- new_pred_array[, 2]
    
tackle_values <- data.frame(true_x, true_y, pred_x, pred_y, new_pred_x, new_pred_y)
    
tackle_values <- tackle_values %>%
      mutate(yards_saved = abs(new_pred_x - true_x))
    
directions <- clean_tracking %>%
      group_by(gameId, playId) %>%
      dplyr::select(gameId, playId, playDirection) %>%
      summarise(playDirection = first(playDirection))
    
    
joined_values <- tackle_locations %>%
      tail(2306) %>%
      left_join(tackle_values, join_by(ballCarrierX == true_x, ballCarrierY == true_y)) %>%
      left_join(tackles_df, by = c("gameId", "playId")) %>%
      left_join(directions, by = c("gameId", "playId"))
    
    
avg_yards_saved <- joined_values %>%
      group_by(nflId) %>% 
      summarize(avg_yards_saved = mean(yards_saved),
                tackles = n()) %>%
      arrange(desc(avg_yards_saved))
    
mvt <- joined_values %>% 
      filter((ballCarrierX < 50 & playDirection == "right") | (ballCarrierX > 50 & playDirection =="left")) %>%
      arrange(desc(yards_saved)) 
    
    
heads3 <- full_players %>%
      filter(gsis_it_id %in% avg_yards_saved$nflId)
    
top_yards_saved <- avg_yards_saved %>%
      filter(tackles >= 10) %>%
      arrange(desc(avg_yards_saved)) %>%
      top_n(10,avg_yards_saved) %>%
      merge(heads3, by.x = "nflId", by.y = "gsis_it_id")
    
top_yards_saved_viz <- ggplot(data = top_yards_saved,
                                  aes(x = reorder(gsis_id, avg_yards_saved),
                                      y = avg_yards_saved,
                                      color = team, 
                                      label = full_name
                                  ))+
      geom_point(size = 3) +
      geom_text_repel(nudge_x = .35, nudge_y = .01, color = "black", fontface = "bold") +
      geom_text_repel(aes(label = round(avg_yards_saved, digits = 2)), nudge_x = 0, nudge_y = .03, fontface = "bold", color = "black") +
      scale_x_discrete(expand = expansion(add = c(1,1))) +
      scale_color_nfl(type = "secondary") +
      theme_fivethirtyeight() +
      theme(axis.text.y = element_nfl_headshot(size = 1.8)) +
      coord_flip() +
      labs(title = "Top 10 by Average Yards Saved",
           caption = "Ben Millman | NFL Big Data Bowl 2024")
    
top_yards_saved_viz
    
ggsave(plot = top_yards_saved_viz,
           "top_yards_saved.jpg",
           dpi = 300)
    
    
tracking_dist %>%
      filter(gameId == 2022102309 & playId == 3262 & event == "tackle")


### Animation
source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
#install.packages("gganimate")
#install.packages("png")
#install.packages("gifski")


library(gganimate)
library(png)
library(gifski)

mycolors <-
  c(
    "football" = "#492E22",
    "MIN" = "purple",
    "WAS" = "maroon"
  )
mysize <- c("football" = 3,
            "MIN" = 4,
            "WAS" = 4)
myalpha <- c("football" = 1,
             "MIN" = 0.7,
             "WAS" = 0.7)

data_ani_2 <- clean_tracking %>%
  select(gameId, playId, time, displayName, x, y, club, frameId) %>%
  filter(gameId == 2022110607,
         playId == 2536)  %>%
  mutate(to_label = case_when(displayName == "Brian Robinson" ~ "Brian Robinson",
                              displayName == "Akayleb Evans" ~ "Akayleb Evans",        
                              TRUE ~ " "))
plt_2 <-
  ggplot(data = data_ani_2,
         aes(
           x = x,
           y = y,
           color = club,
           size = club,
           alpha = club,
           label = to_label
         )) +
  gg_field() +
  geom_point(show.legend = FALSE) +
  scale_colour_manual(values = mycolors) +
  scale_size_manual(values = mysize) +
  scale_alpha_manual(values = myalpha) +
  geom_text(
    data = data_ani_2,
    aes(x = x, y = y) ,
    size = 4,
    color = "black",
    show.legend = FALSE,
    fontface = "bold",
    vjust = -0.5
  ) +
  transition_time(time) 


anim_save(
  'MVT2.gif',
  animate(
    plt_2,
    width = 840,
    height = 374,
    fps = 20,
    duration = 15,
    end_pause = 60, 
    start_pause = 30
  )
)


tt <- clean_tracking %>%
  select(gameId, playId, time, displayName, x, y, club, frameId) %>%
  filter(gameId == 2022102400,
         playId == 1351)

