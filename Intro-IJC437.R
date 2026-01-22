library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(patchwork)
library(corrplot)
library(broom)
library(tm)
library(glmnet)
library(SnowballC)
library(readr)
#loading necessary libraries, use install.packages("") if not downloaded already

setwd("~/SHEFFIELD/INTRO TO DS/COURSEWORK/musicO/musicoset_songfeatures")
#set working directory to folder containing all needed csv files downloaded from MusicOSet

song_chart <- read_tsv("song_chart.csv")
lyrics <- read_tsv("lyrics.csv")
#read_tsv used due to the nature of the csv files.

lines <- readLines("acoustic_features.csv")
lines <- gsub('^"|"$', '', lines) #removes quotation marks from each string.
lines <- gsub("\\\\t", "\t", lines) #replaces text "\t" with real tab character "\t"
writeLines(lines, "songs_clean.csv") #writes the prepared lines into new text csv file
acoustic_features <- read_delim(
  "songs_clean.csv",
  delim = "\t", #uses the created tab characters to separate between columns
  trim_ws = TRUE,      # <-- trims spaces from all columns
  show_col_types = FALSE)
#acoustic_features.csv 's format meant it had to be read in manually

#checking all dates are in the same format for the week variable of the chart data
dates <- song_chart$week
parsed_dates <- as.Date(dates, format = "%Y-%m-%d")
# Check if any failed to parse
invalid_dates <- is.na(parsed_dates) & !is.na(dates)
any(invalid_dates)
#shows FALSE so all in correct format

summary(acoustic_features)
#check that all acoustic features are within expected ranges as detailed in report

music_data <- song_chart %>%
  group_by(song_id) %>%
  summarise(
    peak_position = 101 - max(rank_score, na.rm = TRUE),
    top10_vs_bottom20 = case_when(
      peak_position <= 10 ~ 1L,
      peak_position >= 81 ~ 0L,
      TRUE ~ NA_integer_
    ),
    peak_week = week[which.min(rank_score)],
    .groups = "drop"
  )
#creates one line per song, specifying peak_position as the highest 
#(lowest value) position achieved in the chart by this song in any week.
#creates a new binary variable for songs that reached the top 10 (1) and
#songs that only reached the bottom 50.
#peak_week specifies which week the song reached its most successful position

music_data <- inner_join(music_data,acoustic_features, by = "song_id")
#joins the chart success data with acoustic features of each song

music_data_2006_10 <- music_data %>%
  filter(between(peak_week, as.Date("2006-01-01"), as.Date("2010-12-31")))
#filters to choose data in years 2006-2010

cutoff <- quantile(music_data_2006_10$danceability, 0.75, na.rm = TRUE)
#determines the third quartile of danceablility value 
#songs to classify those above it as 'dance songs'

music_data_2006_10 <- music_data_2006_10 %>%
  mutate(dance_song = if_else(danceability >= cutoff, 1L, 0L))
#creates binary dance/non-dance song variable

dance_data <- music_data_2006_10 %>%
  filter(dance_song == 1)
#filters out non-dance songs to keep only dance songs

dance_data <- dance_data %>%
  filter(top10_vs_bottom20 == 1 | top10_vs_bottom20 == 0)
#filters out songs that peaked between position 51 and 11 in the charts.

song_long <- dance_data %>%
  select(top10_vs_bottom20, duration_ms,key,mode,time_signature,acousticness,
         danceability,energy,instrumentalness,liveness,loudness,speechiness,
         valence,tempo) %>%
  pivot_longer(
    cols = c(duration_ms,key,mode,time_signature,acousticness,
             danceability,energy,instrumentalness,liveness,loudness,speechiness,
             valence,tempo),   # or cols = c(var1, var2, var3, var4)
    names_to = "variable",
    values_to = "value"
  )
#arranges dance songs and features into correct format for boxplotting

ggplot(song_long, aes(x = factor(top10_vs_bottom20), y = value, 
                      fill = factor(top10_vs_bottom20))) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Top 10 Song",
    y = "Value",
    fill = "Top 10"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3,
    color = "red"
  )+    #adds red points for mean values
  scale_fill_manual(
    values = c("0" = "gray70", "1" = "steelblue"),
    labels = c("0" = "Non-hit", "1" = "Hit")
  ) +
  theme_minimal()
#creates box plots for all acoustic features

corr_dance_data <- dance_data %>%
  select(top10_vs_bottom20, duration_ms,key,mode,time_signature,acousticness,
         danceability,energy,instrumentalness,liveness,loudness,speechiness,
         valence,tempo)
#selects all numerical valued variables
cor_matrix <- cor(corr_dance_data, use = "complete.obs", method = "pearson")
round(cor_matrix, 2)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7
  )
#plots heat map chart with colour coordination for higher/lower 
#correlation between audio features


#missing data investigation
weekly_coverage <- song_chart %>%
  group_by(week) %>%
  summarise(
    observed_100 = n(),
    expected_100 = 100,
    missing_100 = expected_100 - observed_100,
    pct_missing_100 = 100 * missing_100 / expected_100,
    
    observed_10 = sum(101 - rank_score <= 10, na.rm = TRUE),
    expected_10 = 10,
    missing_10 = expected_10 - observed_10,
    pct_missing_10 = 100 * missing_10 / expected_10,
    .groups = "drop"
  )
#groups data by week and defines a new variable scaled from 0 to 100 
#of how many missing data points there are from the overall week and 
#just from the top ten.

ggplot(weekly_coverage, aes(x = week)) +
  geom_point(aes(y = pct_missing_100, colour = "All 100"), size = 0.7) +
  geom_point(aes(y = pct_missing_10, colour = "Top 10"), size = 0.7) +
  scale_colour_manual(
    name = "Chart Range",
    values = c("All 100" = "black", "Top 10" = "red")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(
    title = "Percentage of Missing Chart Entries per Week",
    x = "Week",
    y = "Missing (%)"
  ) +
  theme_minimal()
#plots missing data for each week as a percentage of total 100 in the chart
#as well as percentage of top 10 songs missing (red)


#Year by year comparison

avg_year <- music_data %>%
  mutate(year = year(as.Date(peak_week))) %>%   # extract year
  filter(top10_vs_bottom50 == 1) %>%                    # Top 10 songs only
  group_by(year) %>%
  summarise(
    avg_duration = mean(duration_ms, na.rm = TRUE),
    avg_key = mean(key, na.rm = TRUE),
    avg_mode = mean(mode, na.rm = TRUE),
    avg_timesignature = mean(time_signature, na.rm = TRUE),
    avg_acousticness= mean(acousticness, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_energy= mean(energy, na.rm = TRUE),
    avg_instrumentalness = mean(instrumentalness, na.rm = TRUE),
    avg_liveness = mean(liveness, na.rm = TRUE),
    avg_loudness = mean(loudness, na.rm = TRUE),
    avg_speechiness = mean(speechiness, na.rm = TRUE),
    avg_valence = mean(valence, na.rm = TRUE),
    avg_tempo = mean(tempo, na.rm = TRUE),
    .groups = "drop"
  )
#Groups single that reached the top ten by year and calculates the average 
#of each audio feature

p1 <- ggplot(avg_year, aes(x = year, y = avg_duration)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Duration of Top 10 Songs",
    x = "Year",
    y = "Average Duration"
  ) +
  theme_minimal()

p2 <- ggplot(avg_year, aes(x = year, y = avg_timesignature)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Time Signature of Top 10 Songs",
    x = "Year",
    y = "Average Time Signature"
  ) +
  theme_minimal()

p3 <- ggplot(avg_year, aes(x = year, y = avg_acousticness)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Acousticness of Top 10 Songs",
    x = "Year",
    y = "Average Acousticness"
  ) +
  theme_minimal()

p4 <- ggplot(avg_year, aes(x = year, y = avg_mode)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Mode of Top 10 Songs",
    x = "Year",
    y = "Average Mode"
  ) +
  theme_minimal()

p5 <- ggplot(avg_year, aes(x = year, y = avg_danceability)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Danceability of Top 10 Songs",
    x = "Year",
    y = "Average Danceability"
  ) +
  theme_minimal()

p6 <- ggplot(avg_year, aes(x = year, y = avg_energy)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Energy of Top 10 Songs",
    x = "Year",
    y = "Average Energy"
  ) +
  theme_minimal()

p7 <- ggplot(avg_year, aes(x = year, y = avg_instrumentalness)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Instrumentalness of Top 10 Songs",
    x = "Year",
    y = "Average Mode"
  ) +
  theme_minimal()

p8 <- ggplot(avg_year, aes(x = year, y = avg_loudness)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Loudness of Top 10 Songs",
    x = "Year",
    y = "Average Loudness"
  ) +
  theme_minimal()

p9 <- ggplot(avg_year, aes(x = year, y = avg_speechiness)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Speechiness of Top 10 Songs",
    x = "Year",
    y = "Average Speechiness"
  ) +
  theme_minimal()

p10 <- ggplot(avg_year, aes(x = year, y = avg_valence)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Valence of Top 10 Songs",
    x = "Year",
    y = "Average Valence"
  ) +
  theme_minimal()
#p1 to 10 are charts displaying these averages over time

(p1 + p2 + p3)
(p4 + p5 +p6)
(p7 + p8 + p9)
(p10)
#compiling these together in groups for presentation


##Binary Classification Model

dance_data <- dance_data[sample(1:nrow(dance_data)),]
train_size = 0.7 #using 70% of data for training
dance_train <- dance_data[1:(train_size*nrow(dance_data)),]
dance_test <- dance_data[(nrow(dance_train)+1):nrow(dance_data),]
#selecting 70% of songs as training data and 30% as testing data 
#for a binary classification model

dance_model_full <- glm(
  top10_vs_bottom20 ~ speechiness+acousticness+duration_ms+energy
                      +liveness+instrumentalness+tempo+valence,
  family=binomial(link='logit'), # use a logistic regression
  data=dance_train
)
#defining generalized linear model as logistic regression model and specifying variables used

dance_model_step <- step(dance_model_full,direction = "both")
#AIC step based selection to select contributing variables of original model

summary(dance_model_step)
#coefficients and standard errors of new model

binomial_probabilities_step <- predict(
  dance_model_step,
  newdata=dance_test,
  type='response'
)
#applying model to test data

binomial_predictions_step <- ifelse(
  binomial_probabilities>0.5,
  1,
  0
)
#assigning hit prediction status for those with probabilities > 0.5

binomial_classification_error_step <- mean(
  binomial_predictions != dance_test$top10_vs_bottom20
)
#calculates number of correctly predicted hit and non hit songs

print(paste('Accuracy',1-binomial_classification_error_step))
#calculates accuracy of model on scale of 0 to 1.

confusion_matrix_dt <- table(dance_test$top10_vs_bottom20,binomial_predictions_step)

confusion_matrix_dt_df <- as.data.frame(confusion_matrix_dt)

colnames(confusion_matrix_dt_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_dt_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label")+
  theme_minimal()
#creates confusion matrix showing how many songs were guessed correctly 
#and incorrectly and in which way they were guessed

tidy_model <- tidy(dance_model_step) %>%
  mutate(
    OR = exp(estimate),                    # Odds ratios
    Lower_CI = exp(estimate - 1.96 * std.error),  # 95% CI
    Upper_CI = exp(estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, OR, Lower_CI, Upper_CI, p.value)
tidy_model <- tidy_model %>%
  mutate(
    estimate = round(estimate, 3),
    OR = round(OR, 3),
    Lower_CI = round(Lower_CI, 3),
    Upper_CI = round(Upper_CI, 3),
    p.value = round(p.value, 3)
  )
tidy_model
#shows coefficients, odds ratios, confidence intervals and p-values for regression model


#Creating new glm with addition of repeatability of lyrics

dance_lyrics <- inner_join(lyrics,dance_data,by = "song_id")

tidy_lyrics <- dance_lyrics %>%
  filter(!is.na(lyrics)) #removes NA lyrics

tidy_lyrics$lyrics <- tolower(tidy_lyrics$lyrics) #lowercasing lyrics

tidy_lyrics$lyrics <- sub("^\\[(.*)\\]$", "\\1", tidy_lyrics$lyrics) 
#removes [ and ] from start and end of each lyric set

tidy_lyrics <- tidy_lyrics %>%
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", " ")) #removes \\n from between lines

tidy_lyrics <- tidy_lyrics %>% mutate(
  lyrics = str_remove_all(lyrics, "\\[[^]]*\\]"), #removes [chorus],[verse],[intro] etc
  lyrics = str_squish(lyrics) #removes double spaces
)
tidy_lyrics <- tidy_lyrics %>%
  mutate(lyrics = str_replace_all(lyrics, "\\\\", "")) #removes \\ from within lyrics

stops <- stopwords('en') #list of stop words

tidy_lyrics$lyrics <- removeWords(tidy_lyrics$lyrics,stops) 
#removes stop words (must do before removing punctuation, ie i'll is in stopword list)

tidy_lyrics$lyrics <- removePunctuation(tidy_lyrics$lyrics) #removes punctuation

tidy_lyrics <- tidy_lyrics %>%
  mutate(
    lyrics = str_squish(lyrics)
  ) #remove double spaces again

dance_lyrics_data <- tidy_lyrics %>%
  mutate(
    words = str_split(lyrics, "\\s+"),
    total_words = lengths(words),
    unique_words = lengths(lapply(words, unique)),
    repeatability = 1 - (unique_words / total_words)
  ) %>%
  select(-words)
#separates each word to then calculate repeatability for each song as defined in study


dance_lyrics_data <- dance_lyrics_data[sample(1:nrow(dance_lyrics_data)),]
train_size = 0.7 #using 70% of data for training
dance_lyrics_train <- dance_lyrics_data[1:(train_size*nrow(dance_lyrics_data)),]
dance_lyrics_test <- dance_lyrics_data[(nrow(dance_lyrics_train)+1):nrow(dance_lyrics_data),]
#lost 10 songs through lack of lyrics data
#selecting 70% of data for training model and 30% for testing model

dance_model_lyrics <- glm(
  top10_vs_bottom20 ~ acousticness+energy
  +liveness+tempo+repeatability,
  family=binomial(link='logit'), # use a logistic regression
  data=dance_lyrics_train
)
#logistic regression model made as before, with added variable of repeatability

binomial_probabilities_lyrics <- predict(
  dance_model_lyrics,
  newdata=dance_lyrics_test,
  type='response'
)
#testing the model on test data

binomial_predictions_lyrics <- ifelse(
  binomial_probabilities_lyrics>0.5,
  1,
  0
)
#assigning hit prediction status for those with probabilities > 0.5

binomial_classification_error_lyrics <- mean(
  binomial_predictions_lyrics != dance_lyrics_test$top10_vs_bottom20
)
#calculates number of correctly predicted hit and non hit songs

print(paste('Accuracy',1-binomial_classification_error_lyrics))

#Accuracy on a scale of 0 to 1 calculated.
