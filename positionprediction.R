library(nbastatR)
library(dplyr)
library(randomForest)
library(vip)
library(caret)
library(ggplot2)

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
stats_total <- bref_players_stats(2015:2024)

colnames(stats_total)

stats <- stats_total %>%
  mutate(ppg = ptsTotals/countGames, apg = astTotals/countGames, rpg = trbTotals/countGames, spg = stlTotals/countGames, bpg = blkTotals/countGames, tpg = tovTotals/countGames, pfpg = pfTotals/countGames) %>%
  mutate(slugPosition = sapply(strsplit(slugPosition, "-"), function(x) x[1])) %>%
  group_by(yearSeason) %>%
  filter(countGames >= 0.5 * max(countGames)) %>%
  select(season = yearSeason, pos = slugPosition, id = slugPlayerBREF, player = namePlayer, ppg, apg, rpg, spg, bpg, tpg, pfpg, fg = pctFG, fg3 = pctFG3, ft = pctFT, ts = pctTrueShooting, per = ratioPER, ows = ratioOWS, dws = ratioDWS, obpm = ratioOBPM, dbpm = ratioDBPM, usg = pctUSG) %>%
  ungroup()

stats_train <- stats %>% filter(season <= 2021)
stats_test <- stats %>% filter(season >= 2022)

stats_train$pos <- as.factor(stats_train$pos)
stats_test$pos <- as.factor(stats_test$pos)

position_rf <- randomForest(pos ~ ppg + apg + rpg + spg + bpg + tpg + pfpg + fg + fg3 + ft + per + ows + dws + obpm + dbpm + usg, data = stats_train)

vip(position_rf)

vi(position_rf)

predictions <- predict(position_rf, newdata = stats_test)
conf_matrix <- confusionMatrix(predictions, stats_test$pos)

comparison <- data.frame(
  season = stats_test$season,
  player = stats_test$player,
  actual_pos = stats_test$pos,
  pred_pos = predictions,
  stringsAsFactors = FALSE
)

conf_matrix <- conf_matrix$table[c("PG", "SG", "SF", "PF", "C"), c("PG", "SG", "SF", "PF", "C")] 

conf_matrix <- as.data.frame(conf_matrix)
unique(predictions)
unique(stats_test$pos)

ggplot(conf_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="red") +
  labs(x = "Predicted Position", y = "Actual Position", title = "Predicted vs Actual NBA Positions For 2021/22 - 2023/24 Players", caption = "Amrit Vignesh") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) 