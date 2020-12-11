# Reference: https://www.reddit.com/r/rstats/comments/aibo72/building_an_advanced_web_scraper_for_kaggle/
library(dplyr)
library(jsonlite)
# req <- fromJSON("https://www.kaggle.com/c/9933/leaderboard.json?includeBeforeUser=true&includeAfterUser=true")
req <- fromJSON("https://www.kaggle.com/c/lish-moa/leaderboard.json?includeBeforeUser=true&includeAfterUser=true")

ranking <- bind_rows(req$beforeUser, req$afterUser) %>%
  distinct(teamId, rank, score, entries, lastSubmission)

# all
plot(1:nrow(ranking), ranking$score, type = 'l',
     main = 'LB Score vs. LB Rank',
     xlab = 'LB Rank', ylab = 'LB Score')
abline(h = 0.23398, col = 'red')



top_percent <- function(scores=score, percentage = 0.9) {
  # Take top percentage
  n <- length(scores)
  top_n <- floor(n * percentage)
  plot(1:top_n, scores[1:top_n], type = 'l',
       main = paste0('LB Score vs. LB Rank: Top ', percentage*100, '%'),
       xlab = 'LB Rank', ylab = 'LB Score')
  print(max(scores[1:top_n]))
}
N <- length(score)
# top 95%
### Baseline model
score <- as.numeric(ranking$score)
median(score); mean(score)
abline(h = median(score), col = 'red'); abline(h = mean(score), col = 'blue')
top_percent(percentage = 0.975)
# legend("bottomright",
#        legend = c("Baseline LR Model: 0.1183 (Top 95.84%)",
#                   "Baseline (Extend to 50 Classes): 0.11459 (Top 95.52%)",
#                   "Baseline (No Zeroing): 0.0721 (Top 94.1%)"),
#        col = c("red", "blue", "purple"), lty = c(1, 1, 1))
legend("bottomleft",
       # legend = c("Model 1", "Model 2", "Model 3"),
       legend = c("Model 1 (0.1183, Top 95.84%)",
                  "Model 2 (0.11459, Top 95.52%)",
                  "Model 3 (0.0721, Top 94.1%)",
                  "Model 4 (0.06500, Top 93.79%)",
                  "Model 5 (0.06492, Top 93.79%)"),
       col = c("red", "blue", "green", "red", "blue"),
       lty = c(1, 1, 1, 2, 4),
       pch=c(NA,NA,NA, 1, 2))
abline(h = 0.1183, col = 'red')
abline(h = 0.11459, col = 'blue')
abline(h = 0.0721, col = 'green')
round((sum(score < 0.1183) / N) * 100, 2)
round((sum(score < 0.11459) / N) * 100, 2)
round((sum(score < 0.0721) / N) * 100, 2)

abline(h = 0.06500, col = 'red', lty=2)
points(seq(from=1,to=N, length.out = 10), rep(0.06500, 10), col = 'red')
abline(h = 0.06492, col = 'blue', lty=4)
points(seq(from=100,to=N, length.out = 9), rep(0.06492, 9), col = 'blue', pch=2)

### Tuned model_selection
top_percent(percentage = 0.95)
abline(h = 0.06500, col = 'red', lty=2)
points(seq(from=1,to=N, length.out = 10), rep(0.06500, 10), col = 'red')
round((sum(score < 0.06500) / N) * 100, 2)
abline(h = 0.06492, col = 'blue', lty=4)
points(seq(from=100,to=N, length.out = 9), rep(0.06492, 9), col = 'blue', pch=2)
round((sum(score < 0.06492) / N) * 100, 2)

top_percent(percentage = 0.9)
# abline(h=0.1); abline(h=0.025)
abline(h = 0.01961, col = 'red')
round((sum(score < 0.01961) / N) * 100, 2)
abline(h = 0.02441, col = 'blue')
round((sum(score < 0.02441) / N) * 100, 2)
legend("left", legend = c("Model 6 (0.01961, Top 66.99%)", "Model 7 (0.02441, Top 89.28%)"),
       col = c("red", "blue"), lty = c(1, 1))
### Draw vertical lines for personal position
# top_percent(percentage = 0.80)
# top_percent(percentage = 0.7)
top_percent(percentage = 0.7)
abline(h = 0.01961, col = 'red')
abline(h = 0.01803, col = 'blue')
abline(h = 0.01823, col = 'gold')
round((sum(score < 0.01823) / N) * 100, 2)
abline(h = 0.01832, col = 'gray')
round((sum(score < 0.01832) / N) * 100, 2)
abline(h = 0.01838, col = 'brown')
round((sum(score < 0.01838) / N) * 100, 2)
legend("left",
       legend = c("Model 6 (0.01961, Top 66.99%)",
                  "First Place (0.01803, #1)",
                  "Gold Medal Cutoff (0.01823, Top 0.4%)",
                  "Silver Medal Cutoff (0.01832, Top 3.56%)",
                  "Bronze Medal Cutoff (0.01838, Top 8.84%)"),
       col = c("red", "blue", "gold", "gray", "brown"),
       lty = rep(1,5))
# abline(h = 0.01839, col = 'black', lty = 2)
# abline(h = 0.01840, col = 'blue', lty = 3)
# abline(h = 0.01842, col = 'black', lty = 4)
# abline(h = 0.0185, col = 'black', lty = 5)

top_percent(percentage = 0.5)
top_percent(percentage = 0.4)
top_percent(percentage = 0.3)
top_percent(percentage = 0.2)
top_percent(percentage = 0.1)
top_percent(percentage = 0.05)
top_percent(percentage = 0.01)

# many duplicate scores
common_30 <- head(sort(table(scores), decreasing = TRUE), 30)
# sort(as.numeric(names(common_30)), decreasing = TRUE)
common_30_df <- as.data.frame(common_30)
is.factor(common_30_df$scores)
common_30_df$scores <- varhandle::unfactor(common_30_df$scores)
common_30_df[order(common_30_df$scores), ]



