
# Background --------------------------------------------------------------

# Adding a custom plot to show how at-risk the patients are. 


# Housekeeping ------------------------------------------------------------

library(ggplot2)
library(magrittr)
library(tidyr)

# Building the plot -------------------------------------------------------

user_risk <- 44
data <- data.frame(
  category=c("user_risk", "max_risk"),
  count=c(user_risk, 44)
)

# Compute percentages
data$fraction = data$count / 44
#data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = data$fraction

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  annotate("text", label = user_risk, size = 15, x = 0, y = 0) +
  theme_void()














#install.packages("gganimate")
library(gganimate)

file_renderer(dir = ".")



tmp <- t(data.frame(PARS = 12, ST1_BS_ECG = 14, ER = 9, T1BP = 9))
tmp <- as.data.frame(tmp)
colnames(tmp) <- "score"
tmp

total <- sum(18 + 12 + 14)
score <- 30

results <- data.frame(score = 1:10)
results

p <- ggplot(data = results, aes(x = 1, y = 1, label = score)) +
  geom_text() 
ani <- p +  transition_states(score,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes('cubic-in-out')

ani

ggplot(data = data.frame(), aes(x = factor(1), y = score)) +
  geom_bar(stat = "identity") +
  coord_polar() +
  ylim(c(-5,40))


ggplot(data = data.frame(), aes(xmin = 1, xmax = 2, ymin = 1, ymax = score )) +
  geom_rect() +
  coord_polar() +
  ylim(c(-3,40))
  


  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab(label = "") +
  ylab(label = "") +
  scale_y_continuous(limits = c(0,44)) +

