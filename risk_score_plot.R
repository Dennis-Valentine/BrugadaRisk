
# Background --------------------------------------------------------------

# Adding a custom plot to show how at-risk the patients are. 


# Housekeeping ------------------------------------------------------------

library(ggplot2)
library(magrittr)
library(tidyr)

# Building the plot -------------------------------------------------------

user_risk <- 30
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
base_colour <- "#DBEAE0"
#fill_col <- "#C69A60"
fill_col_range <- seq(from = 1, to = 5, by = 1)
names(fill_col_range) <- c("#4C8C4C", "#82C57B", "#CBCC85", "#DD9854", "#B22B3B")
fill_col_index <- round(quantile(fill_col_range, user_risk/44))
fill_col <- fill_col_range[fill_col_index]

ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  annotate("text", label = user_risk, size = 15, x = 0, y = 0) +
  scale_fill_manual(values = c(base_colour, names(fill_col))) +
  theme_void() +
  theme(legend.position = "none")


