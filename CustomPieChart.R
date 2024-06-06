## Pie chart for customization 
# Load ggplot2
library(ggplot2)
library(dplyr)

# Create Data
data <- data.frame(
  group=c("Inflation", "Deflation"),
  value=c(7,3)
)
# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

data$percent <- scales::percent(data$value / sum(data$value))
data$label <- paste(data$group, data$percent, sep = "\n")

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = label), color = "black", size=6) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "Direction Indicated by ...") +
  theme(plot.title = element_text(size=18, hjust=0.5, face = "bold", line = 0.3)) +
  scale_fill_manual(values=c("#FF9966", "#FFCC66")) 

