ColorsByCondition <- c("#f8766d", "#619cff", "#00ba38")

## WORKING #####################################################################
# boxplot for averages results with the simplified NASATLX csv
NASATLX<-read.csv(file="NASATLX.csv") 
NASA <- data.frame(
  Categories=c(NASATLX[, c(1)] ),
  Condition=c(NASATLX[, c(2)] ),
  AverageScore=c(NASATLX[, c(3)])
)

library(ggplot2)
ggplot(NASA, aes(x = Categories, y = AverageScore, fill = Condition)) + expand_limits(y=c(0, 7)) +
  geom_col(position = position_dodge()) + scale_fill_manual(values = ColorsByCondition) +
  labs(title="NASA-TLX Averaged Results", x="Category", y="Average Score")+
  theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(breaks = seq(0, 7, 1)) +
  geom_text(aes(label = AverageScore),colour = "white", size = 4, vjust = 1.5, position = position_dodge(.9))

################################################################################
# Using all the participant results to graph
Results<-read.csv(file="ParticipantResults.csv") 
allResults <- data.frame(

)

################################################################################
################################################################################
##In Progress ##################################################################
# maybe add comments section?
# add ability to hover and get values
# data analysis -> glm to consider participant trends 
library(ggplot2)
library(dplyr)
DataTrustState<-read.csv(file="DataTrustState.csv") 
DTSog <- data.frame(
  participantNumber=c(DataTrustState[, c(1)] ),
  Condition=c(DataTrustState[, c(2)] ),
  turn_number=c(DataTrustState[, c(3)] ),
  score=c(DataTrustState[, c(4)] )
)
# Remove rows with missing values in the "Score" column
DTS <- DTSog[!is.na(DTSog$score), ]
# Reorder the rows by Turn within each Condition
sorted_DTS <- DTS %>% arrange(Condition, turn_number)
# Calculate the average scores for each condition and turn number
averages <- aggregate(score ~ turn_number + Condition, data = sorted_DTS, FUN = mean)
condition_colors <- c(NoWay = "#f8766d", OneWay = "#619cff", TwoWay = "#00ba38")

## Averages scatter plot
ggplot(averages, aes(x = turn_number, y = score, color = Condition)) +
  geom_point(size=4) +
  geom_line(linewidth=1.5) +
  scale_color_manual(values = condition_colors) +
  expand_limits(x=c(1, 40),y=c(1,5)) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "Average Trust Scores by Condition and Turn Number",
       x = "Turn Number",
       y = "Average Score")

## Oneway Communication Plot
OneWay_data <- sorted_DTS[sorted_DTS$Condition == "OneWay", ]
# Plot scatter plot
blue_shades <- c("#619cff", "#4682d4", "#2f77ca", "#177cc0", "#086ea6", "#005a7a")
# Plot scatterplot with connected lines, each a different shade of blue per participant
ggplot(OneWay_data, aes(x = turn_number, y = score, color = as.factor(participantNumber))) +
  geom_point(size=4) +
  geom_line(linewidth=1.5, aes(group = participantNumber), alpha = 0.5) +
  labs(x = "Turn Number", y = "Score", title = "Scatterplot of Participant Scores with Connected Lines") +
  scale_color_manual(values = blue_shades) +
  theme_minimal()

## Twoway Communication Plot
TwoWay_data <- sorted_DTS[sorted_DTS$Condition == "TwoWay", ]
# Plot scatter plot
green_shades <- c("#00ba38", "#12c94b", "#24d45d", "#36df70", "#49ea82", "#5bf494")
# Plot scatterplot with connected lines, each a different shade of blue per participant
ggplot(TwoWay_data, aes(x = turn_number, y = score, color = as.factor(participantNumber))) +
  geom_point(size=4) +
  geom_line(linewidth=1.5, aes(group = participantNumber), alpha = 0.5) +
  labs(x = "Turn Number", y = "Score", title = "Scatterplot of Participant Scores with Connected Lines") +
  scale_color_manual(values = green_shades) +
  theme_minimal()
################################################################################
################################################################################
#Data from matlab performance metrics 
PerformMetric<-read.csv(file="PerformanceMetrics.csv") 

