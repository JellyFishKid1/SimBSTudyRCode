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
## WORKING #####################################################################
# All participant trust state results
# Load necessary libraries
library(ggplot2)

SpecificTrustState<-read.csv(file="SpecificTrustState.csv") 
STS <- data.frame(
  turn_number=c(SpecificTrustState[, c(1)] ),
  Condition=c(SpecificTrustState[, c(2)] ),
  participantNumber=c(SpecificTrustState[, c(3)] ),
  score=c(SpecificTrustState[, c(4)] )
)

# Calculate the average scores for each condition and turn number
averages <- aggregate(score ~ turn_number + Condition, data = STS, FUN = mean)
condition_colors <- c(NoWay = "#f8766d", OneWay = "#619cff", TwoWay = "#00ba38")
# Plot the averages on a scatterplot
ggplot(averages, aes(x = turn_number, y = score, color = Condition)) +
  geom_point(size=4) +
  geom_line(linewidth=1.5) +
  scale_color_manual(values = condition_colors) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "Average Scores by Condition and Turn Number",
       x = "Turn Number",
       y = "Average Score")

##############################################################################


#legend went away

# ggplot(df, aes(x=year, y= score, group = label)) +
#   geom_line(color='lightgrey')+
#   geom_line(data = df[df$label=='CHL',], color = 'red') +
#   geom_text(data = df[df$year == 2000,],mapping =aes(x=1999.5,label=label),color='lightgrey')+
#   geom_text(data = df[df$year == 2000 & df$label=='CHL',],mapping =aes(x=1999.5,label=label),color='red')+
#   theme_bw() +
#   theme(legend.position = 'none')

# Oneway Communication Plot
ggplot(STS, aes(x=TurnNumber, y=Score, group=Condition, color=Condition)) + 
  geom_point(data=STS[STS$Condition=="OneWay",], color =  "#619cff", size=4) +
  scale_fill_manual(values = ColorsByCondition) +
  labs(title="OneWayTrust State Results", x="Turn Number", y="Score") +
  theme(plot.title = element_text(hjust=0.5)) +
  expand_limits(x=c(1, 40) )

# Twoway Communication Plot

# Averaged Plot

################################################################################
################################################################################
#Data from matlab performance metrics 
PerformMetric<-read.csv(file="PerformanceMetrics.csv") 

