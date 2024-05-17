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
# Averaged trust state results scatter plot 
AveragedTrustState<-read.csv(file="AveragedTrustState.csv") 
ATS <- data.frame(
  TurnNumber=c(AveragedTrustState[, c(1)] ),
  Condition=c(AveragedTrustState[, c(2)] ),
  AverageTS=c(AveragedTrustState[, c(3)])
)

ggplot(ATS, aes(x=TurnNumber, y=AverageTS, color=Condition)) + 
  geom_point(size=5) + 
  scale_fill_manual(values = ColorsByCondition) +
  labs(title="Trust State Averaged Results", x="Turn Number", y="Average Score") +
  theme(plot.title = element_text(hjust=0.5)) + 
  geom_rect(aes(xmin = 15, xmax = 25, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.01, linetype=0) +
  expand_limits(x=c(1, 40), y=c(0, 5))

################################################################################
# All participant trust state results
SpecificTrustState<-read.csv(file="SpecificTrustState.csv") 
STS <- data.frame(
  TurnNumber=c(SpecificTrustState[, c(1)] ),
  Condition=c(SpecificTrustState[, c(2)] ),
  Participant=c(SpecificTrustState[, c(3)] ),
  Score=c(SpecificTrustState[, c(4)] )
)

# All Communication Plot
STS$Condition = factor(STS$Condition, levels = c("NoWay", "OneWay", "TwoWay"))
STS$TurnNumber = factor(STS$TurnNumber, levels = c(1,2,3,4,5,6,7,8,9,10,
                                                  11,12,13,14,15,16,17,18,19,20, 
                                                  21,22,23,24,25,26,27,28,29,30,
                                                  31,32,33,34,35,36,37,38,39,40))
STS$Participant = factor(STS$Participant, levels = c("P01","P02","P03","P04","P05"))

ggplot(STS, aes(x=TurnNumber, y=Score, group=Condition, color=Condition)) + 
  geom_point(data=STS[STS$Condition=="NoWay",], color =  "#f8766d", size=4) +
  geom_point(data=STS[STS$Condition=="OneWay",], color =  "#619cff", size=4) +
  geom_point(data=STS[STS$Condition=="TwoWay",], color =  "#00ba38", size=4) +
  scale_fill_manual(values = ColorsByCondition) +
  labs(title="Trust State Results", x="Turn Number", y="Average Score") +
  theme(plot.title = element_text(hjust=0.5)) +
  expand_limits(x=c(1, 40) )

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

