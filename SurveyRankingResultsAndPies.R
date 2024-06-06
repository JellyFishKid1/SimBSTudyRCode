library(ggplot2)
library(dplyr)
OneWaySurvey<-read.csv(file="OneWaySurvey.csv") 
OneWay <- data.frame(
  Participants=c(OneWaySurvey[, c(1)] ),
  Question=c(OneWaySurvey[, c(2)] ),
  Score=c(OneWaySurvey[, c(3)] )
)
OneWay$Condition <- "OneWay"

TwoWaySurvey<-read.csv(file="TwoWaySurvey.csv") 
TwoWay <- data.frame(
  Participants=c(TwoWaySurvey[, c(1)] ),
  Question=c(TwoWaySurvey[, c(2)] ),
  Score=c(TwoWaySurvey[, c(3)] )
)
TwoWay$Condition <- "TwoWay"

# Bar Graph Displaying Rankings
intuitive <- "Intuitiveness"
easeOfUse <- "Ease"
satisfaction <- "Satisfaction"
control <- "control"
trust <- "trust"
intentAwareness <- "intent"

intuitiveOneWay <- OneWay[grep(intuitive, OneWay$Question), ]
easeOfUseOneWay <- OneWay[grep(easeOfUse, OneWay$Question), ]
satisfactionOneWay <- OneWay[grep(satisfaction, OneWay$Question), ]
trustOneWay <- OneWay[grep(trust, OneWay$Question), ]
intentAwarenessOneWay <- OneWay[grep(intentAwareness, OneWay$Question), ]

intuitiveTwoWay <- TwoWay[grep(intuitive, TwoWay$Question), ]
easeOfUseTwoWay <- TwoWay[grep(easeOfUse, TwoWay$Question), ]
satisfactionTwoWay <- TwoWay[grep(satisfaction, TwoWay$Question), ]
trustTwoWay <- TwoWay[grep(trust, TwoWay$Question), ]
intentAwarenessTwoWay <- TwoWay[grep(intentAwareness, TwoWay$Question), ]

combined_intuitive <- rbind(intuitiveOneWay, intuitiveTwoWay)
combined_intuitive <- subset(combined_intuitive, select = -Question)
combined_intuitive$Category <- "Intuitiveness"
combined_ease <- rbind(easeOfUseOneWay, easeOfUseTwoWay)
combined_ease <- subset(combined_ease, select = -Question)
combined_ease$Category <- "EaseOfUse"
combined_sat <- rbind(satisfactionOneWay, satisfactionTwoWay)
combined_sat <- subset(combined_sat, select = -Question)
combined_sat$Category <- "Satisfaction"
combined_trust <- rbind(trustOneWay, trustTwoWay)
combined_trust <- subset(combined_trust, select = -Question)
combined_trust$Category <- "Trust"
combined_awareness <- rbind(intentAwarenessOneWay, intentAwarenessTwoWay)
combined_awareness <- subset(combined_awareness, select = -Question)
combined_awareness$Category <- "AwarenessOfAutoIntent"

surveyData <- rbind(combined_intuitive, combined_ease, combined_sat, combined_trust, combined_awareness)

library(tidyverse)
library(ggpubr)
library(rstatix)

# Summary Stats
surveyData %>%
  group_by(Condition, Category) %>%
  get_summary_stats(Score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = surveyData, dv = Score, wid = Participants,
  within = c(Condition, Category)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- surveyData %>%
  group_by(Category) %>%
  pairwise_t_test(
    Score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats
pwc <- pwc %>% add_xy_position(x = "Category")
bxp <- ggboxplot(
  surveyData, x = "Category", y = "Score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Boxplot of summary stats for Condition
pwc <- pwc %>% add_xy_position(x = "Condition")
bxp <- ggboxplot(
  surveyData, x = "Condition", y = "Score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )

############################################################################
############################################################################
# Confusion and Braking Bar Charts 
############################################################################
library(ggplot2)
# Example data
confusion <- data.frame(
  condition = c("OneWay", "OneWay", "TwoWay", "TwoWay"),
  response = c("Yes", "No", "Yes", "No"),
  score = c(4, 6, 3, 7)
)
cplot <- ggplot(confusion, aes(x = condition, y = score, fill = response, label = score)) +
  geom_col() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "Number Who Reported Confusion") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_text(size = 8, position = position_stack(vjust = 0.5)) 
###########################################################################
  library(ggplot2)
  # Example data
  brake <- data.frame(
    condition = c("OneWay", "OneWay", "TwoWay", "TwoWay"),
    response = c("Yes", "No", "Yes", "No"),
    score = c(5, 5, 6, 4)
  )
  bplot <- ggplot(brake, aes(x = condition, y = score, fill = response, label = score)) +
  geom_col() +
    guides(fill = guide_legend(reverse = TRUE))+
    labs(title = "Number Who Reported Having Used the Brake") +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_text(size = 8, position = position_stack(vjust = 0.5))  
###########################################################################
  library(ggpubr)
  ggarrange(cplot, bplot, 
            labels = c("A", "B"),
            ncol = 2, nrow = 1)
###########################################################################
# Pie Charts 
  library(ggplot2)
  library(dplyr)
  
    Comfort <- data.frame(
    group=c("OneWay", "TwoWay"),
    value=c(0,10)
  )
  Comfort <- Comfort %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(Comfort$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  Comfort$percent <- scales::percent(Comfort$value / sum(Comfort$value))
  Comfort$label <- paste(Comfort$group, Comfort$percent, sep = "\n")
  # comfort piechart
  c <- ggplot(Comfort, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    geom_text(aes(y = ypos, label = label), color = "black", size=4) +
    scale_fill_brewer(palette="Set1") +
    labs(title = "Participants felt the ... most comfortable in") +
    theme(plot.title = element_text(size=15, hjust=0.5, face = "bold", line = 0.3)) +
    scale_fill_manual(values=c("#619cff", "#00ba38")) 
  ####################################
  relationship <- data.frame(
    group=c("OneWay", "TwoWay"),
    value=c(0,10)
  )
  relationship <- relationship %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(relationship$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  relationship$percent <- scales::percent(relationship$value / sum(relationship$value))
  relationship$label <- paste(relationship$group, relationship$percent, sep = "\n")
  # comfort piechart
  r <- ggplot(relationship, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    geom_text(aes(y = ypos, label = label), color = "black", size=4) +
    scale_fill_brewer(palette="Set1") +
    labs(title = "...strongest relationship with the car in") +
    theme(plot.title = element_text(size=15, hjust=0.5, face = "bold", line = 0.3)) +
    scale_fill_manual(values=c("#619cff", "#00ba38"))
  ####################################
  aware <- data.frame(
    group=c("OneWay", "TwoWay", "Both/Neither"),
    value=c(0,9, 1)
  )
  aware <- aware %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(aware$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  aware$percent <- scales::percent(aware$value / sum(aware$value))
  aware$label <- paste(aware$group, aware$percent, sep = "\n")
  # comfort piechart
  a <- ggplot(aware, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    geom_text(aes(y = ypos, label = label), color = "black", size=4) +
    scale_fill_brewer(palette="Set1") +
    labs(title = "...most aware of the automation's intentions in") +
    theme(plot.title = element_text(size=15, hjust=0.5, face = "bold", line = 0.3)) +
    scale_fill_manual(values=c("#619cff", "#00ba38", "#FFCC00"))
  ####################################
  comfortableness <- data.frame(
    group=c("OneWay", "TwoWay"),
    value=c(0,10)
  )
  comfortableness <- comfortableness %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(comfortableness$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  comfortableness$percent <- scales::percent(comfortableness$value / sum(comfortableness$value))
  comfortableness$label <- paste(comfortableness$group, comfortableness$percent, sep = "\n")
  # comfort piechart
  cn <- ggplot(comfortableness, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    geom_text(aes(y = ypos, label = label), color = "black", size=4) +
    scale_fill_brewer(palette="Set1") +
    labs(title = "...fastest to become comfortable in") +
    theme(plot.title = element_text(size=15, hjust=0.5, face = "bold", line = 0.3)) +
    scale_fill_manual(values=c("#619cff", "#00ba38"))
  ####################################
  failures <- data.frame(
    group=c("OneWay", "TwoWay", "Same"),
    value=c(4,2,4)
  )
  failures <- failures %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(failures$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  failures$percent <- scales::percent(failures$value / sum(failures$value))
  failures$label <- paste(failures$group, failures$percent, sep = "\n")
  # comfort piechart
  f <- ggplot(failures, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    geom_text(aes(y = ypos, label = label), color = "black", size=4) +
    scale_fill_brewer(palette="Set1") +
    labs(title = "... most automation failres in") +
    theme(plot.title = element_text(size=15, hjust=0.5, face = "bold", line = 0.3)) +
    scale_fill_manual(values=c("#619cff", "#FFCC00", "#00ba38"))
  ####################################
  library(ggplot2)
  library(dplyr)
  guesses<-read.csv(file="FailureGuesess.csv") 
  guesses <- data.frame(
    participantNumber=c(guesses[, c(1)] ),
    Condition=c(guesses[, c(2)] ),
    number=c(guesses[, c(3)] )
  )
  condition_colors <- c(NoWay = "#f8766d", OneWay = "#619cff", TwoWay = "#00ba38")

  guessessss <- ggplot(guesses, aes(x=Condition, y=number, fill=Condition)) + 
    geom_boxplot() +
    scale_fill_manual(values=c("#619cff", "#00ba38")) +
    labs(title = "Guesses for Number of Autiomation Failures") +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_point(position = position_jitter(width = 0.2), color = "red", size = 3)
  ###########################################################################
  library(ggpubr)
  ggarrange(c, r, a, cn, f, guessessss,
            labels = c("A", "B", "C", "D", "E", "F"),
            ncol = 2, nrow = 3)
  ###########################################################################
    
  