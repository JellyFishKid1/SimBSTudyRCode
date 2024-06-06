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
##################################################################################
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
  annotate("rect", xmin = 14, xmax = 25, ymin = -Inf, ymax = Inf, alpha = .45,fill = "grey") + 
  xlim(1, 40) + ylim(1, 5) +
  labs(title = "Average Trust Scores by Condition and Turn Number", 
       x = "Turn Number",
       y = "Average Score")
##################################################################################
## Oneway Communication Plot
OneWay_data <- sorted_DTS[sorted_DTS$Condition == "OneWay", ]
# Plot scatter plot
blue_shades <- c("#619cff", "#4682d4", "#2f77ca", "#177cc0", "#086ea6", "#005a7a", "#619cff99")
# Plot scatterplot with connected lines, each a different shade of blue per participant
ggplot(OneWay_data, aes(x = turn_number, y = score, color = as.factor(participantNumber))) +
  geom_point(size=4) +
  geom_line(linewidth=1.5, aes(group = participantNumber), alpha = 0.5) +
  labs(x = "Turn Number", y = "Score", title = "Scatterplot of Participant Scores with Connected Lines") +
  scale_color_manual(values = blue_shades) +
  annotate("rect", xmin = 14, xmax = 25, ymin = -Inf, ymax = Inf, alpha = .45,fill = "grey") + 
  xlim(1, 40) + ylim(1, 5) +
  theme_minimal()
## Twoway Communication Plot
TwoWay_data <- sorted_DTS[sorted_DTS$Condition == "TwoWay", ]
# Plot scatter plot
green_shades <- c("#00ba38", "#12c94b", "#24d45d", "#36df70", "#49ea82", "#5bf494", "#2ca34d")
# Plot scatterplot with connected lines, each a different shade of blue per participant
ggplot(TwoWay_data, aes(x = turn_number, y = score, color = as.factor(participantNumber))) +
  geom_point(size=4) +
  geom_line(linewidth=1.5, aes(group = participantNumber), alpha = 0.5) +
  labs(x = "Turn Number", y = "Score", title = "Scatterplot of Participant Scores with Connected Lines") +
  annotate("rect", xmin = 14, xmax = 25, ymin = -Inf, ymax = Inf, alpha = .45,fill = "grey") + 
  scale_color_manual(values = green_shades) +
  theme_minimal()
##################################################################################
##################################################################################
(lme4)
lmm_model <- lmer(score ~ Condition * turn_number + (1 | participantNumber), data = sorted_DTS)
summary(lmm_model)
library(emmeans)
emm <- emmeans(lmm_model, ~ Condition | turn_number)
plot(emm, comparisons = TRUE)
##################################################################################
# this anova shows significance between the conditions, but not between the condition and turn number 
anova_result <- aov(score ~ Condition * turn_number + Error(participantNumber/turn_number), data = sorted_DTS)
summary(anova_result)
#interaction plot
interaction.plot(sorted_DTS$turn_number, sorted_DTS$Condition, sorted_DTS$score, type = "b", xlab = "turn_number", ylab = "Scores")
#pairwise comparisons
pairwise_comp <- pairwise.t.test(sorted_DTS$score, sorted_DTS$Condition, p.adj = "bonferroni")
print(pairwise_comp)
##################################################################################
# Summary Stats
sorted_DTS %>%
  group_by(Condition, turn_number) %>%
  get_summary_stats(score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = sorted_DTS, dv = score, wid = participantNumber,
  within = c(Condition, turn_number)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- sorted_DTS %>%
  group_by(turn_number) %>%
  pairwise_t_test(
    score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats for turn number 
pwc <- pwc %>% add_xy_position(x = "turn_number")
bxp <- ggboxplot(
  sorted_DTS, x = "turn_number", y = "score",
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
  sorted_DTS, x = "Condition", y = "score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )
################################################################################################
# Now divided into sections (1,2,3) for average conditional difference
sorted_DTS_Stage1 <- sorted_DTS[sorted_DTS$turn_number < 14, ]
sorted_DTS_Stage2 <- sorted_DTS[sorted_DTS$turn_number <= 25 & sorted_DTS$turn_number >= 14, ]
sorted_DTS_Stage3 <- sorted_DTS[sorted_DTS$turn_number < 25, ]
##################################################################################################
# Summary Stats
sorted_DTS_Stage1 %>%
  group_by(Condition, turn_number) %>%
  get_summary_stats(score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = sorted_DTS_Stage1, dv = score, wid = participantNumber,
  within = c(Condition, turn_number)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- sorted_DTS_Stage1 %>%
  group_by(turn_number) %>%
  pairwise_t_test(
    score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats for Condition
pwc <- pwc %>% add_xy_position(x = "Condition")
bxp <- ggboxplot(
  sorted_DTS_Stage1, x = "Condition", y = "score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )
###############################################################################
# Summary Stats
sorted_DTS_Stage2 %>%
  group_by(Condition, turn_number) %>%
  get_summary_stats(score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = sorted_DTS_Stage2, dv = score, wid = participantNumber,
  within = c(Condition, turn_number)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- sorted_DTS_Stage2 %>%
  group_by(turn_number) %>%
  pairwise_t_test(
    score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats for Condition
pwc <- pwc %>% add_xy_position(x = "Condition")
bxp <- ggboxplot(
  sorted_DTS_Stage2, x = "Condition", y = "score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )
################################################################################
sorted_DTS_Stage3 %>%
  group_by(Condition, turn_number) %>%
  get_summary_stats(score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = sorted_DTS_Stage3, dv = score, wid = participantNumber,
  within = c(Condition, turn_number)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- sorted_DTS_Stage3 %>%
  group_by(turn_number) %>%
  pairwise_t_test(
    score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats for Condition
pwc <- pwc %>% add_xy_position(x = "Condition")
bxp <- ggboxplot(
  sorted_DTS_Stage3, x = "Condition", y = "score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )
################################################################################
################################################################################
# Add ability to see differences in score for each participant 
# Two-way - OneWay
# So we want the difference to be larger 
library(dplyr)
library(ggplot2)
# Calculate the difference in score between conditions for each participant for each turn
sorted_DTS_difference <- sorted_DTS %>%
  group_by(participantNumber, turn_number) %>%
  summarize(score_difference = diff(score))
# define shaded rectangles 
rect1 <- data.frame (xmin=0, xmax=40, ymin=-4.2, ymax=-0.8)
rect2 <- data.frame (xmin=0, xmax=40, ymin=0.8, ymax=4.2)

## Differences scatter plot
ggplot(sorted_DTS_difference, aes(x = turn_number, y = score_difference, color = as.factor(participantNumber))) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#619cff", alpha=0.35, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#00ba38", alpha=0.35, inherit.aes = FALSE) +
  geom_point() +
  labs(x = "Turn Number", y = "Score Difference") +
  ggtitle("Score Difference between Conditions by Turn for Each Participant") +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  scale_color_grey() +
  expand_limits(x=c(1, 40),y=c(-4,4)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1)) + 
  theme_minimal() 




