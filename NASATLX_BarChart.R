ColorsByCondition <- c("#f8766d", "#619cff", "#00ba38")

################################################################################
# boxplot for averages results with the simplified NASATLX csv
NASATLX<-read.csv(file="B Study_NASA-TLX.csv") 
NASAOrg <- data.frame(
  Participants=c(NASATLX[, c(1)] ),
  Condition=c(NASATLX[, c(2)] ),
  Category=c(NASATLX[, c(3)]),
  Score=c(NASATLX[, c(4)])
)
################################################################################
library(ggplot2)
summary_stats_NASA <- with(NASAOrg, tapply(Score, list(Condition, Category), summary))
# grouped boxplot
ggplot(NASAOrg, aes(x=Category, y=Score, fill=Condition)) + 
  geom_boxplot() +
  expand_limits(y=c(1, 7)) +
  scale_fill_manual(values=c("#619cff", "#00ba38")) +
  labs(title = "NASA-TLX Results") +
  theme(plot.title = element_text(hjust=0.5)) 
################################################################################
# Stats Analysis
library(tidyverse)
library(ggpubr)
library(rstatix)

# Summary Stats
NASAOrg %>%
  group_by(Condition, Category) %>%
  get_summary_stats(Score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = NASAOrg, dv = Score, wid = Participants,
  within = c(Condition, Category)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- NASAOrg %>%
  group_by(Category) %>%
  pairwise_t_test(
    Score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats
pwc <- pwc %>% add_xy_position(x = "Category")
bxp <- ggboxplot(
  NASAOrg, x = "Category", y = "Score",
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
  NASAOrg, x = "Condition", y = "Score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )
#################################################################################################
# Outliers
NASAOrg %>%
  group_by(Condition, Category) %>%
  identify_outliers(Score)
#Normality Assumption
NASAOrg %>%
  group_by(Condition, Category) %>%
  shapiro_test(Score)
#qqplots
ggqqplot(NASAOrg, "Score", ggtheme = theme_bw()) +
  facet_grid(Category ~ Condition, labeller = "label_both")
# Effect of Condition at each level of Category
one.way2 <- NASAOrg %>%
  group_by(Category) %>%
  anova_test(dv = Score, wid = Participants, within = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
# Pairwise comparisons between Condition 
pwc2 <- NASAOrg %>%
  group_by(Category) %>%
  pairwise_t_test(
    Score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2
# comparisons for treatment variable
NASAOrg %>%
  pairwise_t_test(
    Score ~ Condition, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# comparisons for time variable
NASAOrg %>%
  pairwise_t_test(
    Score ~ Category, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
################################################################################################
# now we need to flip performance and then ask of there is significance between the conditions
NASAOrg_flippedPref <- NASAOrg
NASAOrg_flippedPref$Score[NASAOrg_flippedPref$Category == "Performance"] <- 8 - NASAOrg_flippedPref$Score[NASAOrg_flippedPref$Category == "Performance"]

library(tidyverse)
library(ggpubr)
library(rstatix)

# Summary Stats
NASAOrg_flippedPref %>%
  group_by(Condition, Category) %>%
  get_summary_stats(Score, type = "mean_sd")
# actual anova
res.aov <- anova_test(
  data = NASAOrg_flippedPref, dv = Score, wid = Participants,
  within = c(Condition, Category)
)
get_anova_table(res.aov)
# Pairwise comparisons between Conditions
pwc <- NASAOrg_flippedPref %>%
  group_by(Category) %>%
  pairwise_t_test(
    Score ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# Boxplot of summary stats
pwc <- pwc %>% add_xy_position(x = "Category")
bxp <- ggboxplot(
  NASAOrg_flippedPref, x = "Category", y = "Score",
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
  NASAOrg_flippedPref, x = "Condition", y = "Score",
  fill = "Condition", palette = c("#619cff", "#00ba38") 
)
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc) 
  )





