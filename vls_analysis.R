
library(dplyr)
library(readxl)
library(writexl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(car)
library(gridExtra)
library(lme4)
library(lmerTest)
library(gridExtra)
library(RColorBrewer)
library(emmeans)
library(magick)
library(ez)

setwd("~/Documents/RPTU/WS_24_25/LB_Cognition/VLS/data")
data_for_mixed = read_excel("vvlsdeen_mixed_0617.xlsx")


# -------------------- Accuracy in mixed blocks  --------------------
## General accuracy

round(prop.table(table(data_for_mixed$accuracy))*100, 2)


## Accuracy per person
data_for_mixed %>%
  group_by(subject_nr) %>%
  summarise(
    accuracy_percent = round(mean(accuracy) * 100, 2)
  )
# All the participants have an accuracy of over 90%, with the lowest one being 94%, and the others over 97%.

# -------------------- Language use pattern  --------------------
data_lan <- data_for_mixed %>%
  filter(accuracy == 1, 
         #!subject_nr %in% c(2,5,9,11,14)
         )

table(data_lan$subject_nr, data_lan$language)


# set up aesthetic parameters for plotting
labeltype = c("repeat" = "Repeat", "switch" = "Switch")
labelvalence <- c("neutral" = "Neutral", "positive" = "Positive", "negative" = "Negative")
labellanguage = c("DE" = "German", "EN" = "English")
labellerlanguage <- as_labeller(c("EN" = "English",
                               "DE" = "German"))

# plot language use pattern (frequency of each language by valence)
ggplot(data_lan, aes(x = valence, fill = language)) +
  geom_bar(position = position_dodge(width = 0.6), width = 0.6) +
  scale_fill_brewer(palette = "Set2", labels = labellanguage, name = "") +  # Set labels and name here
  scale_x_discrete(labels = labelvalence) +
  labs(
    title = "",
    x = "Valence",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# This quick visualization suggests that participants tended to use English more frequently when responding to emotionally charged images. So, I decided to run a generalized linear model (GLM) to examine language use, then a generalized linear mixed-effects model (GLMER) to account for random variation across participants.

## -------------------- Run statistical models  --------------------

# set contrast 
data_lan$valence = as.factor(data_lan$valence)
contrast.valence = cbind(c(1, 0, 0), c(0, 0, 1)) # "negative","neutral","positive"
colnames(contrast.valence) = c("negative", "positive")
contrasts(data_lan$valence) = contrast.valence
contrasts(data_lan$valence)
# using neutral as the reference level

data_lan$language = as.factor(data_lan$language)

### -------------------- 1 glm  --------------------
glm_languageuse <- glm(language ~ valence, data = data_lan, family = binomial(link="logit"))


### -------------------- 2 glmer  --------------------
glmer_languageuse <- glmer(language ~ valence + (valence | subject_nr), 
                           data = data_lan, 
                           family = binomial())

### model comparison
AIC(glm_languageuse, glmer_languageuse) 

# glmer better
summary(glmer_languageuse)

## Interpreting results
### negative valence
exp(0.6230)
# 1.86 times more likely to use English than German when the word is negative compared to neutral.

# 95CI
exp(0.6230 - 1.96*0.2103)
exp(0.6230 + 1.96*0.2103)


### positive valence
exp(0.5598)
# 1.75 times more likely to use English than German when the word is negative compared to neutral.

# 95CI
exp(0.5598 - 1.96*0.1722)
exp(0.5598 + 1.96*0.1722)

# --------------------        RT             --------------------

data_trim <- data_for_mixed %>%
  filter(type != "unclassifiable", accuracy == 1)%>%
  group_by(subject_nr) %>%
  filter(between(RT,
                 mean(RT, na.rm = TRUE) - 2.5 * sd(RT),
                 mean(RT, na.rm = TRUE) + 2.5 * sd(RT))) %>%
  ungroup()

round((1-length(data_trim$RT)/length(data_for_mixed$RT))*100,2)
# In total, 5.5% of data are trimmed out.

hist(data_trim$RT)
range(data_trim$RT)

# plot RT
data_trim$type <- factor(data_trim$type, levels = c("switch", "repeat"))

ggplot(data_trim, aes(x = valence, y = RT, color = type, group = type)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size = 0.8) +
  scale_color_discrete(name = "", label = labeltype) +
  facet_wrap(~ language, labeller = labellerlanguage) +        # labeller, not label
  scale_x_discrete(labels = labelvalence) +
  labs(
    title = "",
    x = "Valence",
    y = "Mean reaction time (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


ggplot(data_trim, aes(x = valence, y = RT, color = type, group = type)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(
    fun.data = mean_se,  
    geom = "errorbar",
    width = 0.15, size = 0.8
  ) +
  scale_color_brewer(palette = "Paired", labels = labeltype, name = "") +
  scale_x_discrete(labels = labelvalence) +
  labs(
    title = "Reaction time by valence and type",
    x = "Valence",
    y = "Mean reaction time (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    # legend.position = "top",
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16)
  )


ggplot(data_trim, aes(x = valence, y = RT, color = language, group = language)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(
    fun.data = mean_se,  
    geom = "errorbar",
    width = 0.15, size = 0.8
  ) +
  scale_color_discrete(name = "", label = labellanguage) +
  scale_x_discrete(labels = labelvalence) +
  labs(
    title = "Reaction time by valence and language",
    x = "Valence",
    y = "Mean reaction time (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    # legend.position = "top",
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16)
  )


# Emotional charge decreases reaction time in the foreign language, 
# but increased reaction time when use native language.

## --------------------   Run statistical models    --------------------


### -------------------- 1 anova   --------------------

aov_model <- aov(RT ~ valence * language * type + 
                   Error(subject_nr/(valence + language + type)), 
                 data = data_trim)

ezDesign(
  data = data_trim,
  x = valence,
  y = subject_nr,
  col = language,
  row = type
)

# remove participants with incomplete data
aovdata = data_trim %>%
  filter(!subject_nr %in% c(2,5,9,11,14))

aov_model1 <- aov(RT ~ valence * language * type + 
                   Error(subject_nr/(valence + language + type)), 
                 data = aovdata)
names(aov_model1)
resid_vals <- residuals(aov_model[["Within"]])

hist(resid_vals)
qqnorm(resid_vals)
qqline(resid_vals)

shapiro.test(resid_vals)
# violated but it doesn't look too bad?

ezANOVA(
  data = aovdata,
  dv = .(RT),
  wid = .(subject_nr),
  within = .(valence, language, type),
  detailed = TRUE
)

# main effect of type F(1,9) = 8.769, p<.05. 
# interaction valence:language:type F(2,18) = 5.804, ε = 0.87, p<.05.


### -------------------- 2 lm   --------------------

# set sum contrast for all factors
data_trim$language = as.factor(data_trim$language)
contrast.language = cbind(c(-1,1))
colnames(contrast.language) = c("EN")
contrasts(data_trim$language) = contrast.language
contrasts(data_trim$language)

data_trim$valence = as.factor(data_trim$valence)
contrast.valence = cbind(c(1, -1, 0), c(0, -1, 1)) # "negative", "neutral","positive"
colnames(contrast.valence) = c("negative", "positive")
contrasts(data_trim$valence) = contrast.valence
contrasts(data_trim$valence)

# data_trim$type = as.factor(data_trim$type)
type = cbind(c(1,-1))
colnames(type) = c("switch")
contrasts(data_trim$type) = type
contrasts(data_trim$type)


lm_rt = lm(RT ~ valence * language * type, data = data_trim)
qqnorm(resid(lm_rt))
qqline(resid(lm_rt))

# The residuals showed a poor fit, so I transformed the data.

loglm_rt = lm(log(RT) ~ valence * language * type, data = data_trim)
qqnorm(resid(loglm_rt))
qqline(resid(loglm_rt))
# now it looks good enough

### -------------------- 3 lmer   --------------------
loglmer_rt0 = lmer(log(RT) ~ valence * language * type + (valence | subject_nr), data = data_trim)
# failed to converge
loglmer_rt1 = lmer(log(RT) ~ valence * language * type + (1 | subject_nr), data = data_trim)
loglmer_rt2 = lmer(log(RT) ~ valence * language + type + (valence | subject_nr), data = data_trim)
loglmer_rt3 = lmer(log(RT) ~ valence * language + type + (1 | subject_nr), data = data_trim)
loglmer_rt4 = lmer(log(RT) ~ valence * language + (valence | subject_nr), data = data_trim)
loglmer_rt5 = lmer(log(RT) ~ valence * language + (1 | subject_nr), data = data_trim)
loglmer_rt6 = lmer(log(RT) ~ valence * language * type + (1+ valence | subject_nr) + (1|german), data = data_trim)
# failed to converge
loglmer_rt7 = lmer(log(RT) ~ valence * language * type + (1|subject_nr) + (1|german), data = data_trim)

AIC(loglm_rt, loglmer_rt1, loglmer_rt2, loglmer_rt3, loglmer_rt4, loglmer_rt5, loglmer_rt7)
BIC(loglm_rt, loglmer_rt1, loglmer_rt2, loglmer_rt3, loglmer_rt4, loglmer_rt5, loglmer_rt7)

qqnorm(resid(loglmer_rt7))
qqline(resid(loglmer_rt7))

anova(loglmer_rt7)
summary(loglmer_rt7)

pairs(emmeans(loglmer_rt7, ~ valence * language * type, adjust="tukey",
              lmerTest.limit = 5000, 
              pbkrtest.limit = 5000))

