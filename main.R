############ PACKAGES ###########

library(coefplot)
library(ggplot2)
library(readxl)
library(pROC)
library(conf)
library(aod)

############ CONFIGURATION ###########

# First we read and format correctly:
df <-
  read_excel(
    "data/distance.dataset.xlsx",
    sheet = 1,
    col_names = TRUE,
    col_types = c("numeric", "text", "text", "numeric")
  )

df$structure <- gsub("\\r\\n", "", df$structure)
df$question <- as.numeric(gsub("[^0-9]", "", df$question))
df$structure <- factor(df$structure)
summary(df)

# Then we get the categories we want to observe
target_values <- c(unique(df$structure))

# And we create binary columns of them if they were found or not
df$overt_connective <-
  ifelse(df$structure %in% target_values[1], 1, 0)
df$elliptical <- ifelse(df$structure %in% target_values[2], 1, 0)
df$juxtaposition <- ifelse(df$structure %in% target_values[3], 1, 0)

# We factor them, as they're categorical values
df$overt_connective <- factor(df$overt_connective)
df$elliptical <- factor(df$elliptical)
df$juxtaposition <- factor(df$juxtaposition)

# And we observe them
summary(df)

# NOTE:
# We have 1 numeric value (distance), 4 categorical values (structure, that we
# won't use anymore; overt_connective; elliptical and juxtaposition) as well as
# two more numeric values (questions and participants) that we will analyze as
# random values. This means we have to analyze how influential (or not) these
# are in our analysis.

############ LOGISTIC REGRESSION ###########

# First, we'll check out if the binary columns (e.g. "overt connective") can be
# interpreted with the other values
log_reg <-
  glm(
    overt_connective ~ distance + question + participant,
    data = df,
    family = "binomial"
  )
summary(log_reg)
# Our results show that there's not a statistically significant relation with
# "participants" as p = 0.20719. However (!) we find a highly significant
# relation with "questions" as p = 0.00712. In order to find more about it, we
# will  examine the question values as categorical values

df_to_observe <- df
df_to_observe$question <- factor(df_to_observe$question)
summary(df_to_observe)

# So we check the logistic regression again
log_reg_to_observe <-
  glm(
    overt_connective ~ distance + question + participant,
    data = df_to_observe,
    family = "binomial"
  )
summary(log_reg_to_observe)

# If we check the questions as categorical values, we notice that the questions
# 7, 8, 11, 16, 18, 21, 21, 22, 24, 27 and 29 are highly significant. So, we remove
# them from our data set. Why? Because we want random values that do not affect
# our intercept.
values_to_remove <- c(7, 8, 11, 16, 18, 21, 22, 24, 27, 29)
df_to_observe <-
  subset(df_to_observe,
         !(df_to_observe$question %in% values_to_remove))

# We notice that now, we have 361 less observations
length(df$question) - length(df_to_observe$question)

# And we check our logistic regression again
log_reg_to_observe <-
  glm(
    overt_connective ~ distance + question + participant,
    data = df_to_observe,
    family = "binomial"
  )
summary(log_reg_to_observe)

# There aren't any more highly correlated values
# So we can turn them back to numeric
df_to_observe$question <- as.numeric(df_to_observe$question)

# And test our logistic regression again
log_reg_to_observe <-
  glm(
    overt_connective ~ distance + question + participant,
    data = df_to_observe,
    family = "binomial"
  )
summary(log_reg_to_observe)

# Gott sei Dank is our intercept still statistically significant (p= 0.00133)
# and implies that when distance increases by 1% the probabilities of an
# overt connective happennig increases by 0.012169%. Now, we can visualize.

############ VISUALIZATION ###########

df_to_visualize <- df_to_observe
log_reg_to_visualize <-
  glm(overt_connective ~ distance,
      data = df_to_visualize,
      family = "binomial")

# Coefficient Plot:
coefplot(log_reg_to_visualize)

# ROC Curve:
roc_curve <-
  roc(df_to_visualize$overt_connective,
      predict(log_reg_to_visualize, type = "response"))
plot(roc_curve, main = "ROC Curve", col = "blue")

# Deviance Residuals Plot:
plot(log_reg_to_visualize, which = 1)

# And lastly a normal plot:
summary(log_reg_to_visualize)
xdistance <- seq(0, max(df_to_visualize$distance), 5)
yprobabily <-
  predict(log_reg_to_visualize, list(distance = xdistance), type = "response")

plot(xdistance, yprobabily)
lm_model <- lm(yprobabily ~ xdistance)
abline(lm_model, col = "blue")





############ REFERENCES ###########

# Articles:
# Clark, R. G., Blanchard, W., Hui, F. K. C., Tian, R., & Woods, H. (2023, April 1). Dealing with complete separation and quasi-complete separation in logistic regression for linguistic data. Research Methods in Applied Linguistics. https://doi.org/10.1016/j.rmal.2023.100044

# Books:
# Johnson, K. (2008). Quantitative methods in linguistics / Keith Johnson. Blackwell. Chapter 5.5 An Example from the [∫]treets of Columbus.
# Hosmer, D. & Lemeshow, S. (2000). Applied Logistic Regression (Second Edition). New York: John Wiley & Sons, Inc. Chapter 1 Introduction to the Logistic Regression Model

# Websites:
# https://stats.oarc.ucla.edu/r/dae/logit-regression/
