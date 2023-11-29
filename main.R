library(aod)
library(ggplot2)
library(readxl)

############ CONFIGURATION ###########

# First we read and format
df <- read_excel("data/distance.dataset.xlsx", sheet=1, col_names = TRUE, col_types = c("numeric", "text", "text", "numeric"))
df$structure <- gsub("\\r\\n", "", df$structure)
df$question <- as.numeric(gsub("[^0-9]", "", df$question))
df$structure <- factor(df$structure)
summary(df)

# Then we get the categories we want to observe
target_values <- c(unique(df$structure))

# And we create binary columns of them if they were found or not
df$overt_connective <- ifelse(df$structure %in% target_values[1], 1, 0)
df$elliptical <- ifelse(df$structure %in% target_values[2], 1, 0)
df$juxtaposition <- ifelse(df$structure %in% target_values[3], 1, 0)

# We factor them, as they're categorical values
df$overt_connective <- factor(df$overt_connective)
df$elliptical <- factor(df$elliptical)
df$juxtaposition <- factor(df$juxtaposition)

# And we observe them
summary(df)

############ LOGISTIC REGRESSION ###########

# First, we'll check out if the binary columns (e.g. "overt connective") can be interpreted with the
# other values
mylogit <- glm(overt_connective ~ distance + question + participant, data = df, family = "binomial")
summary(mylogit)
# Our results show that there's not a statistically significant relation with participans
# as p = 0.20719. However (!) we find a highly significant relation with questions
# as p = 0.00712

# In order to find more about it, we have to examine the question variable as categorical values
df_to_observe <- df
df_to_observe$question <- factor(df_to_observe$question)
summary(df_to_observe)

# So we check the logistic regression again
mylogit_to_observe <- glm(overt_connective ~ distance + question + participant, data = df_to_observe, family = "binomial")
summary(mylogit_to_observe)
# If we check the questions as categorical values, we notice that questions 
# 7, 8, 11, 16, 18, 21, 22, 24, 27 and 29 are highly significant related.
# So, we remove them from our data set
values_to_remove <- c(7, 8, 11, 16, 18, 21, 22, 24, 27, 29)
df_to_observe <- subset(df_to_observe, !(df_to_observe$question %in% values_to_remove))

# We notice that now, we have 361 observations less
length(df$question) - length(df_to_observe$question)

# And we check our logistic regression again
mylogit_to_observe <- glm(overt_connective ~ distance + question + participant, data = df_to_observe, family = "binomial")
summary(mylogit_to_observe)

# There aren't any more highly correlated relations between the questions
# So we can turn them back to numeric
df_to_observe$question <- as.numeric(df_to_observe$question)

# And test our logistic regression again
mylogit_to_observe <- glm(overt_connective ~ distance + question + participant, data = df_to_observe, family = "binomial")
summary(mylogit_to_observe)

# Our distance is statistically significant and implies that when distance increases 1 pt.,
# the probabilities of our overt connective increases by 0.012169 

############ VISUALIZATION ###########
#install.packages("coefplot")
library(coefplot)
#install.packages("pROC")
library(pROC)
#install.packages("car")
library(car)

df_to_visualize <- df_to_observe
mylogit_to_visualize <- glm(overt_connective ~ distance + question + participant, data = df_to_visualize, family = "binomial")

# Coefficient Plot:
coefplot(mylogit_to_observe)

# ROC Curve:
roc_curve <- roc(df_test$overt_connective, predict(model, type = "response"))
plot(roc_curve, main = "ROC Curve", col = "blue")

# Partial results Plot:
crPlots(model, "distance")

# Deviance Residuals Plot:
plot(model, which = 1)



