library(aod)
library(ggplot2)
library(readxl)

df <- read_excel("data/distance.dataset.xlsx", sheet=1, col_names = TRUE, col_types = c("numeric", "text", "text", "numeric"))
df$structure <- gsub("\\r\\n", "", df$structure)
df$question <- as.numeric(gsub("[^0-9]", "", df$question))

