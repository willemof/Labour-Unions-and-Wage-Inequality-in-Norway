caret

sds <- read_csv("csv/sds.csv")

#Visualise

ggplot(sds, aes(x=sds$))

#OLS regression

glm

iris <- iris

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))


featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))


createDataPartition()

model <- glm(log(wage) ~ education_level + occupation + industry, data = sds, family = gaussian(link = "log"))


wage_cols <- sds %>%
  select(colnames(sds[85:3972]))

wage_matrix <- as.matrix(wage_cols)

wage_matrix <- na.omit(wage_matrix)

pca <- prcomp(wage_matrix, scale. = TRUE)

library(mice)
