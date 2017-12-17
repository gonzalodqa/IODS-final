# Final Assignment

# Read data

human <- read.csv("C:/Users/D E L L/Documents/GitHub/IODS-project/Data/human.csv", sep = ",", header = TRUE)
glimpse(human)

library(GGally)
library(dplyr)
library(corrplot)

# Correlation

ggpairs(human)

cor(human) %>% corrplot

# Model

model <- lm(matermor ~ lifexp + adobirth, data = human)
summary(model)
model

#Plot

g1 <- ggplot(human, aes(x=matermor, y=lifexp + adobirth))
g1 + geom_point()

par(mfrow = c(2,2))
plot(model, which= c(1,2,5))

# K-means

human_scaled <- scale(human)
summary(human_scaled)

dist_eu <- dist(human_scaled)

summary(dist_eu)

k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans (human_scaled, k)$tot.withinss})
qplot(x = 1:k_max, y = twcss, geom = 'line')

km <-kmeans(human_scaled, centers = 2)
pairs(human_scaled,col = km$cluster)
