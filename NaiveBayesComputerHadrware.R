# Libraries
install.packages("naivebayes")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("magrittr")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(magrittr)
# Data
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data'
data <- read.csv(urlfile, header = FALSE, sep = ",", quote = "\"'")
str(data)
xtabs(~V1+V9, data = data)
data$vendor <- as.factor(data$V1)
data$model <- as.factor(data$V9)
data$myct <- as.factor(data$V2)
data$cach <- as.factor(data$V5)


# Visualization
pairs.panels(data[-1])
data %>%
  ggplot(aes(x=vendor, y=cach, fill = vendor)) + 
  geom_boxplot() +   
  ggtitle("Box Plot")

data %>% ggplot(aes(x=cach, fill = vendor)) +  
  geom_density(alpha=0.8, color= 'black') +   
  ggtitle("Summary Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(vendor ~ ., data = train, usekernel = T)
model

train %>%
  filter(vendor == "apollo") %>%
  summarise(mean(V9), sd(V9))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$weight))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$weight))
1 - sum(diag(tab2)) / sum(tab2)

