library(rpart)
data.pred$CLIENT_ID <- as.factor(data.pred$CLIENT_ID)

mytree <- rpart(score ~ CLIENT_ID, 
                data   = data.pred, 
                method = "anova"
)

a <- predict(mytree, df=data.pred)
data.pred$client_group <- predict(mytree, df=data.pred)


data <- read.csv(file = paste("", "high_risk.csv", sep = ""))

i <- data$IS_CHARGEOFF == 1
plot(density(data[i,]$RATIO), col="red")
lines(density(data[!i,]$RATIO))

summary(data[i,]$RATIO)
summary(data[!i,]$RATIO)
