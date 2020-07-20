#data <- read.csv(file = "export.csv")
# Kernel Density Plot
#i <- data$BIN1 == 1
#d1 <- density(data[i,]$DEM_VS_SL, na.rm = TRUE) # returns the density data
#median(data[i,]$DEM_VS_SL, na.rm = TRUE)
#summary(data[i,]$DEM_VS_SL, na.rm = TRUE)
#quartile
#plot(d1) # plots the results

library(rpart)
library(rpart.plot)
data <- read.csv(file = "export2.csv")
dat <- data
mytree <- rpart(BENEFIT ~ VERIFIEDSALARY + TENURE_FIRSTORDER + SPENDING_LIMIT_FIRSTORDER, 
                data   = dat, 
                method = "anova",
                control = list(cp = 0.001, xval = 10),
                na.action = na.exclude
)
mytree
rpart.plot(mytree, cex = 0.6)
dat$Segment <- predict(mytree, df=dat)

unique(dat$Segment)


i <- round(dat$Segment, 4) == 125.8105
dat <- dat[i,]
summary(dat$DEM_VS_SL)


i <- round(dat$Segment, 4) == 125.8105 & dat$OUTCOME == 1
dat <- dat[i,]
summary(dat$DEM_VS_SL)




aggregate(data$LOSS, by=list(data$Segment), FUN = sum)
df.1 <- aggregate(data$APPLIED, by=list(data$Segment), FUN = sum)
df.2 <- aggregate(data$COST, by=list(data$Segment), FUN = sum)

df.1$ratio <- df.1$x / df.2$x
df.1

i <- !is.na(dat$Segment)
a1 <- sum((dat[i,]$BENEFIT - dat[i,]$Segment)^2, na.rm=TRUE)
a2 <- sum((dat[i,]$BENEFIT - mean(dat[i,]$BENEFIT))^2, na.rm=TRUE)

a1/a2
rsq.rpart(mytree)