library(smbinning)
library(InformationValue)
library(randomForest)
library(woeBinning)
library(randomForest)
# Read Data
set.seed = 1


#query <- 'select * from v_mart_delinquency_train'
#data2 <- exeQueryString (query, stringsAsFactors = TRUE)
#data2 <- as.data.frame(unclass(data2), stringsAsFactors=TRUE)
#data2$ORDER_DT_MONTH <- as.factor(data2$ORDER_DT_MONTH)
#data2$ORDER_DT_HOUR  <- as.factor(data2$ORDER_DT_HOUR)

  
data <- read.csv(file = "v_mart_delinquency_train.csv")
data <- as.data.frame(unclass(data), stringsAsFactors=TRUE)
#data <- data2

# Create Training Data
input_ones <- data[which(data$OUTCOME == 1), ]  # all 1's
input_zeros <-data[which(data$OUTCOME == 0), ]  # all 0's
# Create Test Data
input_ones_train_rows  <- sample(1:nrow(input_ones), 0.7 * nrow(input_ones))  # 1's for training
input_zeros_train_rows <- sample(1:nrow(input_zeros), 0.7 * nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's
# 
training_ones <- input_ones[input_ones_train_rows, ]  
training_zeros <- input_zeros[input_zeros_train_rows, ]
#data_train <- rbind(training_ones, training_zeros[1:nrow(training_ones),])  # row bind the 1's and 0's 
data_train <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 
# 
test_ones <- input_ones[-input_ones_train_rows, ]
test_zeros <- input_zeros[-input_zeros_train_rows, ]
data_test <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 


lst.smb <- list()

var <- 'DEM'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'DIS'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'USERGROUP_WOA'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var = 'TIME_BETWEEN_REG_FIRST'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'STATE_WOA'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'DEPT_WOA'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'CLASS_WOA'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'DAYS_SINCE_LAUNCH'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = lst.smb[[var]], chrname = paste(var, "_SMBIN", sep=""))

var <- 'ELIGIBLES'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = smb, chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = smb, chrname = paste(var, "_SMBIN", sep=""))

var = 'VERIFIEDSALARY'
smb <- smbinning(data_train, y="OUTCOME", x=var, p = 0.05); lst.smb[[var]] <- smb 
smb$iv
data_train <- smbinning.gen(df = data_train, ivout = smb, chrname = paste(var, "_SMBIN", sep=""))
data_test  <- smbinning.gen(df = data_test , ivout = smb, chrname = paste(var, "_SMBIN", sep=""))



lst.models[["logitMod"]] <- list()
logitMod <- glm(OUTCOME ~ ORDER_DT_MONTH + 
                          DEPT_WOA_SMBIN +
                          CLASS_WOA_SMBIN +
                          DEM_SMBIN + 
                          DIS_SMBIN +
                          USERGROUP_WOA_SMBIN +
                          TIME_BETWEEN_REG_FIRST_SMBIN +
                          STATE_WOA_SMBIN +
                          ELIGIBLES_SMBIN +
                          DAYS_SINCE_LAUNCH_SMBIN +
                          VERIFIEDSALARY_SMBIN
                , 
                data=data_train, 
                family=binomial(link="logit")
)
lst.models[["logitMod"]] <- logitMod
summary(logitMod)
predicted <- predict(logitMod, data_test, type="response")  # predicted scores
plotROC(data_test$OUTCOME, predicted)

i <- !is.na(predicted) & predicted >= 0.375
sum(i)
sum(i)/length(i)







save(lst.models, file = "models.RData")
save(lst.smb, file = "smb.RData")



library(rpart)

fit <- rpart(formula = OUTCOME ~ VERIFIEDSALARY + TIME_BETWEEN_REG_FIRST + CLASS_WOA + USERGROUP_WOA + DEM + DIS, 
                data = data_train, 
             control = rpart.control(cp = 0.001), 
              method = "anova"
            )
predicted <- predict(fit, data_test)
i <- !is.na(predicted) & predicted >= 0.375
sum(i)
sum(i)/length(i)


fit <- rpart(formula = OUTCOME ~ TIME_BETWEEN_REG_FIRST, 
             data = data_train, 
             control = rpart.control(cp = 0.0001), 
             method = "anova"
)
fit


dat <- data[!is.na(data$VERIFIEDSALARY) & data$VERIFIEDSALARY <= 26000,]


plot(density(dat$ORIGINAL))
lines(density(dat[dat$OUTCOME == 1,]$ORIGINAL))



plot(density(dat[dat$OUTCOME == 1,]$ORIGINAL))
lines(density(dat[dat$OUTCOME == 1,]$APPLIED))

mean(dat[dat$OUTCOME == 1,]$ORIGINAL)
mean(dat$ORIGINAL)





dt <- read.csv("export.csv")
fit <- rpart(formula = OUTCOME ~ VERIFIEDSALARY + RATIO, 
             data = dt, 
             control = rpart.control(cp = 0.003), 
             method = "anova"
)
fit
