# Libraries
source("db.R")
library(survival)
library(dplyr)
library(rpart)
datapath <- "C:/Users/tbountourelis/OneDrive - PURCHASING POWER, LLC/R Projects/Data/"

# Load Objects
load(paste(datapath, "predict.RData", sep = ""))

# Read Data
query <- 'select * from v_mart_delinquency_pred_buyers'
data.pred <- exeQueryString (query, stringsAsFactors = TRUE)

# Get model
model <- models[["Registrants"]]

# Get current score
data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_C 
data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_C
data.pred$score_credit_c <- predict(model, 
                             data.pred, 
                             type = "response"
)

data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_F 
data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_F
data.pred$score_credit_f <- predict(model, 
                             data.pred, 
                             type = "response"
)

data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_R 
data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_R
data.pred$score_credit_r <- predict(model, 
                             data.pred, 
                             type = "response"
)


# Load Objects
load(paste(datapath, "model_NetMargin.RData", sep = ""))

# Load Objects
model <- models[["NetMargin-RF-Registrants"]]

##
data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_C
data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_C
cols <- c("WOE_BIN_TENURE", "WOE_BIN_VERIFIEDSALARY", "WOE_BIN_CLIENT", "WOE_BIN_DAYS_SINCE_REG")
data.pred[, cols] <- lapply(data.pred[, cols], factor)
levels(data.pred$WOE_BIN_CLIENT)         <- model$forest$xlevels$WOE_BIN_CLIENT
levels(data.pred$WOE_BIN_VERIFIEDSALARY) <- model$forest$xlevels$WOE_BIN_VERIFIEDSALARY
levels(data.pred$WOE_BIN_TENURE)         <- model$forest$xlevels$WOE_BIN_TENURE
levels(data.pred$WOE_BIN_DAYS_SINCE_REG) <- model$forest$xlevels$WOE_BIN_DAYS_SINCE_REG
data.pred$score_nm_reg <- predict(model, 
                              data.pred[, c("WOE_USERGROUP", "WOE_BIN_DAYS_SINCE_REG", "WOE_BIN_TENURE", "WOE_BIN_VERIFIEDSALARY", "WOE_BIN_CLIENT")]
)

model <- models[["NetMargin-RF-FTB"]]
##
data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_C
data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_C
cols <- c("WOE_BIN_TENURE", "WOE_BIN_VERIFIEDSALARY", "WOE_BIN_CLIENT", "WOE_BIN_DAYS_SINCE_REG", "STATUS_ACTIVE")
data.pred[, cols] <- lapply(data.pred[, cols], factor)
levels(data.pred$WOE_BIN_CLIENT)         <- model$forest$xlevels$WOE_BIN_CLIENT
levels(data.pred$WOE_BIN_VERIFIEDSALARY) <- model$forest$xlevels$WOE_BIN_VERIFIEDSALARY
levels(data.pred$WOE_BIN_TENURE)         <- model$forest$xlevels$WOE_BIN_TENURE
levels(data.pred$WOE_BIN_DAYS_SINCE_REG) <- model$forest$xlevels$WOE_BIN_DAYS_SINCE_REG
levels(data.pred$STATUS_ACTIVE)          <- model$forest$xlevels$STATUS_ACTIVE
data.pred$score_nm_ftb <- predict(model, 
                                data.pred[, c("WOE_USERGROUP", "WOE_BIN_DAYS_SINCE_REG", "WOE_BIN_TENURE", "WOE_BIN_VERIFIEDSALARY", "WOE_BIN_CLIENT", "STATUS_ACTIVE", "BALANCE")]
)



data.pred$OUTCOME_TIME
conversion <- function(time)
{
  if(is.na(time)) {return (NA)}
  if (time > 1000) (return (0.001))
  i = which(curve$time == time)
  return(curve[i,]$conv_90)
}
data.pred$baseline_hazard <- sapply(data.pred$OUTCOME_TIME, conversion)
head(data.pred)


# Output data
table <- "mart_delinquency_buyers_score"
data.out <- data.pred[,c("CUSTOMER_ID", 
                         "score_credit_r", 
                         "score_credit_c",
                         "score_credit_f",
                         "score_nm_reg",
                         "score_nm_ftb",
                         "baseline_hazard"
                         )]



con <- openCon()
sqlDrop(channel = con, 
        sqtable = table, 
        errors = FALSE)
sqlSave(channel = con, 
        dat = data.out, 
        tablename = table,
        append   = FALSE,
        rownames = FALSE, 
        colnames = FALSE, 
        verbose  = FALSE,
        safer    = TRUE, 
        addPK    = FALSE, 
        fast     = TRUE, 
        test     = FALSE, 
        nastring = NULL
)
close(con)
