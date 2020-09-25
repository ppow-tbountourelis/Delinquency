# Libraries
source("db.R")
library(survival)
library(dplyr)
library(rpart)
library(naivebayes)
datapath <- "C:/Users/tbountourelis/OneDrive - PURCHASING POWER, LLC/R Projects/Data/"

# Load Objects
load(paste(datapath, "model_Buyers.RData", sep = ""))

# Read Data
query <- 'select * from mart_delinquency_lookup'
data.pred <- exeQueryString (query, stringsAsFactors = TRUE)

# Get model
#model <- models[["Registrants - v2"]]
# Get current score
#data.pred$logistic <- predict(model, 
#                              data.pred, 
#                              type = "response"
#)


# Get model
model <- models[["Registrants - Bayes - v2"]]

# Get current score
#data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_C 
#data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_C
data.pred$bayes <- predict(model, 
                              data.pred, 
                              type = "prob"
)



# Get current score
#data.pred$WOE_BIN_DAYS_SINCE_REG <- data.pred$WOE_BIN_DAYS_SINCE_REG_C 
#data.pred$WOE_BIN_TENURE <- data.pred$WOE_BIN_TENURE_C
model <- models[["Registrants - Bayes - High Risk"]]
a <- predict(model, 
             data.pred, 
             type = "prob"
)
data.pred$bayes_group_high <- a[,2]

model <- models[["Registrants - Bayes - Low Risk"]]
a <- predict(model, 
             data.pred, 
             type = "prob"
)
data.pred$bayes_group_low <- a[,2]

data.pred$buyes_group <- ifelse(data.pred$CLIENT_RISK_GROUP == 'High Risk', data.pred$bayes_group_high, data.pred$bayes_group_low)



# Output data
table <- "mart_delinquency_lookup_out"
con <- openCon()
sqlDrop(channel = con, 
        sqtable = table, 
        errors = FALSE)
sqlSave(channel = con, 
        dat = data.pred, 
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
