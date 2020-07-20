# Libraries
source("db.R")
library(smbinning)

# Load Objects
load("predict.RData")

# Read Data
query <- 'select * from v_mart_delinquency_pred_daily'
data.pred <- exeQueryString (query, stringsAsFactors = TRUE)

# Convert to factors certain columns
col_names <- c("ORDER_DT_MONTH", "ORDER_DT_HOUR")
data.pred[col_names] <- lapply(data.pred[col_names] , factor)

# Predict
model <- models[["Orders"]]
data.pred$score <- predict(model, 
                           data.pred, 
                           type = "response"
)

# Output data
table <- "mart_delinquency_score_daily"
data.out <- data.pred[data.pred$PREDICT == 1, c("ORDER_ID", "CUSTOMER_ID", "ORDER_DT", "STATUS", "ORIGINAL", "DEM", "DIS", "VERIFIEDSALARY", "TENURE_ORDER", "score")]
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
        nastring = NULL,
        varTypes=c(ORDER_DT="varchar(30)")
)
close(con)