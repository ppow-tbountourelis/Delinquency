# Libraries
source("db.R")

# Load Objects
load("predict.RData")

# Read Data
query <- 'select * from v_mart_delinquency_pred_buyers'
data.pred <- exeQueryString (query, stringsAsFactors = TRUE)

# Get model
model <- models[["Registrants"]]

# Predict
data.pred$score <- predict(model, 
                           data.pred, 
                           type = "response"
)

# Output data
table <- "mart_delinquency_buyers_score"
data.out <- data.pred[,c("CUSTOMER_ID", "CLIENT_ID", "EMAIL", "TENURE", "score")]
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
        #varTypes=c(ORDER_DT="varchar(30)")
)
close(con)

# Analytics
#dat <- data.pred[!is.na(data.pred$SPENDING_LIMIT_REGISTRATION),]
#summary(dat$score)
#density(dat$score, na.rm = TRUE)
#dat <- dat[order(dat$score),]
#plot(dat$score) 
#plot(sort(dat$SPENDING_LIMIT_REGISTRATION))