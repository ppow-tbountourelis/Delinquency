# Read Data
query <- 'select * from v_mart_delinquency_pred'
data.pred <- exeQueryString (query, stringsAsFactors = TRUE)
data.pred <- as.data.frame(unclass(data.pred), stringsAsFactors=TRUE)
data.pred$ORDER_DT_MONTH <- as.factor(data.pred$ORDER_DT_MONTH)
data.pred$ORDER_DT_HOUR  <- as.factor(data.pred$ORDER_DT_HOUR)

# Prepare data
nms <- c("DEM", 
         "DIS", 
         "TIME_BETWEEN_REG_FIRST", 
         "TENURE_FIRSTORDER",      
         "ELIGIBLES",              
         "VERIFIEDSALARY",
         "STATE_WOA"
         )

for (var in nms)
{
    data.pred <- smbinning.gen(df      = data.pred, 
                               ivout   = lst.smb[[var]], 
                               chrname = paste(var, "_SMBIN", sep="")
                               )
}

# Score orders
data.pred$score <- predict(logitMod, 
                           data.pred, 
                           type = "response"
                           )  
i <- !is.na(data.pred$score)
data.pred <- data.pred[i,]
source("rpart.R")


# Output data
con <- openCon()
sqlDrop(channel = con, 
        sqtable = "mart_delinquency_score", 
        errors = FALSE)
sqlSave(channel = con, 
        dat = data.pred[, c("ORDER_ID", "PREDICT", "score", "client_group")], 
        tablename = "mart_delinquency_score",
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

#a <- predict(logitMod, data.pred[data.pred$CLIENT_ID == 2399,],  type = "response") 


