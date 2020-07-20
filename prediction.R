# Read Data
query <- 'select * from mart_ftb_test'
data_test <- exeQueryString (query, stringsAsFactors = TRUE)

###
data_test$baseline_hazard <- sapply(data_test$TEST_TIME, conversion)
data_test$risk            <- predict(cox, newdata = data_test, type = 'risk') #hazard ratio
data_test$score           <- data_test$baseline_hazard * data_test$risk
###

data_test$validation <- ifelse(!is.na(data_test$TEST_DAYS_TO_EVENT) & data_test$TEST_DAYS_TO_EVENT <= 90, 1, 0)


breaks <- seq(0, 1, 0.05)
data_test$score_bins <- cut(data_test$score, 
                            breaks=breaks, 
                            include.lowest=TRUE, 
                            right=FALSE 
                            #labels=tags
                            )

aggregate(data_test$validation, by=list(data_test$score_bins), FUN=mean)

df <- data[1:100,]

query <- 'drop table "test69"'
a <- exeQueryString (query, stringsAsFactors = TRUE)
con <- openCon()
sqlSave(channel = con, 
        dat = df, 
        tablename = "test69", 
        append = FALSE, 
        rownames = TRUE, 
        colnames = TRUE, 
        verbose = FALSE, 
        safer = TRUE, 
        addPK = FALSE, 
        fast = TRUE
        )
close(con)
