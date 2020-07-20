
df <- data.frame()
a <- data_train[,c("OUTCOME_TIME", "OUTCOME_CENSORED")]
for (k in 0:200){
  
  dem <- sum(a$OUTCOME_TIME >= k)
  num <- sum(a$OUTCOME_TIME == k & a$OUTCOME_CENSORED == 1)
  row <- c(k, dem, num, num/dem); names(row) <- c("k", "dem", "num", "haz");
  df <- rbind(df, row) 
}
names(df) <- c("k", "dem", "num", "haz")
head(df)

srv <- c()
for (k in 0:200){
  
  srv <- c(srv, prod(1- df[1:(k+1),]$haz))
}
df$srv <- srv

df$chaz <- -log(df$srv)
head(df)


a <- basehaz(cox, centered=FALSE)