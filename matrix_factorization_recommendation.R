# loading libraries
library(NMF)#used specifically for Matrix Factorization
library(plyr)#reshaping data..functions like dcast


# model
matrix_factorization_recommendation <- function(panel, n_recs = 10, n_features = 5)
{
  colnames(panel) <- c("user", "item", "weight")
  
  user_item <- dcast(panel, user ~ item, max, value.var = "weight", fill = 0)
  #similar as in item item code
  item_popularity <- data.frame("item" = colnames(user_item[-1]), "popularity" = colSums(user_item[-1]))
  
  #applying matrix factorization using nmf function
  x <- nmf(as.matrix(user_item[,-1]), n_features, method = "lee")
  y <- fitted(x)
  
  mf <- data.frame(cbind(user_item$user, y))
  names(mf)[1] <- "user"
  names(mf)[2:ncol(mf)] <- colnames(user_item[,-1])
  
  #generationg recommendations
  ldf <- lapply(mf$user, function(k)
                {
                  rec <- data.frame(t(subset(mf, user == k, select = -c(user))), row.names = colnames(mf[,-1]))
                  names(rec)[1] <- "value"
                  rec$value <- as.numeric(as.character(rec$value))
                  rec$count <- 1
                    
                  rec <- data.frame("item" = as.numeric(head(rownames(rec[order(rec$value, decreasing =T),]),n=n_recs*n_features)))
                    
                  rec <- subset(rec, !item %in% panel$item[panel$user == k & panel$weight >= 0.5])
                  rec <- subset(rec, !item %in% interactions_data$product_id[interactions_data$user == k])
                  rec <- merge(rec, item_popularity, by = c("item"))
                    
                  rec <- head(rec[order(rec$popularity,decreasing=T),],n=n_recs)
                    
                  if (nrow(rec) > 0)
                  {
                    return(data.frame("user" = rep(k, nrow(rec)),
                                      "item" = rec$item,
                                      "rank" = seq(1, nrow(rec))))
                  }else{return(NULL)}
                })
  
  output <- ldply(ldf, data.frame)
  
  return(output)  
} 
