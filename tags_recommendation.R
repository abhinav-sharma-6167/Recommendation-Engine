# using tags names for recommendations, not many changes
tags_recommendation <- function(panel, tags_data)
{
  colnames(panel) <- c("user", "item", "weight")
  panel$weight <- 1
  panel$count <- 1
  
  colnames(tags_data) <- c("tag", "item")
  
  panel <- merge(panel, tags_data, by = c("item"))
  
  sales <- panel
  
  d <- dcast(panel, user ~ count, sum, value.var = "count")
  names(d)[2] <- "fre"
  d <- subset(d, fre >= 5)
  
  panel <- subset(panel, user %in% d$user)
  
  d1 <- dcast(panel, user + tag ~ count, sum, value.var = "count")
  names(d1)[3] <- "fre"
  d1 <- d1[order(d1$fre, decreasing = T),]
  d1 <- d1[!duplicated(d1[,"user"]),]
    
  d2 <- dcast(sales, tag + item ~ count, sum, value.var = "count")
  names(d2)[3] <- "fre"
  d2 <- d2[order(d2$fre, decreasing = T),]
  
  ldf <- lapply(unique(panel$user), function(k)
                {
                  rec <- subset(d2, tag == d1$tag[d1$user == k])
                  rec <- subset(rec, !item %in% sales$item[sales$user == k])
                  rec <- subset(rec, !item %in% interactions_data$product_id[interactions_data$user == k])
                  
                  if (nrow(rec) > 10)
                  {
                    return(data.frame("user" = rep(k,10),
                                      "item" = rec$item[1:10],
                                      "rank" = seq(1,10)))                    
                  }
                  else{return(NULL)}
                })
  
  output <- ldply(ldf, data.frame)
  
  return(output)
}
