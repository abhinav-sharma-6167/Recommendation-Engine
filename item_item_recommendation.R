# model
#view the theory
item_item_recommendation <- function(panel, views_data, n_recs = 10, n_buys = 3)
{
  colnames(panel) <- c("user", "item", "weight")
  
  #reassigning weights to distinguish carts and sales 
  panel$weight[panel$weight <= 0.25] <- 0.25
  panel$weight[panel$weight >= 0.5] <- 1
  
  #reshaping the table
  user_item <- dcast(panel, user ~ item, max, value.var = "weight", fill = 0)
  #using cosine similarity
  item_item <- data.frame(cosine(as.matrix(user_item[,2:ncol(user_item)])))
  
  item_popularity <- data.frame("item" = colnames(user_item[-1]), "popularity" = colSums(user_item[-1]))
  
  item_neighbours <- data.frame(matrix(NA, nrow=ncol(item_item), ncol=(n_recs+1), dimnames=list(colnames(item_item))))  
  
  #to find similar items,filling up item_neighbours
  for (i in 1:ncol(item_item)) 
  {
    item_neighbours[i,] <- t(head(rownames(item_item[order(item_item[,i],decreasing=T),][i]),n=(n_recs+1)))
  }
  
  users <- data.frame(matrix(NA, nrow = nrow(user_item), ncol = n_buys))
  for (i in 1:nrow(users))
  {
    for (j in 1:n_buys)
    {
      names(users)[j] <- paste0("item_", j)
      users[i,j] <- rev(tail(panel$item[panel$user == user_item$user[i]],n=n_buys))[j]
    }
  }
  users$user <- user_item$user
  
  for (i in 1:n_buys)
  {
    names(item_neighbours)[1] <- paste0("item_", i)
    users <- merge(users, item_neighbours, all.x = T, by = c(paste0("item_", i)))
  }
  
  #generating recommendations
  ldf <- lapply(unique(users$user), function(k)
                {
                  rec <- data.frame(t(subset(users, user == k, select = -c(user))))
                  names(rec)[1] <- "item"
                    
                  rec <- unique(subset(rec, !item %in% panel$item[panel$user == k & panel$weight == 1]))
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


