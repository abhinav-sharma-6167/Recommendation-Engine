# just finding the most sought after product
popularity_recommendation <- function(panel, n_recs = 10)
{
  colnames(panel) <- c("user", "item", "weight")
  panel$weight <- 1
  panel$count <- 1
  
  panel <- merge(panel, gender_data, all.x = T, by = c("user"))
  panel$gender[is.na(panel$gender)] <- "not_specified"
  
  d <- dcast(panel, gender + item ~ count, sum, value.var = "weight")
  names(d)[3] <- "popularity"
  
  d_male <- subset(d, gender == "male")
  d_female <- subset(d, gender == "female")
  d_not <- subset(d, gender == "not_specified")
  
  popular_items_male <- head(d_male$item[order(d_male$popularity,decreasing=T)],n=100)
  popular_items_female <- head(d_female$item[order(d_female$popularity,decreasing=T)],n=100)
  popular_items_not <- head(d_not$item[order(d_not$popularity,decreasing=T)],n=100)
  
  output <- data.frame("user" = rep(panel$user[1],300),
                       "gender" = c(rep("male",100),rep("female",100),rep("not_specified",100)),
                       "item" = c(popular_items_male,popular_items_female,popular_items_not),
                       "rank" = rep(seq(1, 100),3))
  
  return(output)
}

