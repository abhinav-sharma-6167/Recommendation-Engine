# setting working directory
# main_path <- "/home/sqs64/rec_engine_scripts"
# raw_path <- "/apps/64sqs/today"
# output_path <- "/apps/64sqs/today/recos"



#all the paths required to load and store data
main_path <- "D:/Work/Ahalife/rec_engine_scripts"
raw_path <- "D:/Work/Ahalife"
output_path <- "D:/Work/Ahalife"

setwd(main_path)


# parameters that can to tweaked
today_date <- gsub("-", "", as.character(as.Date(Sys.time())))
# log_file <- paste0("/home/sqs64/logs/log_rec_", today_date, ".txt")
log_file <- paste0("D:/Work/Ahalife", today_date, ".txt")
#initializing a connection
sink(log_file)
cat("Model Start Time: ", as.character(Sys.time()), "\n\n", sep = "")
start_time <- Sys.time()

#avoiding scientific notation and warnings
options(scipen = 999, warn = -1)

#Paramaeters
n_recs <- 20             # (number of recommended items to be shown)
n_features <- 3          # (number of features for matrix factorization)
n_buys <- 3              # (number of purchases for item-item recommendation)
bought_cutoff <- 3       # (customers who bought lesser number of products than this are excluded) 
sold_cutoff <- 2         # (items that sold lesser number of times than this are excluded)


# loading libraries

suppressPackageStartupMessages(library("lsa")) 
suppressPackageStartupMessages(library("NMF")) 
library(lsa)
library(NMF)
library(plyr)
library(reshape2)


# preparing data
cat("Preparing Data\n")
source("data_preparation.R")




carts_data$wt <- seq(1, nrow(carts_data))
sales_data$wt <- seq(1, nrow(sales_data))

#assigning weights based on more recent
carts_data$wt <- 0.1 + ((carts_data$wt - min(carts_data$wt))*0.15 / (max(carts_data$wt) - min(carts_data$wt)))
sales_data$wt <- 0.5 + ((sales_data$wt - min(sales_data$wt))*0.5 / (max(sales_data$wt) - min(sales_data$wt)))

#making final panel data to be used
panel <- rbind(carts_data, sales_data)
panel$count <- 1


# removing customers
d <- dcast(panel, email ~ count, sum, value.var = "count")
names(d)[2] <- "units_bought"
d <- subset(d, units_bought >= bought_cutoff)

panel <- subset(panel, email %in% d$email)


# removing items
d <- dcast(panel, product_id ~ count, sum, value.var = "wt")
names(d)[2] <- "units_sold"
d <- subset(d, units_sold >= sold_cutoff)

panel <- subset(panel, product_id %in% d$product_id)

panel <- unique(panel)


# item-item recommendation
cat("Building Item-Item Recommendation\n")
source("item_item_recommendation.R")
item_rec <- item_item_recommendation(panel, views_data, n_recs, n_buys)


# matrix factorization recommendation
cat("Building Matrix-Factorization Recommendation\n")
source("matrix_factorization_recommendation.R")
mf_rec <- matrix_factorization_recommendation(panel, n_recs, n_features)


# tags recommendation
cat("Building Tags-Based Recommendation\n")
source("tags_recommendation.R")
tags_rec <- tags_recommendation(sales_data, tags_data)


# popularity recommendation
cat("Extracting Most Popular Items\n\n")
source("popularity_recommendation.R")
popularity_rec <- popularity_recommendation(sales_data, n_recs)


# final recommendation
cat("Creating Final Recommendations\n")
ldf <- lapply(unique(test_data$user), function(k)
              {
                if (regexpr("@", k) > 0)
                {
                  #print(k)
                  
                  final <- popularity_rec[popularity_rec$gender == test_data$gender[test_data$user == k][1],]
                  final <- subset(final, !item %in% sales_data$item[sales_data$user == k], select = -c(gender))
                  #assigning number of recommendations to be taken from each algorithm
                  if (nrow(item_rec[item_rec$user == k,]) > 0)
                  {
                    if (nrow(tags_rec[tags_rec$user == k,]) > 0)
                    {
                      #number of items from popularity algo
                      final1 <- final[1:2,]
                      
                      #items from item-item recommendations
                      final2 <- item_rec[item_rec$user == k & !item_rec$item %in% final1$item,]
                      final2 <- final2[1:6,]
                      
                      #items from tags recommendations
                      final3 <- tags_rec[tags_rec$user == k & !tags_rec$item %in% final1$item
                                         & !tags_rec$item %in% final2$item,]
                      final3 <- final3[1:8,]
                      #items from matrix factorization recommendations
                      final4 <- mf_rec[mf_rec$user == k & !mf_rec$item %in% final1$item
                                       & !mf_rec$item %in% final2$item
                                       & !mf_rec$item %in% final3$item,]
                      final4 <- final4[1:7,]
                      
                      #remaining items from item-item recommendations
                      final5 <- item_rec[item_rec$user == k & !item_rec$item %in% final1$item
                                         & !item_rec$item %in% final2$item
                                         & !item_rec$item %in% final3$item
                                         & !item_rec$item %in% final4$item,]
                      
                      final <- rbind(final2,final3,final4,final5)                                      
                    }else
                    {
                      final1 <- final[1:2,]
                      
                      final2 <- item_rec[item_rec$user == k & !item_rec$item %in% final1$item,]
                      final2 <- final2[1:7,]
                      
                      final3 <- mf_rec[mf_rec$user == k & !mf_rec$item %in% final1$item
                                       & !mf_rec$item %in% final2$item,]
                      final3 <- final3[1:8,]
                      
                      final4 <- item_rec[item_rec$user == k & !item_rec$item %in% final1$item
                                         & !item_rec$item %in% final2$item
                                         & !item_rec$item %in% final3$item,]
                      
                      final <- rbind(final2,final3,final4)
                    }                    
                  }
                   
                  #final recommendations                 
                  return(data.frame("user" = k,
                                    "rec1" = final$item[1],
                                    "rec2" = final$item[2],
                                    "rec3" = final$item[3],
                                    "rec4" = final$item[4],
                                    "rec5" = final$item[5],
                                    "rec6" = final$item[6],
                                    "rec7" = final$item[7],
                                    "rec8" = final$item[8],
                                    "rec9" = final$item[9],
                                    "rec10" = final$item[10],
                                    "rec11" = final$item[11],
                                    "rec12" = final$item[12],
                                    "rec13" = final$item[13],
                                    "rec14" = final$item[14],
                                    "rec15" = final$item[15],
                                    "rec16" = final$item[16],
                                    "rec17" = final$item[17],
                                    "rec18" = final$item[18],
                                    "rec19" = final$item[19],
                                    "rec20" = final$item[20],
                                    "rec21" = final$item[21],
                                    "rec22" = final$item[22],
                                    "rec23" = final$item[23],
                                    "rec24" = final$item[24],
                                    "rec25" = final$item[25],
                                    "rec26" = final$item[26],
                                    "rec27" = final$item[27],
                                    "rec28" = final$item[28],
                                    "rec29" = final$item[29],
                                    "rec30" = final$item[30]))                  
                }else{return(NULL)}
              })

output1 <- do.call(rbind, ldf)

other_users <- subset(customer_data, !user %in% output1$user, select = c("user"))
other_users <- merge(other_users, gender_data, all.x = T, by = c("user"))
other_users$gender[is.na(other_users$gender)] <- "not_specified"

ldf <- lapply(unique(other_users$gender), function(k)
              {
                rec <- subset(other_users, gender == k, select = c("user"))
                
                for (i in 1:n_recs)
                {
                  S <- paste0("rec$rec", i, " <- popularity_rec$item[popularity_rec$gender == k][i]")
                  eval(parse(text=S))
                }
                
                return(rec)
              })

output2 <- ldply(ldf, data.frame)


# output file
cat("Creating Final Output File\n\n")
output <- rbind(output1, output2)
output <- merge(output, customer_data, by = c("user"))
output <- subset(output, select = c("customer_id", grep("rec", colnames(output), value = T)))
output <- output[complete.cases(output),]
output <- output[order(output$customer_id),]

setwd(output_path)
write.csv(output, paste0("64sqs-output2-",today_date,".csv"), row.names = F)


# model statistics
cat("Top-", n_recs, " recommendations generated for ", nrow(output), " users\n\n", sep = "")


# run time
cat("Model End Time: ", as.character(Sys.time()), "\n\n", sep = "")
cat("Total Time Taken: ", round(as.numeric(difftime(Sys.time(), start_time, units = "mins"))), " minutes", sep = "")

