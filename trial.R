library(tm)#for text mining
library(data.table)

product <- cbind(c(carts_id,sales_id),c(carts_name,sales_name))
product<-as.data.table(product)
names(product)<-c("id","name")
product<-unique(na.omit(product[order(name,id)]))
setkey(product,id)
product<-unique(product)


words_rm <-unique(c(stopwords(kind = "en")))
corpus1=Corpus(DataframeSource(as.data.frame(product$name)))
corpus1=tm_map(corpus1, removePunctuation)
corpus1=tm_map(corpus1, removeWords, words_rm)#removing the most common words

product$name<-(as.data.table(data.frame(text=unlist(sapply(corpus1, `[`, "content")), stringsAsFactors=FALSE)))
product$name<-tolower(product$name)

########ignore_below############################################################################
# trial_name1 <-product$name[1:100]
# trial_name2 <-product$name[99:199]

#pmatch : {base} number(exact matches).......pmatch(trial_name1, trial_name2, nomatch = NA_integer_, duplicates.ok = FALSE)
#amatch : {stringdist} returns maximum matching not matrix.......amatch(trial_name1,trial_name2, nomatch = NA_integer_, matchNA = TRUE, method =  "cosine",useBytes = FALSE, maxDist = 0.6)
#........mmm<-stringdistmatrix(trial_name1, trial_name2, method ="cosine",maxDist = 1,ncores = 1)
#################################################################################################

distance_matrix<-as.data.table(stringdistmatrix(product$name,product$name, method ="cosine",maxDist = 1,ncores = 1))


distance_matrix<-as.data.frame(distance_matrix)

#popularity <- data.frame("item" = colnames(distance_matrix[-1]), "popularity" = colSums(distance_matrix[-1]))

recommendations <- data.frame(matrix(NA, nrow=ncol(distance_matrix), ncol=(n_recs+1), dimnames=list(colnames(distance_matrix))))  

for (i in 1:ncol(distance_matrix)) 
{
  recommendations[i,] <- t(head(rownames(distance_matrix[order(distance_matrix[,i],decreasing=T),][i]),n=(n_recs+1)))
}

recommendations <-cbind(product, as.data.table(recommendations))
recommendations$srno <- 1:nrow(recommendations)

#the recommendations give a 16.5k X 21 matrix giving the recommendations based on the algorithm 
#these recommendations are not product ids but the serial number as appearing in the last row
