# raw data
filename_sales <- grep(today_date, grep("sale", list.files(raw_path), value = T), value = T)
filename_carts <- grep(today_date, grep("cart", list.files(raw_path), value = T), value = T)
filename_views <- grep(today_date, grep("pageview", list.files(raw_path), value = T), value = T)
filename_customers <- grep(today_date, grep("customer", list.files(raw_path), value = T), value = T)
filename_tags <- grep(today_date, grep("tagged", list.files(raw_path), value = T), value = T)
filename_interactions <- grep(today_date, grep("imageview", list.files(raw_path), value = T), value = T)

setwd(raw_path)
sales_data <- read.csv(filename_sales, stringsAsFactors = F)
carts_data <- read.csv(filename_carts, stringsAsFactors = F)
views_data <- read.csv(filename_views, stringsAsFactors = F)
customer_data <- read.csv(filename_customers, stringsAsFactors = F)
tags_data <- read.csv(filename_tags, stringsAsFactors = F)
interactions_data <- read.csv(filename_interactions, stringsAsFactors = F)

cat("Sales Data has ", nrow(sales_data), " rows\n", sep = "")
cat("Carts Data has ", nrow(carts_data), " rows\n", sep = "")
cat("Views Data has ", nrow(views_data), " rows\n", sep = "")
cat("Customer Data has ", nrow(customer_data), " rows\n", sep = "")
cat("Tags Data has ", nrow(tags_data), " rows\n", sep = "")
cat("Interaction Data has ", nrow(interactions_data), " rows\n\n", sep = "")


# customer data
customer_data <- subset(customer_data, select = c("customer_id", "user_name", "gender"))
names(customer_data)[2] <- "user"
customer_data$customer_id <- as.numeric(customer_data$customer_id)
customer_data$gender <- tolower(customer_data$gender)
customer_data$gender[customer_data$gender == 'null'] <- 'not_specified'

gender_data <- unique(customer_data[,2:3])
customer_data <- unique(customer_data[,1:2])

customer_data <- customer_data[regexpr("@", customer_data$user) > 0,]
customer_data <- customer_data[order(customer_data$customer_id),]
customer_data <- customer_data[!duplicated(customer_data[,"user"]),]


# tags data
tags_data <- unique(subset(tags_data, select = c("tag_name", "product_id")))


# sales, carts, views
colnames(carts_data) <- tolower(colnames(carts_data))
colnames(sales_data) <- tolower(colnames(sales_data))
colnames(views_data) <- tolower(colnames(views_data))

carts_name<-(carts_data$name)
sales_name<-(sales_data$name)
carts_id <- (carts_data$product_id)
sales_id <- (sales_data$product_id)

carts_data$date <- as.Date(substr(carts_data$date_added,1,10))
sales_data$date <- as.Date(paste0(substr(sales_data$date_key,1,4),"-",substr(sales_data$date_key,5,6),"-",substr(sales_data$date_key,7,8)))
views_data$date <- as.Date(substr(views_data$date,1,10))


carts_data <- carts_data[!is.na(carts_data$date),]
sales_data <- sales_data[!is.na(sales_data$date),]
views_data <- views_data[!is.na(views_data$date),]

carts_data <- carts_data[order(carts_data$date, decreasing = T),]
sales_data <- sales_data[order(sales_data$date, decreasing = T),]
views_data <- views_data[order(views_data$date, decreasing = T),]

carts_data$email[carts_data$email == "NULL"] <- carts_data$customer_id[carts_data$email == "NULL"]
carts_data <- carts_data[regexpr("@", carts_data$email) > 0,]
carts_data$count <- 1

sales_data$email[sales_data$email == "NULL"] <- sales_data$customer_id[sales_data$email == "NULL"]
sales_data <- sales_data[regexpr("@", sales_data$email) > 0,]
sales_data$count <- 1

carts_data$name <- tolower(carts_data$name)
sales_data$name <- tolower(sales_data$name)

carts_data <- carts_data[-c(grep("anti-bottle", carts_data$name)),]
sales_data <- sales_data[-c(grep("anti-bottle", sales_data$name)),]

carts_data <- subset(carts_data, select = c("email", "product_id"))
sales_data <- subset(sales_data, select = c("email", "product_id"))

views_data <- views_data[,1:3]
colnames(views_data) <- c("date", "customer_id", "product_id")

views_data$product_id <- as.numeric(gsub("\\D", "", views_data$product_id))
views_data <- subset(views_data, product_id %in% carts_data$product_id | product_id %in% sales_data$product_id)
views_data <- merge(views_data, customer_data, by = c("customer_id"))

views_data <- views_data[regexpr("@", views_data$user) > 0,]
views_data <- views_data[order(views_data$date, decreasing = T),]
views_data <- subset(views_data, select = c("user", "product_id"))
colnames(views_data)[1] <- "email"

carts_data <- unique(carts_data)
sales_data <- unique(sales_data)
views_data <- unique(views_data)

test_data <- rbind(carts_data, sales_data, views_data)
test_data <- subset(test_data, select = -c(product_id))
names(test_data)[1] <- "user"
test_data <- unique(test_data)

test_data <- merge(test_data, gender_data, all.x = T, by = c("user"))
test_data$gender[is.na(test_data$gender)] <- "not_specified"


# interactions data
colnames(interactions_data) <- c("date", "customer_id", "product_id")
interactions_data$product_id <- as.numeric(gsub("\\D", "", interactions_data$product_id))
interactions_data <- subset(interactions_data, product_id %in% carts_data$product_id | product_id %in% sales_data$product_id)
interactions_data <- merge(interactions_data, customer_data, by = c("customer_id"))
interactions_data <- interactions_data[regexpr("@", interactions_data$user) > 0,]
interactions_data <- unique(interactions_data)

setwd(main_path)

