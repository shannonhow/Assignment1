#####################################
#                                   #
#           Reading Data            #
#                                   #
#####################################

library(stringr)
retail_data <- read.csv("online_retail.csv",header=TRUE)
head(retail_data)
dim(retail_data) #contains 541909 obs, 8 columns
length(unique(retail_data$CustomerID)) #4373 unique customer IDs



#####################################
#                                   #
#          Data Cleaning            #
#                                   #
#####################################

#Convert CustomerID to factor
retail_data$CustomerID <- as.factor(retail_data$CustomerID)

#Convert Invoice date to character class first 
invoicedate <- as.character(retail_data$InvoiceDate)

#Creating a Date & Time column 
retail_data$Time <- format(as.POSIXct(invoicedate,format="%d/%m/%Y %H:%M"),"%H:%M")
retail_data$Date <- format(as.POSIXct(invoicedate,format="%d/%m/%Y %H:%M"),"%Y-%m-%d")
retail_data$InvoiceDate <- NULL

#Creating a TotalSpent column
retail_data$TotalSpent = retail_data$Quantity * retail_data$UnitPrice

#Exploring the occurence for each items
number <- table(retail_data$Description)
number <- data.frame(number)
colnames(number) <- c('Desciption', 'Freq')
number <- number[sort(number$Desciption),]

#remove all the problematic descriptions
number2 <- number[-c(14:28,58:59,152:156,167,174,179:186,321,323,598,603,649,655,695,
                     742:744,883,918,921,954:957,987,1001:1013,1061,1069:1070,1137:1151,
                     1153,1196,1293,1297,1418,1420:1426,1536,1681:1682,1790,1836:1838,
                     1839:1840,1901,1964,2068,2085:2087,2145:2146,2155,2170,2187,
                     2188,2256:2258,2261,2263,2288:2290,2312,2343,2378,2380,2465,2817:2818,
                     2830:2831,2877:2879,3031:3033,3113:3114,3132,3134:3136,3434:3435,
                     3570,3582:3590,3635:3636,3703:3704,3728:3729,3734:3738,3739,3744,
                     3835,3976:3978,3986,3989:3997,4167:4181),]
new_retail_data <- retail_data[retail_data$Description %in% number2[,1],]

missingCust <- new_retail_data %>% filter(is.na(CustomerID))
knownCust <- new_retail_data %>% filter(!is.na(CustomerID))
intersect(missingCust$InvoiceNo,knownCust$InvoiceNo)

#Since the intersect of the InvoiceNo for those with missing and known CustomerID is empty,
# there are no rows with missing CustomerID that can be filled using InvoiceNo.

#Below we split the dataset into those with descriptions and those without. 
missingDctn <- new_retail_data %>% filter(Description == "")
retail_data_w_description <- new_retail_data %>% filter(Description != "")
intersect(missingDctn$StockCode, retail_data_w_description$StockCode)

retail_data_DescandCID <- retail_data_w_description %>% filter(!is.na(CustomerID))
retail_data_w_description_wo_CID <- retail_data_w_description %>% filter(is.na(CustomerID))
#retail_data_w_description is split into those with CID and those without.
#retail_data_DescandCID is data that has BOTH Description and CID. (Use for Analysis where CustomerID is needed)


#For the data without descriptions, we fill in the descriptions using StockCode.
missingDctn %>% filter(!is.na(CustomerID))
#There are no products that are missing description and have a CustomerID.
dim(missingDctn)
#1454 rows without Descriptions

unknownDctn <- unique(subset(missingDctn,missingDctn$Description == "", "StockCode"))
foundDctn <- unique(subset(new_retail_data,StockCode %in% unknownDctn$StockCode & new_retail_data$Description != ""))
for(i in unknownDctn$StockCode) {
  if (i %in% foundDctn$StockCode) {
    missingDctn[missingDctn$StockCode == i,"Description"] <- subset(foundDctn,StockCode %in% i,"Description")[1,]
  }  
}
missingDctn %>% filter(Description == "") %>% dim()
#There are still 124 rows without Descriptions.

#Removing rows without descriptions
retail_filled_descriptions <- missingDctn %>% filter(Description != "")

retail_filled_descriptions %>% filter(UnitPrice == 0) %>% dim()
retail_filled_descriptions %>% dim()
#All 1330 rows in retail_filled_description have UnitPrice = 0.

unknownPrice <- unique(subset(retail_filled_descriptions,retail_filled_descriptions$UnitPrice == 0, "StockCode"))
foundPrice <- unique(subset(new_retail_data,StockCode %in% unknownPrice$StockCode & new_retail_data$UnitPrice != 0)) %>% arrange(StockCode,desc(Date, Time))
for(i in unknownPrice$StockCode) {
  if (i %in% foundPrice$StockCode) {
    retail_filled_descriptions[retail_filled_descriptions$StockCode == i,"UnitPrice"] <- subset(foundPrice,StockCode %in% i,"UnitPrice")[1,]
  }  
}

retail_filled <- retail_filled_descriptions %>% filter(UnitPrice!= 0)
#This is the data with filled descriptions and price

#Update the TotalSpent column
retail_filled$TotalSpent = retail_filled$Quantity * retail_filled$UnitPrice

retail_wo_CID <- rbind(retail_filled, retail_data_w_description_wo_CID)
#This is the all the data without CustomerID.
#Combine this with retail_data_DescandCID to obtain data that can be used for analysis that does not require CustomerID

#Removing those with very large quantity (>12000)
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity <= 12000)

#Removing those with quantity > 1600 & Unitprice==0
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity<1600 && UnitPrice!=0)

#Removing those with InvoiceNo. starting with "C"
retail_data_wo_cancelled <- retail_data_DescandCID %>% filter(!str_detect(InvoiceNo,"C"))

#Converting the Date column to a "DATE" class.
retail_data_DescandCID$Date <- as.Date(retail_data_DescandCID$Date) #for M
retail_data_wo_cancelled$Date <- as.Date(retail_data_wo_cancelled$Date) #for R & F 

#(start)dec 2010 - march 2011(end)
rfm_data_1_M <- retail_data_DescandCID %>% filter(Date <= as.Date("2011-03-31"))
rfm_data_1_RF <- retail_data_wo_cancelled %>% filter(Date <= as.Date("2011-03-31"))

#(start)april 2011 - july 2011(end)
rfm_data_2_M <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 
rfm_data_2_RF <- retail_data_wo_cancelled %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 

#(start)aug 2011 - dec 2011(end)
rfm_data_3_M <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-07-01"))
rfm_data_3_RF <- retail_data_wo_cancelled %>% filter(Date >= as.Date("2011-07-01"))

#####################################
#                                   #
#            RFM Model              #
#                                   #
#####################################


#FIRST PERIOD: Dec 2010 - March 2011

# The following code sorts the transactions by date.
rfm_data_1_RF <- rfm_data_1_RF[order(rfm_data_1_RF$Date),]
rfm_data_1_M <- rfm_data_1_M[order(rfm_data_1_M$Date),]

head(rfm_data_1_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_1 <- NULL
total_amount_1 <- NULL
since_prev_1 <- NULL

for (id in unique(rfm_data_1_RF$CustomerID)) {
  total_visits_1 <- c(total_visits_1, sum(rfm_data_1_RF$CustomerID == id))
  since_prev_1 <- c(since_prev_1, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_1_RF$Date[rfm_data_1_RF$CustomerID == id])))
  total_amount_1 <- c(total_amount_1, sum(rfm_data_1_M$TotalSpent[rfm_data_1_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_1 <- data.frame(id=unique(rfm_data_1_RF$CustomerID),
                              total_visits=total_visits_1,
                              total_amount=total_amount_1,
                              since_prev=since_prev_1)
head(customer_data_1)
################################################################################
customers_1 <- data.frame(cid = unique(customer_data_1$id))
# The following commands assign the recency, frequency, and monetary value
# rating on a scale of 1-5 with 5 being the most recent, most frequent, most
# monetary value, and 1 being the least recent, least frequent and least
# monetary value.

# New R, F, M-score assigning function.
map_quantiles <- function(vect, num_groups=5) {
  ranks <- rank(vect)
  result <- numeric(length(vect))
  one_unit <- floor(length(vect) / num_groups)
  for (index in 1:num_groups) {
    if (index == num_groups) {
      result[(index - 1) * one_unit < ranks] <- index
    } else {
      result[(index - 1) * one_unit < ranks & ranks <= index * one_unit] <- index
    }
  }
  result
}

customers_1$recency <- 6 - map_quantiles(customer_data_1$since_prev)

customers_1$frequency <- map_quantiles(customer_data_1$total_visits)

customers_1$amount <- map_quantiles(customer_data_1$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_1$rfm <- (customers_1$recency*100
                    + customers_1$frequency*10
                    + customers_1$amount)
head(customers_1)




################################################################################################
# SECOND PERIOD: April 2011 - July 2011


# The following code sorts the transactions by date.
rfm_data_2_RF <- rfm_data_2_RF[order(rfm_data_2_RF$Date),]
rfm_data_2_M <- rfm_data_2_M[order(rfm_data_2_M$Date),]

head(rfm_data_2_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_2 <- NULL
total_amount_2 <- NULL
since_prev_2 <- NULL

for (id in unique(rfm_data_2_RF$CustomerID)) {
  total_visits_2 <- c(total_visits_2, sum(rfm_data_2_RF$CustomerID == id))
  since_prev_2 <- c(since_prev_2, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_2_RF$Date[rfm_data_2_RF$CustomerID == id])))
  total_amount_2 <- c(total_amount_2, sum(rfm_data_2_M$TotalSpent[rfm_data_2_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_2 <- data.frame(id=unique(rfm_data_2_RF$CustomerID),
                              total_visits=total_visits_2,
                              total_amount=total_amount_2,
                              since_prev=since_prev_2)
head(customer_data_2)
################################################################################
customers_2 <- data.frame(cid = unique(customer_data_2$id))

customers_2$recency <- 6 - map_quantiles(customer_data_2$since_prev)

customers_2$frequency <- map_quantiles(customer_data_2$total_visits)

customers_2$amount <- map_quantiles(customer_data_2$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_2$rfm <- (customers_2$recency*100
                    + customers_2$frequency*10
                    + customers_2$amount)
head(customers_2)



##############################################################################################
# THIRD PERIOD: Aug 2011 - Dec 2011


# The following code sorts the transactions by date.
rfm_data_3_RF <- rfm_data_3_RF[order(rfm_data_3_RF$Date),]
rfm_data_3_M <- rfm_data_3_M[order(rfm_data_3_M$Date),]

head(rfm_data_3_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_3 <- NULL
total_amount_3 <- NULL
since_prev_3 <- NULL

for (id in unique(rfm_data_3_RF$CustomerID)) {
  total_visits_3 <- c(total_visits_3, sum(rfm_data_3_RF$CustomerID == id))
  since_prev_3 <- c(since_prev_3, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_3_RF$Date[rfm_data_3_RF$CustomerID == id])))
  total_amount_3 <- c(total_amount_3, sum(rfm_data_3_M$TotalSpent[rfm_data_3_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_3 <- data.frame(id=unique(rfm_data_3_RF$CustomerID),
                              total_visits=total_visits_3,
                              total_amount=total_amount_3,
                              since_prev=since_prev_3)
head(customer_data_3)
################################################################################
customers_3 <- data.frame(cid = unique(customer_data_3$id))

customers_3$recency <- 6 - map_quantiles(customer_data_3$since_prev)

customers_3$frequency <- map_quantiles(customer_data_3$total_visits)

customers_3$amount <- map_quantiles(customer_data_3$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_3$rfm <- (customers_3$recency*100
                    + customers_3$frequency*10
                    + customers_3$amount)
head(customers_3)


###############################################################################################
#Combining the rfm from each period

#All the RFM values 

combined_rfm <- merge(customers_1, customers_2 ,by = "cid", all.x = TRUE)
combined_rfm <- merge(combined_rfm, customers_3, by = "cid", all.x = TRUE)
combined_rfm[is.na(combined_rfm)] <- 0
combined_rfm <- combined_rfm[,c(1,5,9,13)]
names(combined_rfm) <- c("CustomerID","RFM1", "RFM2","RFM3")


########### Segment RFM 1

segments1=character(nrow(customers_1))
customers_1$recency  <- as.numeric(as.character((customers_1$recency )))
customers_1$frequency <- as.numeric(as.character((customers_1$frequency)))
customers_1$amount <- as.numeric(as.character((customers_1$amount)))

for(t in 1:nrow(customers_1)){
  r <- customers_1[t,]$recency 
  f <- customers_1[t,]$frequency
  m <- customers_1[t,]$amount
  if(r %in% seq(4,5) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments1[t] = "Champions"
  }else if (r %in% seq(2,5) & f %in% seq(3,5) & m %in% seq(3,5)){
    segments1[t] = "Loyal Customers"
  }else if (r %in% seq(3,5) & f %in% seq(1,3) & m %in% seq(1,3)){
    segments1[t] = "Potential Loyalist"
  }else if (r %in% seq(4,5) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments1[t] = "New Customers"
  }else if (r %in% seq(3,4) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments1[t] = "Promising"
  }else if (r %in% seq(2,3) & f %in% seq(2,3) & m %in% seq(2,3)){
    segments1[t] = "Need Attention"
  }else if (r %in% seq(2,3) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments1[t] = "About To Sleep"
  }else if (r %in% seq(0,2) & f %in% seq(2,5) & m %in% seq(2,5)){
    segments1[t] = "At Risk"
  }else if (r %in% seq(0,1) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments1[t] = "Can't Lose Them"
  }else if (r %in% seq(1,2) & f %in% seq(1,2) & m %in% seq(1,2)){
    segments1[t] = "Hibernating"
  }else if (r %in% seq(0,2) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments1[t] = "Lost"
  }
  else{
    segments1[t] = "Others"
  }
}


segments1 <- as.data.frame(segments1)
segments1 <- data.frame(CID = customers_1$cid,segments1)
count1 <- group_by(segments1, Segment = segments1) %>% summarise(count1 = n())

########### Segment RFM 2

segments2=character(nrow(customers_2))
customers_2$recency  <- as.numeric(as.character((customers_2$recency )))
customers_2$frequency <- as.numeric(as.character((customers_2$frequency)))
customers_2$amount <- as.numeric(as.character((customers_2$amount)))

for(t in 1:nrow(customers_2)){
  r <- customers_2[t,]$recency 
  f <- customers_2[t,]$frequency
  m <- customers_2[t,]$amount
  if(r %in% seq(4,5) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments2[t] = "Champions"
  }else if (r %in% seq(2,5) & f %in% seq(3,5) & m %in% seq(3,5)){
    segments2[t] = "Loyal Customers"
  }else if (r %in% seq(3,5) & f %in% seq(1,3) & m %in% seq(1,3)){
    segments2[t] = "Potential Loyalist"
  }else if (r %in% seq(4,5) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments2[t] = "New Customers"
  }else if (r %in% seq(3,4) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments2[t] = "Promising"
  }else if (r %in% seq(2,3) & f %in% seq(2,3) & m %in% seq(2,3)){
    segments2[t] = "Need Attention"
  }else if (r %in% seq(2,3) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments2[t] = "About To Sleep"
  }else if (r %in% seq(0,2) & f %in% seq(2,5) & m %in% seq(2,5)){
    segments2[t] = "At Risk"
  }else if (r %in% seq(0,1) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments2[t] = "Can't Lose Them"
  }else if (r %in% seq(1,2) & f %in% seq(1,2) & m %in% seq(1,2)){
    segments2[t] = "Hibernating"
  }else if (r %in% seq(0,2) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments2[t] = "Lost"
  }
  else{
    segments2[t] = "Others"
  }
}


segments2 <- as.data.frame(segments2)
segments2 <- data.frame(CID = customers_2$cid,segments2)
count2 <- group_by(segments2, Segment = segments2) %>% summarise(count2 = n())


########### Segment RFM 3

segments3=character(nrow(customers_3))
customers_3$recency  <- as.numeric(as.character((customers_3$recency )))
customers_3$frequency <- as.numeric(as.character((customers_3$frequency)))
customers_3$amount <- as.numeric(as.character((customers_3$amount)))

for(t in 1:nrow(customers_3)){
  r <- customers_3[t,]$recency 
  f <- customers_3[t,]$frequency
  m <- customers_3[t,]$amount
  if(r %in% seq(4,5) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments3[t] = "Champions"
  }else if (r %in% seq(2,5) & f %in% seq(3,5) & m %in% seq(3,5)){
    segments3[t] = "Loyal Customers"
  }else if (r %in% seq(3,5) & f %in% seq(1,3) & m %in% seq(1,3)){
    segments3[t] = "Potential Loyalist"
  }else if (r %in% seq(4,5) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments3[t] = "New Customers"
  }else if (r %in% seq(3,4) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments3[t] = "Promising"
  }else if (r %in% seq(2,3) & f %in% seq(2,3) & m %in% seq(2,3)){
    segments3[t] = "Need Attention"
  }else if (r %in% seq(2,3) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments3[t] = "About To Sleep"
  }else if (r %in% seq(0,2) & f %in% seq(2,5) & m %in% seq(2,5)){
    segments3[t] = "At Risk"
  }else if (r %in% seq(0,1) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments3[t] = "Can't Lose Them"
  }else if (r %in% seq(1,2) & f %in% seq(1,2) & m %in% seq(1,2)){
    segments3[t] = "Hibernating"
  }else if (r %in% seq(0,2) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments3[t] = "Lost"
  }
  else{
    segments3[t] = "Others"
  }
}


segments3 <- as.data.frame(segments3)
segments3 <- data.frame(CID = customers_3$cid,segments3)
count3 <- group_by(segments3, Segment = segments3) %>% summarise(count3 = n())


####### Overall RFM 

rfm_data_M <- retail_data_DescandCID 
rfm_data_RF <- retail_data_wo_cancelled 

# The following code sorts the transactions by date.
rfm_data_RF <- rfm_data_RF[order(rfm_data_RF$Date),]
rfm_data__M <- rfm_data_M[order(rfm_data_M$Date),]

head(rfm_data_RF) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits <- NULL
total_amount <- NULL
since_prev <- NULL

for (id in unique(rfm_data_RF$CustomerID)) {
  total_visits <- c(total_visits, sum(rfm_data_RF$CustomerID == id))
  since_prev <- c(since_prev, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_RF$Date[rfm_data_RF$CustomerID == id])))
  total_amount <- c(total_amount, sum(rfm_data_M$TotalSpent[rfm_data_M$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data <- data.frame(id=unique(rfm_data_RF$CustomerID),
                              total_visits=total_visits,
                              total_amount=total_amount,
                              since_prev=since_prev)


head(customer_data)
################################################################################
customers_all<- data.frame(cid = unique(customer_data$id))
# The following commands assign the recency, frequency, and monetary value
# rating on a scale of 1-5 with 5 being the most recent, most frequent, most
# monetary value, and 1 being the least recent, least frequent and least
# monetary value.


customers_all$recency <- 6 - map_quantiles(customer_data$since_prev)

customers_all$frequency <- map_quantiles(customer_data$total_visits)

customers_all$amount <- map_quantiles(customer_data$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_all$rfm <- (customers_all$recency*100
                    + customers_all$frequency*10
                    + customers_all$amount)
head(customers_all)

########### Segment overall RFM

segments_all=character(nrow(customers_all))
customers_all$recency  <- as.numeric(as.character((customers_all$recency )))
customers_all$frequency <- as.numeric(as.character((customers_all$frequency)))
customers_all$amount <- as.numeric(as.character((customers_all$amount)))

for(t in 1:nrow(customers_all)){
  r <- customers_all[t,]$recency 
  f <- customers_all[t,]$frequency
  m <- customers_all[t,]$amount
  if(r %in% seq(4,5) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments_all[t] = "Champions"
  }else if (r %in% seq(2,5) & f %in% seq(3,5) & m %in% seq(3,5)){
    segments_all[t] = "Loyal Customers"
  }else if (r %in% seq(3,5) & f %in% seq(1,3) & m %in% seq(1,3)){
    segments_all[t] = "Potential Loyalist"
  }else if (r %in% seq(4,5) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments_all[t] = "New Customers"
  }else if (r %in% seq(3,4) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments_all[t] = "Promising"
  }else if (r %in% seq(2,3) & f %in% seq(2,3) & m %in% seq(2,3)){
    segments_all[t] = "Need Attention"
  }else if (r %in% seq(2,3) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments_all[t] = "About To Sleep"
  }else if (r %in% seq(0,2) & f %in% seq(2,5) & m %in% seq(2,5)){
    segments_all[t] = "At Risk"
  }else if (r %in% seq(0,1) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments_all[t] = "Can't Lose Them"
  }else if (r %in% seq(1,2) & f %in% seq(1,2) & m %in% seq(1,2)){
    segments_all[t] = "Hibernating"
  }else if (r %in% seq(0,2) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments_all[t] = "Lost"
  }
  else{
    segments_all[t] = "Others"
  }
}


segments_all <- as.data.frame(segments_all)
segments_all <- data.frame(CID = customers_all$cid,segments_all)
count_all <- group_by(segments_all, Segment = segments_all) %>% summarise(Count = n())


####### VISUALISATION

#### Overall RFM :

library(ggplot2)

levels <- as.factor(c("Champions", "Loyal Customers", "Potential Loyalist", "Need Attention", "About To Sleep","At Risk", "Hibernating",  "Others"))


count_all$Segment <- factor(count$Segment, levels = levels)

bp<- ggplot(count_all, aes(x="", y=Count, fill=Segment))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie + theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
  geom_text(aes(label = percent(Count/sum(Count))), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

###### Plotting RFMs by Period 

counts <- merge(count1 ,count2 ,by = "Segment")
counts <- merge(counts, count3, by = "Segment")


forplotting <- counts %>% gather('count1':'count3',key = 'Period', value = 'Count')
forplotting$Segment <- factor(forplotting$Segment, levels = levels)


ggplot(forplotting, aes(x = Segment, y = Count)) +
  geom_col(aes(fill = Period), position = "dodge")+
  labs(title = "Segments of customers based on RFM scores", xlab = "Segments", ylab = "Count")+
  scale_fill_manual(labels = c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"), values = c("royalblue4", "hotpink3", "yellow3"))




#### Average RFM Score across the Periods


combined_rfm$RFM1 <- as.numeric(all_rfm$rfm1)
combined_rfm$RFM2 <- as.numeric(all_rfm$rfm2)
combined_rfm$rfm3 <- as.numeric(all_rfm$rfm3)

trend <- data.frame(Period = c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"), AverageRFMScore = c(mean(all_rfm$rfm1), mean(all_rfm$rfm2), mean(all_rfm$rfm3)))
trend$Period <- factor(trend$Period, levels =  c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"))
ggplot(trend, aes(x = Period,y = AverageRFMScore)) + geom_line(aes(group = 1)) + geom_point() + labs(title= "Average RFM Scores across the periods", y = "Average RFM Scores")


####### Frequency vs Monetary across the Periods


normalize <- function(x) {
  
  return ((x - min(x)) / (max(x) - min(x)))
  
}

g <- data.frame(Frequency = normalize(customer_data$total_visits), Monetary = normalize(customer_data$total_amount))

m_all <- ecdf(customer_data$total_amount)
customer_data$percentilem <- m_all(customer_data$total_amount)

f_all <- ecdf(customer_data$total_visits)
customer_data$percentilef <- m_all(customer_data$total_visits)

ggplot(customer_data, aes(x = percentilef, y = percentilem)) + geom_point()

ggplot(g, aes(x = Frequency, y = percentilem)) + geom_point()

###### RFM heatmap

customer_data$rf <- (customers_all$recency*10
                      + customers_all$frequency)

customer_data$rf <- as.factor(as.character(customer_data$rf))
rfmeanm <- customer_data %>% group_by(rf) %>% summarise(Mean = mean(total_amount))
rfmeanm$f <- as.factor(as.character(floor(as.numeric(as.character(rfmeanm$rf))/10)))
rfmeanm$r <- as.factor(as.character(as.numeric(as.character(rfmeanm$rf)) - 10* as.numeric(as.character(rfmeanm$f))))


ggplot(data = rfmeanm, mapping = aes(x = rfmeanm$f, y = rfmeanm$r, fill = rfmeanm$Mean))+
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white",
                       high = ("blue"), name = "Mean Monetary 
  Value per RF")+
  labs(title = "RFM Heat Map", y = "Recency", x= "Frequency" )



##############







