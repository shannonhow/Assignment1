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

##################### RFM1:


library(dplyr)
library(gtools)


tm1 <- rfm_data_1_M%>%select(InvoiceNo, CustomerID, Date, TotalSpent)
tm1 <- tm1[order(tm1$CustomerID),]

#getting the rfm
#getting the m first by finding the total amount spent by that customer
tempm1 <- group_by(tm1, CustomerID) %>% summarise(Customertotal = sum(TotalSpent))

library(plyr)
tempm1$m1 <- quantcut(tempm1$Customertotal, 5)
levelm1 <- levels(tempm1$m1)
tempm1$m1 <- mapvalues(tempm1$m1,  from = levelm1, to = c(1,2,3,4,5))


#getting the f
trf1 <- rfm_data_1_RF%>%select(InvoiceNo, CustomerID, Date, TotalSpent) %>% distinct(InvoiceNo, .keep_all = TRUE)
trf1 <- trf1[order(trf1$CustomerID),]



trf1$CustomerID <- as.factor(trf1$CustomerID)
detach("package:plyr", unload=TRUE) 
library(dplyr)
temprf1 <- group_by(trf1, CustomerID) %>% summarise(count = n())

library(plyr)
temprf1$f1 <- quantcut(temprf1$count, 5)
levelf1 <- levels(temprf1$f1)
temprf1$f1 <- mapvalues(temprf1$f1,  from = levelf1, to = c(1,2,3,4))

detach("package:plyr", unload=TRUE) 
library(dplyr)

#getting the r


nowvalue <- as.numeric(as.Date("2018-09-01"))

Datevalue = nowvalue - as.numeric(as.Date(as.character(trf1$Date), "%Y-%m-%d")) 

library(tidyverse)
tempr1 <- add_column(trf1, Datevalue)

tempr1 <- group_by(tempr1, CustomerID) %>% summarise(r1 = min(Datevalue))
 
library(plyr)
tempr1$r1 <- quantcut(tempr1$r1, 5)
levelr1 <- levels(tempr1$r1)
tempr1$r1 <- mapvalues(tempr1$r1,  from = levelr1, to = c(5,4,3,2,1))
detach("package:plyr", unload=TRUE) 


#Getting the RFM values
rfm1 <- merge(tempr1, temprf1, by = "CustomerID")
rfm1 <- merge(rfm1, tempm1, by = "CustomerID")
rfm1 <- add_column(rfm1, rfm1 = paste(rfm1$r1,rfm1$f1,rfm1$m1, sep = ""))


##################### RFM 2:


library(dplyr)
library(gtools)


tm2 <- rfm_data_2_M%>%select(InvoiceNo, CustomerID, Date, TotalSpent)
tm2 <- tm2[order(tm2$CustomerID),]

#getting the rfm
#getting the m first by finding the total amount spent by that customer
tempm2 <- group_by(tm2, CustomerID) %>% summarise(Customertotal = sum(TotalSpent))

library(plyr)
tempm2$m2 <- quantcut(tempm2$Customertotal, 5)
levelm2 <- levels(tempm2$m2)
tempm2$m2 <- mapvalues(tempm2$m2,  from = levelm2, to = c(1,2,3,4,5))


#getting the f
trf2 <- rfm_data_2_RF%>%select(InvoiceNo, CustomerID, Date, TotalSpent) %>% distinct(InvoiceNo, .keep_all = TRUE)
trf2 <- trf2[order(trf2$CustomerID),]

trf2$CustomerID <- as.factor(trf2$CustomerID)
detach("package:plyr", unload=TRUE) 
library(dplyr)
temprf2 <- group_by(trf2, CustomerID) %>% summarise(count = n())

library(plyr)
temprf2$f2 <- quantcut(temprf2$count, 5)
levelf2 <- levels(temprf2$f2)
temprf2$f2 <- mapvalues(temprf2$f2,  from = levelf2, to = c(1,2,3,4))

detach("package:plyr", unload=TRUE) 
library(dplyr)

#getting the r


nowvalue <- as.numeric(as.Date("2018-09-01"))

Datevalue = nowvalue - as.numeric(as.Date(as.character(trf2$Date), "%Y-%m-%d")) 

library(tidyverse)
tempr2 <- add_column(trf2, Datevalue)

tempr2 <- group_by(tempr2, CustomerID) %>% summarise(r2 = min(Datevalue))

library(plyr)
tempr2$r2 <- quantcut(tempr2$r2, 5)
levelr2 <- levels(tempr2$r2)
tempr2$r2 <- mapvalues(tempr2$r2,  from = levelr2, to = c(5,4,3,2,1))
detach("package:plyr", unload=TRUE) 


#Getting the RFM values
rfm2 <- merge(tempr2, temprf2, by = "CustomerID")
rfm2 <- merge(rfm2, tempm2, by = "CustomerID")
rfm2 <- add_column(rfm2, rfm2 = paste(rfm2$r2, rfm2$f2,rfm2$m2, sep = ""))


##################### RFM3:


library(dplyr)
library(gtools)


tm3 <- rfm_data_3_M%>%select(InvoiceNo, CustomerID, Date, TotalSpent)
tm3 <- tm3[order(tm3$CustomerID),]

#getting the rfm
#getting the m first by finding the total amount spent by that customer
tempm3 <- group_by(tm3, CustomerID) %>% summarise(Customertotal = sum(TotalSpent))

library(plyr)
tempm3$m3 <- quantcut(tempm3$Customertotal, 5)
levelm3 <- levels(tempm3$m3)
tempm3$m3 <- mapvalues(tempm3$m3,  from = levelm3, to = c(1,2,3,4,5))



#getting the f
trf3 <- rfm_data_3_RF%>%select(InvoiceNo, CustomerID, Date, TotalSpent) %>% distinct(InvoiceNo, .keep_all = TRUE)
trf3 <- trf3[order(trf3$CustomerID),]

trf3$CustomerID <- as.factor(trf3$CustomerID)
detach("package:plyr", unload=TRUE) 
library(dplyr)
temprf3 <- group_by(trf3, CustomerID) %>% summarise(count = n())

library(plyr)
temprf3$f3 <- quantcut(temprf3$count, 5)
levelf3 <- levels(temprf3$f3)
temprf3$f3 <- mapvalues(temprf3$f3,  from = levelf3, to = c(1,2,3,4))

detach("package:plyr", unload=TRUE) 
library(dplyr)

#getting the r


nowvalue <- as.numeric(as.Date("2018-09-01"))

Datevalue = nowvalue - as.numeric(as.Date(as.character(trf3$Date), "%Y-%m-%d")) 

library(tidyverse)
tempr3 <- add_column(trf3, Datevalue)

tempr3 <- group_by(tempr3, CustomerID) %>% summarise(r3 = min(Datevalue))

library(plyr)
tempr3$r3 <- quantcut(tempr3$r3, 5)
levelr3 <- levels(tempr3$r3)
tempr3$r3 <- mapvalues(tempr3$r3,  from = levelr3, to = c(5,4,3,2,1))
detach("package:plyr", unload=TRUE) 



#Getting the RFM values
rfm3 <- merge(tempr3, temprf3, by = "CustomerID")
rfm3 <- merge(rfm3, tempm3, by = "CustomerID")
rfm3 <- add_column(rfm3, rfm3 = paste(rfm3$r3, rfm3$f3,rfm3$m3, sep = ""))

#All the RFM values 

all_rfm <- merge(rfm1, rfm2, by = "CustomerID", all.x = TRUE)
all_rfm <- merge(all_rfm, rfm3, by = "CustomerID", all.x = TRUE)
all_rfm[is.na(all_rfm)] <- 0
all_rfm <- all_rfm[,c(1,7,13,19)]

########### Segment RFM 1

segments1=character(nrow(rfm1))
rfm1$r1 <- as.numeric(as.character((rfm1$r1)))
rfm1$f1 <- as.numeric(as.character((rfm1$f1)))
rfm1$m1 <- as.numeric(as.character((rfm1$m1)))

for(t in 1:nrow(rfm1)){
  r <- rfm1[t,]$r1
  f <- rfm1[t,]$f1
  m <- rfm1[t,]$m1
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



segments1 <- as.factor(segments1)
segments1 <- data.frame(CID = rfm1$CustomerID,segments1)
count1 <- group_by(segments1, Segment = segments1) %>% summarise(count1 = n())




########### Segment RFM 2

segments2 <- character(nrow(rfm2))
rfm2$r2 <- as.numeric(as.character((rfm2$r2)))
rfm2$f2 <- as.numeric(as.character((rfm2$f2)))
rfm2$m2 <- as.numeric(as.character((rfm2$m2)))

for(t in 1:nrow(rfm2)){
  r <- rfm2[t,]$r2
  f <- rfm2[t,]$f2
  m <- rfm2[t,]$m2
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


segments2 <- as.factor(segments2)
segments2 <- data.frame(CID = rfm2$CustomerID,segments2)
count2 <- group_by(segments2, Segment = segments2) %>% summarise(count2 = n())

########### Segment RFM 3

segments3 <- character(nrow(rfm3))
rfm3$r3 <- as.numeric(as.character((rfm3$r3)))
rfm3$f3 <- as.numeric(as.character((rfm3$f3)))
rfm3$m3 <- as.numeric(as.character((rfm3$m3)))

for(t in 1:nrow(rfm3)){
  r <- rfm3[t,]$r3
  f <- rfm3[t,]$f3
  m <- rfm3[t,]$m3
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



segments3 <- as.factor(segments3)
segments3 <- data.frame(CID = rfm3$CustomerID,segments3)
count3 <- group_by(segments3, Segment = segments3) %>% summarise(count3 = n())



############ ALL Segments


all_segments <- merge(segments1, segments2, by = "CID")
all_segments <- merge(all_segments, segments3, by = "CID")

##############

library(ggplot2)

levels <- as.factor(c("Champions", "Loyal Customers", "Potential Loyalist", "Need Attention", "About To Sleep","At Risk", "Hibernating",  "Others"))

###### plotting RFMs by period 

counts <- merge(count1 ,count2 ,by = "Segment")
counts <- merge(counts, count3, by = "Segment")


forplotting <- counts %>% gather('count1':'count3',key = 'Period', value = 'Count')
forplotting$Segment <- factor(forplotting$Segment, levels = levels)


ggplot(forplotting, aes(x = Segment, y = Count)) +
geom_col(aes(fill = Period), position = "dodge")+
labs(title = "Segments of customers based on RFM scores", xlab = "Segments", ylab = "Count")+
  scale_fill_manual(labels = c("1st Dec 2010 - 31st March 2011", "1st April 2011 - 31st July 2011", "1st August 2011 - 31st December 2011"), values = c("royalblue4", "hotpink3", "yellow3"))

### Overall RFM Data:

rfm_data_M <- retail_data_DescandCID 
rfm_data_RF <- retail_data_wo_cancelled 



##################### Overall RFM:


library(dplyr)
library(gtools)


tm <- rfm_data_M%>%select(InvoiceNo, CustomerID, Date, TotalSpent)
tm <- tm[order(tm$CustomerID),]

#getting the rfm
#getting the m first by finding the total amount spent by that customer
tempm <- group_by(tm, CustomerID) %>% summarise(Customertotal = sum(TotalSpent))

library(plyr)
tempm$m <- quantcut(tempm$Customertotal, 5)
levelm <- levels(tempm$m)
tempm$m <- mapvalues(tempm$m,  from = levelm, to = c(1,2,3,4,5))


#getting the f
trf <- rfm_data_RF%>%select(InvoiceNo, CustomerID, Date, TotalSpent)%>% distinct(InvoiceNo, .keep_all = TRUE)
trf <- trf[order(trf$CustomerID),]

trf$CustomerID <- as.factor(trf$CustomerID)
detach("package:plyr", unload=TRUE) 
library(dplyr)
temprf <- group_by(trf, CustomerID) %>% group_by(CustomerID) %>% summarise(count = n())

library(plyr)
temprf$f <- quantcut(temprf$count, 5)
levelf <- levels(temprf$f)
temprf$f <- mapvalues(temprf$f,  from = levelf, to = c(1,2,3,4,5))

detach("package:plyr", unload=TRUE) 
library(dplyr)

#getting the r


nowvalue <- as.numeric(as.Date("2018-09-01"))

Datevalue = nowvalue - as.numeric(as.Date(as.character(trf$Date), "%Y-%m-%d")) 


library(tidyverse)
tempr<- add_column(trf, Datevalue)

tempr <- group_by(tempr, CustomerID) %>% summarise(r = min(Datevalue))

library(plyr)
tempr$r <- quantcut(tempr$r, 5)
levelr<- levels(tempr$r)
tempr$r <- mapvalues(tempr$r,  from = levelr, to = c(5,4,3,2,1))
detach("package:plyr", unload=TRUE) 

#Getting the RFM values
rfm <- merge(tempr, temprf, by = "CustomerID")
rfm <- merge(rfm, tempm, by = "CustomerID")
rfm <- add_column(rfm, rfm = paste(rfm$r, rfm$f,rfm$m, sep = ""))


###### Segmenting the overall RFM values:

segments <- character(nrow(rfm))
rfm$r <- as.numeric(as.character((rfm$r)))
rfm$f <- as.numeric(as.character((rfm$f)))
rfm$m <- as.numeric(as.character((rfm$m)))

for(t in 1:nrow(rfm)){
  r <- rfm[t,]$r
  f <- rfm[t,]$f
  m <- rfm[t,]$m
  if(r %in% seq(4,5) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments[t] = "Champions"
  }else if (r %in% seq(2,5) & f %in% seq(3,5) & m %in% seq(3,5)){
    segments[t] = "Loyal Customers"
  }else if (r %in% seq(3,5) & f %in% seq(1,3) & m %in% seq(1,3)){
    segments[t] = "Potential Loyalist"
  }else if (r %in% seq(4,5) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments[t] = "New Customers"
  }else if (r %in% seq(3,4) & f %in% seq(0,1) & m %in% seq(0,1)){
    segments[t] = "Promising"
  }else if (r %in% seq(2,3) & f %in% seq(2,3) & m %in% seq(2,3)){
    segments[t] = "Need Attention"
  }else if (r %in% seq(2,3) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments[t] = "About To Sleep"
  }else if (r %in% seq(0,2) & f %in% seq(2,5) & m %in% seq(2,5)){
    segments[t] = "At Risk"
  }else if (r %in% seq(0,1) & f %in% seq(4,5) & m %in% seq(4,5)){
    segments[t] = "Can't Lose Them"
  }else if (r %in% seq(1,2) & f %in% seq(1,2) & m %in% seq(1,2)){
    segments[t] = "Hibernating"
  }else if (r %in% seq(0,2) & f %in% seq(0,2) & m %in% seq(0,2)){
    segments[t] = "Lost"
  }
  else{
    segments[t] = "Others"
  }
}



segments <- as.factor(segments)
segments <- data.frame(CID = rfm$CustomerID,segments)
count <- group_by(segments, Segment = segments) %>% summarise(Count = n())
count$Segment <- factor(count$Segment, levels = levels)



#### Overall RFM :

bp<- ggplot(count, aes(x="", y=Count, fill=Segment))+
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





