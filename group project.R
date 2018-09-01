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
                     1839:1840,1901,1964,2068,2085:2087,2145:2146,2151,2155,2170,2187,
                     2188,2256:2258,2261,2263,2288:2290,2312,2343,2378,2380,2465,2817:2818,
                     2830:2831,2877:2879,3031:3033,3113:3114,3132,3134:3136,3434:3435,
                     3570,3582:3590,3635:3636,3703:3704,3728:3729,3734:3738,3739,3744,
                     3835,3976:3978,3986,3989:3997,4167:4181),]
new_retail_data <- retail_data[retail_data$Description %in% number2[,1],]



#####################################
#                                   #
#            RFM Model              #
#                                   #
#####################################

#Now, for each customer, we find the days between each subsequent purchase, the total number of visits made, and amount spent.
total_visits <- NULL
total_amount <- NULL
since_prev <- NULL
for (id in unique(retail_data$CustomerID)) {
  total_visits <- c(total_visits, sum(retail_data$CustomerID == id))
  total_amount <- c(total_amount, sum(retail_data$totalspent[retail_data$CustomerID == id]))
  since_prev <- c(since_prev, min(as.numeric(as.Date("2011-11-01") - retail_data$Date[retail_data$CustomerID == id])))
  }

head(total_visits)
length(total_visits) #4372 unique visits(i.e unique customer ID)
head(total_amount)

customer_retail_data <- data.frame(id=unique(retail_data$CustomerID),
                                   total_visits=total_visits,
                                   total_amount=total_amount,
                                   since_prev=since_prev)
head(customer_retail_data)

customers <- data.frame(cid = unique(retail_data$CustomerID))

#The following commands assign the recency, frequency, and monetary value rating on a scale of 1-5 with 5 being the most recent,
#most frequent, most monetary value, and 1 being the least recent, least frequent and least monetary value.

#New R, F, M-score assigning function.
map_quantiles <- function(vect, num_groups=5) { 
  ranks <- order(vect)
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

customers$recency <- 6 - map_quantiles(customer_retail_data$since_prev)

customers$frequency <- map_quantiles(customer_retail_data$total_visits)

customers$amount <- map_quantiles(customer_retail_data$total_amount)

#The RFM score is then a concatenation of the above three scores. Here is its calculation:
customers$rfm <- (customers$recency*100 + customers$frequency*10 + customers$amount)
head(customers)


