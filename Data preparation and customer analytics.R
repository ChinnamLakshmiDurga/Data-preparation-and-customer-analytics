library(data.table)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(ggmosaic)
setwd("~/")
QVI_purchase_behaviour <- read_csv("QVI_purchase_behaviour.csv")
View(QVI_purchase_behaviour)
QVI_transaction_data <- read_excel("QVI_transaction_data.xlsx")
View(QVI_transaction_data) 
str(QVI_purchase_behaviour)
str(QVI_transaction_data)
QVI_transaction_data$DATE <- as.Date(QVI_transaction_data$DATE, origin = "1899-12-30")
summary(QVI_transaction_data$PROD_NAME)
str(QVI_transaction_data)
productWords <- data.table(unlist(strsplit(unique(QVI_transaction_data$PROD_NAME), "
")))
setnames(productWords, 'Products')
View(productWords)
productWords$Products <- str_replace_all(productWords$Products,"[0-9]"," ")
productWords$Products <- str_replace_all(productWords$Products,"[gG]"," ")
productWords$Products <- str_replace_all(productWords$Products,"[[:punct:]]"," ")
words <- strsplit(productWords$Products," ")
Products.freq<-table(unlist(words))
Products.freq <-  as.data.frame(Products.freq)
Products.freq <- Products.freq[order(Products.freq$Freq, decreasing = T),]
readdata <- function(fn){
  QVI_transaction_data <- fread(fn)   ## no need to put a sep here, fread guess it
  QVI_transaction_data[, SALSA := grepl("salsa", tolower(QVI_transaction_data$PROD_NAME))]
  return(QVI_transaction_data)
  QVI_transaction_data <- QVI_transaction_data[SALSA == FALSE, ][, SALSA := NULL]
}
summary(QVI_transaction_data)
prod_qty_200 <- QVI_transaction_data %>% filter(PROD_QTY==200)
same_customer <- QVI_transaction_data %>% filter(LYLTY_CARD_NBR == 226000) 
QVI_transaction_data <- QVI_transaction_data[!(QVI_transaction_data$LYLTY_CARD_NBR == 226000),]
summary(QVI_transaction_data)
count_by_date <- count(QVI_transaction_data, QVI_transaction_data$DATE)
count_by_date
transaction_by_date <- QVI_transaction_data[order(QVI_transaction_data$DATE),]
theme_set(theme_dark())
theme_update(plot.title = element_text(hjust = 0.5))
trans_Over_Time <-ggplot(count_by_date, aes(x = count_by_date$`QVI_transaction_data$DATE`, y = count_by_date$n)) +
  geom_line() +
  labs(x = "Day", y = "Number_of_transactions", title = "Transactions_over_time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
trans_Over_Time

filter_data <- count_by_date[ (count_by_date$`QVI_transaction_data$DATE` >= "2018-12-01" & count_by_date$`QVI_transaction_data$DATE`<="2018-12-31"),]
ggplot(filter_data, aes(x = filter_data$`QVI_transaction_data$DATE`, y = filter_data$n)) +
  geom_line() +
  labs(x = "Day", y = "Number_of_transactions", title = "Transactions_in_December") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
is.data.table(QVI_transaction_data)
data.table(QVI_transaction_data)
setDT(QVI_transaction_data)
QVI_transaction_data[, "PACK_SIZE" := parse_number(PROD_NAME)]
PackSize_Vs_Transactions <- QVI_transaction_data[, .N, PACK_SIZE][order(PACK_SIZE)]
PackSize_Vs_Transactions
hist(QVI_transaction_data[, PACK_SIZE])
QVI_transaction_data$BRAND <- gsub("([A-Za-z]+).*", "\\1", QVI_transaction_data$PROD_NAME)
QVI_transaction_data[, .N, by = BRAND][order(-N)]
QVI_transaction_data[BRAND == "RED", BRAND := "RRD"]
QVI_transaction_data[BRAND == "SNBTS", BRAND := "SUNBITES"]
QVI_transaction_data[BRAND == "INFZNS", BRAND := "INFUZIONS"]
QVI_transaction_data[BRAND == "WW", BRAND := "WOOLWORTHS"]
QVI_transaction_data[BRAND == "SMITH", BRAND := "SMITHS"]
QVI_transaction_data[BRAND == "NCC", BRAND := "NATURAL"]
QVI_transaction_data[BRAND == "DORITO", BRAND := "DORITOS"]
QVI_transaction_data[BRAND == "GRAIN", BRAND := "GRNWVES"]
QVI_transaction_data[, .N, by = BRAND][order(BRAND)]
summary(QVI_purchase_behaviour)
QVI_purchase_behaviour[, .N, by = LIFESTAGE][order(.N)]
data1 <- merge(QVI_transaction_data, QVI_purchase_behaviour, all.x = TRUE)
apply(data1, 2, function(x) any(is.na(x)))
write.csv(data1,"QVI_data1.csv")
total_sales <- data1 %>% group_by(LIFESTAGE,PREMIUM_CUSTOMER)
pf.total_sales <- summarise(total_sales,sales_count=sum(TOT_SALES))
summary(pf.total_sales)

p <- ggplot(pf.total_sales) + geom_mosaic(aes(weight = sales_count, x = product(PREMIUM_CUSTOMER, LIFESTAGE),fill = PREMIUM_CUSTOMER)) + labs(x = "Lifestage", y = "Premium_Customer_Flag", title = "Proportion_of_Sales") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
p +geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))), inherit.aes = F)
total_sales <- data1 %>% group_by(LIFESTAGE,PREMIUM_CUSTOMER)
no_of_customers <- summarise(total_sales,customer_count = length(unique(LYLTY_CARD_NBR))) 
summary(no_of_customers)

pl <- ggplot(data = no_of_customers) + geom_mosaic(aes(weight = customer_count, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) + labs(x = "Lifestage", y = "Premium_Customer_Flag", title = "Proportion_of_Customers") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))
pl

total_sales_1 <-data1 %>% group_by(LIFESTAGE,PREMIUM_CUSTOMER)

units <-  summarise(total_sales_1, units_count = (sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)))

summary(units)


ggplot(data = units, aes(weight = units_count, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg Units_per_Transaction", title = "Units_per_Customer") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
total_sales_2 <-data1 %>% group_by(LIFESTAGE,PREMIUM_CUSTOMER)
PricePerUnit <-  summarise(total_sales_2, price_per_unit = (sum(TOT_SALES)/sum(PROD_QTY)))
ggplot(data=PricePerUnit, aes(weight = price_per_unit,x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + geom_bar(position = position_dodge()) + labs(x = "Lifestage", y = "Avg price_per_Unit", title = "Price_per_Unit") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

PricePerUnit <- data1[, price := TOT_SALES/PROD_QTY]
t.test(data1[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price],data1[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price], alternative = "greater")


segment1 <- data1[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data1[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER =="Mainstream"),]

quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]
ggplot(brand_proportions, aes(brand_proportions$BRAND,brand_proportions$affinityToBrand)) + geom_bar(stat = "identity",fill = "blue") + labs(x = "Brand", y = "Customers_Affinity_to_Brand", title = "Favourite_Brands_of_Customers") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]

quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]

pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]
data1[PACK_SIZE == 270, unique(PROD_NAME)]
 
