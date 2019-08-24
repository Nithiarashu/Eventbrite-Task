#Gathered data from the source

events<-read.csv("C:\\Users\\Nithiarashu\\Desktop\\EventBrite\\Actual_sheets\\events (1).csv")
ord<-read.csv("C:\\Users\\Nithiarashu\\Desktop\\EventBrite\\Actual_sheets\\orders (1).csv")
org<-read.csv("C:\\Users\\Nithiarashu\\Desktop\\EventBrite\\Actual_sheets\\organizers (1).csv")

#remove duplicated from the gathered data
library(dplyr)
events_dup<-distinct(events)
ord_dup<-distinct(ord)
org_dup<-distinct(org)

# 2. How many organizers are in the top 10% ranked by their aggregate gross ticket revenue?

org_tkt_revnue<-aggregate(ord_dup$gross_ticket_revenue_usd,by=list(ord_dup$organizer_id),FUN=sum)
names(org_tkt_revnue)<-c('organizer','gross_ticket_revenue')
org_tkt_revnue<-org_tkt_revnue[order(-org_tkt_revnue$gross_ticket_revenue),]

#top 10 % of the organizer ranked by their aggregate gross ticket
Total_count<-length(org_tkt_revnue$organizer)
perc_10_cnt<-round((10/100)*Total_count)
org_tkt_revnue_top10_perc<-org_tkt_revnue[1:perc_10_cnt,]

# 3. What is the mean, median, 25%, 75% and the top 1% of ticket prices (calculated asgross ticket sales/tickets sold) for paid tickets?

#Paid tickets and ticket price
paid_orders<-ord_dup[ord_dup$gross_ticket_sales_usd>0,]
paid_orders$tkt_price<-paid_orders$gross_ticket_sales_usd/paid_orders$tickets_sold

#Mean and Median for paid tickets 
print(mean_tkt_price<-mean(paid_orders$tkt_price))
print(med_tkt_price<-median(paid_orders$tkt_price))

#top 1% for paid
paid_orders_top1<-round(length(paid_orders$organizer)*0.01)
paid_orders_ord<-paid_orders[order(-paid_orders$tkt_price),]
paid_orders_top1_perc<-paid_orders_ord[1:paid_orders_top1,]

#Bottom 25%
paid_orders_25<-round(length(paid_orders$organizer)*0.25)
paid_orders_ord<-paid_orders[order(paid_orders$tkt_price),]
paid_orders_25_perc<-paid_orders_ord[1:paid_orders_25,]

#Bottom 75%
paid_orders_75<-round(length(paid_orders$organizer)*0.75)
paid_orders_ord<-paid_orders[order(paid_orders$tkt_price),]
paid_orders_75_perc<-paid_orders_ord[1:paid_orders_75,]

#4. Plot the relationship between the ticket price and the tickets sold for each event category.

#Ticket price
ord_dup$tkt_price<-ord_dup$gross_ticket_sales_usd/ord_dup$tickets_sold

#Getting event type
ord_event<-merge(ord_dup,events_dup,by=c('event_id','organizer_id'))

#Aggregate tkt price by event type and tkt sold
avg_tkt_price<-aggregate(ord_event$tkt_price,by=list(ord_event$tickets_sold,ord_event$event_category_desc),FUN=mean)
names(avg_tkt_price)<-c('tickets_sold','event_type','avg_tkt_price')

avg_tkt_price$event_type
avg_tkt_price_food<-avg_tkt_price[avg_tkt_price$event_type=="Food & Drink",]
avg_tkt_price_music<-avg_tkt_price[avg_tkt_price$event_type=="Music",]
avg_tkt_price_bp<-avg_tkt_price[avg_tkt_price$event_type=="Business & Professional",]

library(plotly)
plot_ly(avg_tkt_price_food, x = ~tickets_sold, y = ~avg_tkt_price, type = 'bar')%>% layout(xaxis = list(autorange = TRUE),yaxis = list(autorange = TRUE))
plot_ly(avg_tkt_price_music, x = ~tickets_sold, y = ~avg_tkt_price, type = 'bar')%>% layout(xaxis = list(autorange = TRUE),yaxis = list(autorange = TRUE))
plot_ly(avg_tkt_price_bp, x = ~tickets_sold, y = ~avg_tkt_price, type = 'bar')%>% layout(xaxis = list(autorange = TRUE),yaxis = list(autorange = TRUE))

# Part 2: 1. What are our monthly quantities of tickets sold for events in Germany for the past 14months?

#Getting the date and the currency
ord_event_date<-merge(ord_event,org_dup,by=c('organizer_id'))
ord_event_date$first_paid_order_date<-as.Date(ord_event_date$first_paid_order_date)
ord_event_date$email_org<-paste(ord_event_date$organizer_id,"@gmail.com")
                              
#install.packages("mondate")
library(mondate)
max_paid_date<-max(ord_event_date$first_paid_order_date,na.rm=TRUE)
months_14_bf<-as.Date(mondate(max_paid_date)-14)
data_14_months<-subset(ord_event_date,first_paid_order_date>=months_14_bf & first_paid_order_date<=max_paid_date )

data_14_months$month<-month(data_14_months$first_paid_order_date)
data_14_months$year<-year(data_14_months$first_paid_order_date)

tkts_sold_14_months<-aggregate(data_14_months$tickets_sold,by=list(data_14_months$month,data_14_months$year,data_14_months$first_currency),FUN=sum)

# 2. Who are our top 10 event organizers by paid tickets sold in the past 365 days? We needthe event organizer's email address and the number of paid tickets per organizer.

months_12_bf<-as.Date(mondate(max_paid_date)-12)
data_12_months<-subset(ord_event_date,first_paid_order_date>=months_12_bf & first_paid_order_date<=max_paid_date )
paid_tkts_sold_12_months<-data_12_months[data_12_months$gross_ticket_sales_usd>0,]
organizer_paid_tkts_sold<-aggregate(data_12_months$tickets_sold,by=list(data_12_months$organizer_id,data_12_months$email_org),FUN=sum)
organizer_paid_tkts_sold<-organizer_paid_tkts_sold[order(-organizer_paid_tkts_sold$x),]
organizer_paid_tkts_sold_t10<-organizer_paid_tkts_sold[1:10,]
names(organizer_paid_tkts_sold_t10)<-c('organizer_id','email','tickets_sold')

#3. How many event organizers who signed up in 2013 have sold at least one paid ticket,but never a free ticket?

#install.packages("reshape2")
library(reshape2)
ord_event_date$signup_date<-as.Date(ord_event_date$signup_date)
ord_event_date$signup_date_yr<-year(ord_event_date$signup_date)
org_signed_2013<-ord_event_date[ord_event_date$signup_date_yr==2013,]
org_tkt_type_2013<-aggregate(org_signed_2013$tickets_sold,by=list(org_signed_2013$organizer_id,org_signed_2013$event_paid_type),FUN=sum)

names(org_tkt_type_2013)<-c('org','event_type','tkts_sold')
is.na(org_tkt_type_2013$org)

org_tkt_type_2013_trans<-reshape(org_tkt_type_2013, idvar = "org", timevar = "event_type", direction = "wide")
org_tkt_type_2013_trans[is.na(org_tkt_type_2013_trans)]<-0
org_tkt_type_2013_paid_only<-org_tkt_type_2013_trans[org_tkt_type_2013_trans$`tkts_sold.mixed event`==0 & org_tkt_type_2013_trans$`tkts_sold.free event`==0 &org_tkt_type_2013_trans$`tkts_sold.paid event`>0,] 


write.csv(ord_event_date,"C:\\Users\\Nithiarashu\\Desktop\\EventBrite\\Actual_sheets\\final_events_data_combined.csv")
