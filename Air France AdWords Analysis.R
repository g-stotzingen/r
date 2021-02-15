#Importing necessary 
library(readxl)
library(dplyr)
library(ggplot2)
#Reading the necessary file - search engine data
airfrance <- as.data.frame(read_excel("Air France.xls", sheet = "DoubleClick"))
#Dropping the unnecessary columns
airfrance <- select(airfrance, -c('Keyword ID', 'Keyword Type'))
#Reading the Kayak data
kayak <- read_excel("Air France.xls", sheet = "Kayak")
#Simplifying the column names, deleting spaces
colnames(airfrance) <- c("publisher_id", "publisher", "keyword", "match", "campaign", "keyword_group", "category", "strategy", "status", "bid", "clicks", "click_charge", "cost_per_click", "impressions", "click_thru", "avg_pos", "conversion", "cost_conversion", "amount", "cost", "bookings")
# Drop match with N/A values, unable to interpret
airfrance <- airfrance[!airfrance$match=="N/A",]
# Create column success, Sales keyword -> 1 , no sales keyword -> 0
airfrance$success <- c()
for (row in 1:nrow(airfrance)) {
  if (airfrance$bookings[row] == 0) {airfrance$success[row] <- 0}
  else {airfrance$success[row] <- 1}
}
#Create column match_num for Advanced -> 1, others -> 0
airfrance$match_num <- c()
for (row in 1:nrow(airfrance)) {
  if (airfrance$match[row] == "Advanced") {airfrance$match_num[row] <- 1}
  else {airfrance$match_num[row] <- 0}
}
#Create column keyword_length for length of keywords 
airfrance$keyword_length<-nchar(airfrance$keyword)

#Summarize the dataframe by publisher name with impression, clicks, bookings, cost, amount(revenue), number of keywords
publisher <- airfrance %>%
  group_by(publisher) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount), keyword_count = length(keyword))

# Add kayak dataframe to the publisher data frame
kayak_vector = c("publisher" = kayak[3,1], "impression" = as.numeric(NA),  "clicks" = as.numeric(kayak[3,2]), "bookings" = as.numeric(kayak[3,4]), "cost" = round(as.numeric(kayak[3,3],2)), "amount" = round(as.numeric(kayak[3, 6]), 2))
kayak_df = data.frame(kayak_vector)
publisher <- add_row(publisher, "publisher" = kayak_df[1,1], "impressions" = kayak_df[1,2], "clicks" = kayak_df[1,3], "bookings" = kayak_df[1,4], "cost" = kayak_df[1,5], "amount" = kayak_df[1,6])

# Publisher2 <- adding the Total row to the publisher data frame
publisher2 <- add_row(publisher, "publisher" = "Total")
for (col in 2:ncol(publisher2)) {
  publisher2[nrow(publisher2), col] <- sum(publisher2[, col], na.rm = TRUE)
}
# Publisher dataframe adding necessary columns
# margin column = amount - cost to get net revenue
publisher$margin <- c()
for (row in 1:nrow(publisher)) {
  publisher$margin[row]<- round(publisher$amount[row] - publisher$cost[row], 2)
}
# return_rate column = amount / cost to get $ return on every dollar spent for each publisher
publisher$return_rate <- c()
for (row in 1:nrow(publisher)) {
  publisher$return_rate[row]<- round((publisher$amount[row] / publisher$cost[row]), 2)
}
# cost_impressions column = cost/ impressions cost per impression
publisher$cost_impressions <- c()
for (row in 1:nrow(publisher)) {
  publisher$cost_impressions[row]<- round(publisher$cost[row] / publisher$impressions[row], 2)
}
# cost_clicks column = cost / clicks cost per clicks 
publisher$cost_clicks <- c()
for (row in 1:nrow(publisher)) {
  publisher$cost_clicks[row]<- round(publisher$cost[row] / publisher$clicks[row], 2)
}
# cost_bookings column = cost / bookings cost per bookings
publisher$cost_bookings <- c()
for (row in 1:nrow(publisher)) {
  publisher$cost_bookings[row]<- round(publisher$cost[row] / publisher$bookings[row], 2)
}
# clicks_bookings column = clicks / bookings clicks per bookings
publisher$clicks_bookings <- c()
for (row in 1:nrow(publisher)) {
  publisher$clicks_bookings[row]<- round(publisher$clicks[row] / publisher$bookings[row], 2)
}
# conversion_rate column = bookings / clicks clicks to booking conversion rate
publisher$conversion_rate <- c()
for (row in 1:nrow(publisher)) {
  publisher$conversion_rate[row]<- round(publisher$bookings[row] / publisher$clicks[row], 3) *100
}

# Summarise the dataframe by matches
match <- airfrance %>%
  group_by(match) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))
# cost_impressions column = cost/ impressions cost per impression
match$cost_impressions <- c()
for (row in 1:nrow(match)) {
  match$cost_impressions[row]<- round(match$cost[row] / match$impressions[row], 2)
}
# cost_clicks column = cost / clicks cost per clicks 
match$cost_clicks <- c()
for (row in 1:nrow(publisher)) {
  match$cost_clicks[row]<- round(match$cost[row] / match$clicks[row], 2)
}
# cost_bookings column = cost / bookings cost per bookings
match$cost_bookings <- c()
for (row in 1:nrow(publisher)) {
  match$cost_bookings[row]<- round(match$cost[row] / match$bookings[row], 2)
}

# Summarize the airfrance dataframe by campaigns
campaign <- airfrance %>%
  group_by(campaign) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))

# Summarize the airfrance dataframe by status
status <- airfrance %>%
  group_by(status) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))

# margin column = amount - cost to get net revenue
status$margin <- c()
for (row in 1:nrow(status)) {
  status$margin[row]<- round(status$amount[row] - status$cost[row], 2)
}
# return_rate column = amount / cost to get $ return on every dollar spent for each status
status$return_rate <- c()
for (row in 1:nrow(status)) {
  status$return_rate[row]<- round(status$amount[row] / status$cost[row], 2)

# Summarize the airfrance dataframe by publisher and status  
publisher_status <- airfrance %>%
  group_by(publisher, status) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))
}
# Add margin column = amount - cost to get net revenue
publisher_status$margin <- c()
for (row in 1:nrow(publisher_status)) {
  publisher_status$margin[row]<- round(publisher_status$amount[row] - publisher_status$cost[row], 2)
}
# return_rate column = amount / cost to get $ return on every dollar spent for each publisher and status
publisher_status$return_rate <- c()
for (row in 1:nrow(publisher_status)) {
  publisher_status$return_rate[row]<- round(publisher_status$amount[row] / publisher_status$cost[row], 2)
}
# Summarize the airfrance dataframe by publisher and campaign
publisher_campaign <- airfrance %>%
  group_by(publisher, campaign) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))

# Summarize the airfrance dataframe by campaign and publisher
campaign_publisher <- airfrance %>%
  group_by(campaign, publisher) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))

# Summarize the airfrance dataframe by campaign and status
campaign_status <- airfrance %>%
  group_by(campaign, status) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount))

# Add margin column = amount - cost to get net revenue
campaign_status$margin <- c()
for (row in 1:nrow(campaign_status)) {
  campaign_status$margin[row]<- round(campaign_status$amount[row] - campaign_status$cost[row], 2)
}

# Make a new column click per impression click/impression
airfrance$click_impression <-c()
for (row in 1:nrow(airfrance)) {
  airfrance$click_impression[row] = round(airfrance$clicks[row] / airfrance$impressions[row], 4)
}

# Subset the dataframe by the unsuccessful keywords
unsuccess <- airfrance[airfrance$success == 0,]

# Obtain the distribution of the unsuccessful keywords with the clicks per impression rate make a cut-off line at 25% for high potential keywords
ggplot(unsuccess, aes(click_impression)) + geom_histogram(binwidth = 0.03) + ggtitle("Distribution of unsuccessful keywords") + xlab("Impression to click conversion rate") + ylab("Number of keywords") +theme(plot.title = element_text(hjust = 0.5)) + xlim(0,1) + geom_vline(xintercept = 0.25, linetype = "dotted", color= "blue", size = 1.5)

# Summarize the unsuccess dataframe by publisher
unsuccess_publisher <- unsuccess %>%
  group_by(publisher) %>%
  summarise(impressions = sum(impressions), clicks = sum(clicks), cost = sum(cost), keyword_count = length(keyword))

# Add a total column in the unsuccess_publisher dataframe
unsuccess_publisher <- add_row(unsuccess_publisher, "publisher" = "Total")
for (col in 2:ncol(unsuccess_publisher)) {
  unsuccess_publisher[nrow(unsuccess_publisher), col] <- sum(unsuccess_publisher[, col], na.rm = TRUE)
}

# subset the dataframe with unsuccessful keywords with cut off at 25%
unsuccess_high <- unsuccess[unsuccess$click_impression >= 0.25,]
unsuccess_high_publisher <- unsuccess_high %>%
  group_by(publisher) %>%
  summarise(impressions = sum(impressions), clicks = sum(clicks), cost = sum(cost), keyword_count = length(keyword))

# Add a total row and sum by columns
unsuccess_high_publisher <- add_row(unsuccess_high_publisher, "publisher" = "Total")
for (col in 2:ncol(unsuccess_high_publisher)) {
  unsuccess_high_publisher[nrow(unsuccess_high_publisher), col] <- sum(unsuccess_high_publisher[, col], na.rm = TRUE)
}

# Unsuccess high publisher is without Overture - US so need to make an  empty row to make subtraction with unsuccess_publisher dataframe
# Overture - US is unsuccessful search engine
unsuccess_high_publisher <- add_row(unsuccess_high_publisher, "publisher" = "Overture - US", .after = 5)                                                             
for (col in 2:ncol(unsuccess_high_publisher)) {
  unsuccess_high_publisher[6, col] <- 0
}
# Create a empty data frame from unsuccess_publisher dataframe
unsuccess_low_publisher <- unsuccess_publisher[FALSE, ]
# Create the publisher column to unsuccess_low_publisher from unsuccess_publisher dataframe
for (row in 1:nrow(unsuccess_publisher)) {
  unsuccess_low_publisher[row, 1] <- unsuccess_publisher[row,1]
}
# Subtract unsuccess_publisher with unsuccess_high_publisher to obtain the cost of unsuccessful keywords with low conversion rate to clicks
for (col in 2:ncol(unsuccess_publisher)) {
  unsuccess_low_publisher[, col] <- unsuccess_publisher[, col] - unsuccess_high_publisher[, col]
}

# Create an empty data frame with the same column names as publisher2 dataframe
predict <- publisher2[FALSE, ]
# Obtain the publisher name excluding the Total row
for (row in 1:7) {
  predict[row, 1] <- publisher2[row, 1]
}
# Subtract the values in impressions and clicks column  of unsuccess_low_publisher from publisher2 
for (col in 2:3) {
  predict[1:7,col] <- publisher2[1:7,col] - unsuccess_low_publisher[1:7,col]
}
# bookings column remains the same
for (row in 1:7) {
  predict[row,c(4,6)] <- publisher2[row, c(4,6)]
}
# Subtract the cost and keyword count column of unsuccess_low_publisher from publisher2
for (row in 1:7) {
  predict[row,c(5,7)] <- publisher2[row, c(5,7)] - unsuccess_low_publisher[row,c(4,5)]
}
# Add kayak row to the predict dataframe
predict <- add_row(predict, "publisher" = kayak_df[1,1], "impressions" = kayak_df[1,2], "clicks" = kayak_df[1,3], "bookings" = kayak_df[1,4], "cost" = kayak_df[1,5], "amount" = kayak_df[1,6])
# Add total row to the predict dataframe as new varaible and calculate the total
predict2 <- add_row(predict, "publisher" = "Total")
for (col in 2:ncol(predict2)) {
  predict2[nrow(predict2), col] <- sum(predict2[, col], na.rm = TRUE)
}
# Add margin column = amount - cost to get net revenue
predict$margin <- c()
for (row in 1:nrow(predict)) {
  predict$margin[row]<- round(predict$amount[row] - predict$cost[row], 2)
}
# return_rate column = amount / cost to get $ return on every dollar spent 
predict$return_rate <- c()
for (row in 1:nrow(predict)) {
  predict$return_rate[row]<- round((predict$amount[row] / predict$cost[row]), 2)
}
# cost_impressions column = cost/ impressions cost per impression
predict$cost_impressions <- c()
for (row in 1:nrow(predict)) {
  predict$cost_impressions[row]<- round(predict$cost[row] / predict$impressions[row], 2)
}
# cost_clicks column = cost / clicks cost per clicks 
predict$cost_clicks <- c()
for (row in 1:nrow(predict)) {
  predict$cost_clicks[row]<- round(predict$cost[row] / predict$clicks[row], 2)
}
# cost_bookings column = cost / bookings cost per bookings
predict$cost_bookings <- c()
for (row in 1:nrow(predict)) {
  predict$cost_bookings[row]<- round(predict$cost[row] / predict$bookings[row], 2)
}
# clicks_bookings column = clicks / bookings clicks to lead to bookings
predict$clicks_bookings <- c()
for (row in 1:nrow(predict)) {
  predict$clicks_bookings[row]<- round(predict$clicks[row] / predict$bookings[row], 2)
}
# conversion_rate column = bookings / clicks conversion rate of clicks to bookings
predict$conversion_rate <- c()
for (row in 1:nrow(predict)) {
  predict$conversion_rate[row]<- round(predict$bookings[row] / predict$clicks[row], 3) *100
}
# Plot the change occured for the different search engines before and after getting rid of unnecessary keywords
ggplot(publisher, aes(x=conversion_rate, y=clicks, color = publisher)) + geom_point(aes(size = amount)) + xlab("Conversion rate") + ylab("Total Clicks") + ylim(0, 200000)
ggplot(predict, aes(x=conversion_rate, y=clicks, color = publisher)) + geom_point(aes(size = amount)) + xlab("Conversion rate") + ylab("Total Clicks") + ylim(0, 200000)

# Subset the dataframe with only the successful keywords
success <- airfrance[airfrance$success == 1,]
# Calculate the conversion rate of clicks to bookings
success$conversion_rate <- c()
for (row in 1:nrow(success)) {
  success$conversion_rate[row] <- round((success$bookings[row] / success$clicks[row]),2)
}
# See the distribution of the conversion rate
ggplot(success, aes(conversion_rate)) + geom_histogram(binwidth = 0.01) + xlim(0,1)
# Summarise the dataframe by the conversion rate
success_conversion_rate <- success %>%
  group_by(conversion_rate) %>%
  summarise(clicks = sum(clicks), bookings = sum(bookings), cost = round(sum(cost),2), amount = sum(amount), keyword_count = length(keyword))
# Add a new column of net_revenue
success_conversion_rate$net_revenue <- success_conversion_rate$amount - success_conversion_rate$cost
# Calculate the return_rate to get the $ cost spent to lead to bookings
success_conversion_rate$return_rate <- success_conversion_rate$amount / success_conversion_rate$cost
# 85 Keywords that return only $1.5 per bookings, conversion_rate below 1%

# Summarise the dataframe with keyword_length
keyword_length <- airfrance %>%
  group_by(keyword_length) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount), keyword_count = length(keyword))
# Add margin column = amount - cost to get net revenue
keyword_length$margin <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$margin[row]<- round(keyword_length$amount[row] - keyword_length$cost[row], 2)
}
# return_rate column = amount / cost to get $ return on every dollar spent
keyword_length$return_rate <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$return_rate[row]<- round((keyword_length$amount[row] / keyword_length$cost[row]), 2)
}
# cost_impressions column = cost/ impressions cost per impression
keyword_length$cost_impressions <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$cost_impressions[row]<- round(keyword_length$cost[row] / keyword_length$impressions[row], 2)
}
# cost_impressions column = cost/ clicks cost per clicks
keyword_length$cost_clicks <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$cost_clicks[row]<- round(keyword_length$cost[row] / keyword_length$clicks[row], 2)
}
# cost_bookings column = cost/ bookings cost per bookings
keyword_length$cost_bookings <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$cost_bookings[row]<- round(keyword_length$cost[row] / keyword_length$bookings[row], 2)
}
# clicks_bookings column = click/ bookings clicks needed to lead to booking
keyword_length$clicks_bookings <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$clicks_bookings[row]<- round(keyword_length$clicks[row] / keyword_length$bookings[row], 2)
}
# profit_margin column = margin/ amount margin percentage from amount(revenue) generated
keyword_length$profit_margin <- c()
for (row in 1:nrow(keyword_length)) {
  keyword_length$profit_margin[row]<- round(keyword_length$margin[row] / keyword_length$amount[row], 2)
}
# convert the infinite numbers to 0 for easier calculations
for (row in 1:nrow(keyword_length)) {
  if (is.infinite(keyword_length$profit_margin[row]) == TRUE) {keyword_length$profit_margin[row] <- 0}
}
# Plot the graph with the
summary(lm(keyword_length$profit_margin ~ poly(keyword_length$keyword_length, 2, raw = TRUE)))
ggplot(keyword_length, aes(x = keyword_length, y= profit_margin)) + geom_point() + ylim(-3, 1) + xlim(3, 37) + stat_smooth(method = "lm", formula = y~poly(x,2), size = 1)


#regression for keyword_length
length_reg<-glm(success~keyword_length, data=airfrance, family='binomial')
summary(length_reg)

mean(airfrance$keyword_length)
median(airfrance$keyword_length)

#Predicting success of future keywords according to their length

successprob<-function(beta_0,beta_length,x_length){
  logit<-beta_0+x_length*beta_length
  odds<-exp(logit)
  prob<-odds/(1+odds)
  return(c(odds,prob))
}
successprob(-0.69360,-0.10576,15)

probabilities<-c()
for (i in 4:45) {
  probabilities[i-3]<-c(successprob(-0.69360,-0.10576,i)[2])
}
probabilities
#Visualization of probabilites 
probabilities_df<-as.data.frame(probabilities)
bar4<-ggplot(probabilities_df, aes(4:45, probabilities))
bar4 + stat_summary(fun.y = mean, geom = "bar", fill = "blue", colour = "black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "keyword length", y = "Success Probability")

keyword_17<-c()
airfrance$keyword_length<-as.numeric(airfrance$keyword_length)
for (i in 1:nrow(airfrance)){
  if (airfrance$keyword_length[i]<=17){airfrance$keyword_17[i]<-1}
  else{airfrance$keyword_17[i]<-0}
}
#17 new sumary length less than 17 letters
airfrance_17 <- airfrance[!airfrance$keyword_17=="0",]
#summarize by publisher
publisher_17 <- airfrance_17 %>%
  group_by(publisher) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount), keyword_count= length(keyword))

#margin column = amount - cost to get the net revenue
publisher_17$margin <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$margin[row]<- round(publisher_17$amount[row] - publisher_17$cost[row], 2)
}
#return_rate column = amount / cost to get $ return for every $ spent
publisher_17$return_rate <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$return_rate[row]<- round((publisher_17$amount[row] / publisher_17$cost[row]), 2)
}
#cost_impression column = cost / impressions cost per impressions
publisher_17$cost_impressions <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$cost_impressions[row]<- round(publisher_17$cost[row] / publisher_17$impressions[row], 2)
}
#cost_clicks column = cost / clicks cost per clicks
publisher_17$cost_clicks <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$cost_clicks[row]<- round(publisher_17$cost[row] / publisher_17$clicks[row], 2)
}
#cost_bookings column = cost / bookings cost per bookings
publisher_17$cost_bookings <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$cost_bookings[row]<- round(publisher_17$cost[row] / publisher_17$bookings[row], 2)
}
#clicks_bookings column = clicks / bookings clicks required per bookings
publisher_17$clicks_bookings <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$clicks_bookings[row]<- round(publisher_17$clicks[row] / publisher_17$bookings[row], 2)
}
#add the row for total
publisher_17 <-add_row(publisher_17, "publisher" = "total")
for (col in 2:ncol(publisher_17)) {
  publisher_17[nrow(publisher_17),col]<- sum(publisher_17[, col], na.rm = TRUE)
}
#profit_margin column = margin / amount margin compared to the amount
publisher_17$profit_margin <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$profit_margin[row]<- round(publisher_17$margin[row] / publisher_17$amount[row], 2)
}
#cost_margin column = cost / amount cost compared to amount
publisher_17$cost_margin <- c()
for (row in 1:nrow(publisher_17)) {
  publisher_17$cost_margin[row]<- round(publisher_17$cost[row] / publisher_17$amount[row], 2)
}

# getting the table for optimal length 18
keyword_18<-c()
airfrance$keyword_length<-as.numeric(airfrance$keyword_length)
for (i in 1:nrow(airfrance)){
  if (airfrance$keyword_length[i]<=18){airfrance$keyword_18[i]<-1}
  else{airfrance$keyword_18[i]<-0}
}
#18 new sumary 
airfrance_18 <- airfrance[!airfrance$keyword_18=="0",]
#Summarise by publisher with length 18 keywords
publisher_18 <- airfrance_18 %>%
  group_by(publisher) %>% 
  summarise(impressions = sum(impressions), clicks = sum(clicks), bookings = sum(bookings), cost = sum(cost), amount = sum(amount), keyword_count= length(keyword))

#margin column = amount - cost to get the net revenue
publisher_18$margin <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$margin[row]<- round(publisher_18$amount[row] - publisher_18$cost[row], 2)
}
#return_rate column = amount / cost to get $ return for every $ spent
publisher_18$return_rate <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$return_rate[row]<- round((publisher_18$amount[row] / publisher_18$cost[row]), 2)
}
#cost_impression column = cost / impressions cost per impressions
publisher_18$cost_impressions <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$cost_impressions[row]<- round(publisher_18$cost[row] / publisher_18$impressions[row], 2)
}
#cost_clicks column = cost / clicks cost per clicks
publisher_18$cost_clicks <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$cost_clicks[row]<- round(publisher_18$cost[row] / publisher_18$clicks[row], 2)
}
#cost_bookings column = cost / bookings cost per bookings
publisher_18$cost_bookings <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$cost_bookings[row]<- round(publisher_18$cost[row] / publisher_18$bookings[row], 2)
}
#clicks_bookings column = clicks / bookings clicks required per bookings
publisher_18$clicks_bookings <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$clicks_bookings[row]<- round(publisher_18$clicks[row] / publisher_18$bookings[row], 2)
}
#add the total row to publisher_18
publisher_18 <-add_row(publisher_18, "publisher" = "total")
for (col in 2:ncol(publisher_18)) {
  publisher_18[nrow(publisher_18),col]<- sum(publisher_18[, col], na.rm = TRUE)
}
#profit_margin column = margin / amount margin compared to the amount
publisher_18$profit_margin <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$profit_margin[row]<- round(publisher_18$margin[row] / publisher_18$amount[row], 2)
}
#cost_margin column = cost / amount cost compared to amount
publisher_18$cost_margin <- c()
for (row in 1:nrow(publisher_18)) {
  publisher_18$cost_margin[row]<- round(publisher_18$cost[row] / publisher_18$amount[row], 2)
}

#Visualization, preparing tables for the presentation
pres_publisher_18 <- publisher_18 %>%
  group_by(publisher) %>% 
  summarise( Keywords=keyword_count, Bookings=bookings, Cost=cost, Revenue=amount, Net_Revenue=margin, Return_rate=return_rate, Cost_per_booking=cost_bookings, Clicks_per_booking=clicks_bookings,Cost_margin=cost_margin, Profit_margin=profit_margin)

pres_publisher_18_total <- publisher_18 %>%
  group_by(publisher) %>% 
  summarise( Keywords=keyword_count, Bookings=bookings, Cost=cost, Revenue=amount, Net_Revenue=margin, Return_rate=return_rate, Cost_per_booking=cost_bookings, Clicks_per_booking=clicks_bookings,Cost_margin=cost_margin, Profit_margin=profit_margin)

#profit_margin column = margin / amount margin compared to the amount
publisher$profit_margin <- c()
for (row in 1:nrow(publisher)) {
  publisher$profit_margin[row]<- round(publisher$margin[row] / publisher$amount[row], 2)
}
#cost_margin column = cost / amount cost compared to amount
publisher$cost_margin <- c()
for (row in 1:nrow(publisher)) {
  publisher$cost_margin[row]<- round(publisher$cost[row] / publisher$amount[row], 2)
}

pres_publisher <- publisher %>%
  group_by(publisher) %>% 
  summarise( Keywords=keyword_count, Bookings=bookings, Cost=cost, Revenue=amount, Net_Revenue=margin, Return_rate=return_rate, Cost_per_booking=cost_bookings, Clicks_per_booking=clicks_bookings,Cost_margin=cost_margin, Profit_margin=profit_margin)

pres_publisher_total <- publisher %>%
  group_by(publisher) %>% 
  summarise( Keywords=keyword_count, Bookings=bookings, Cost=cost, Revenue=amount, Net_Revenue=margin, Return_rate=return_rate, Cost_per_booking=cost_bookings, Clicks_per_booking=clicks_bookings,Cost_margin=cost_margin, Profit_margin=profit_margin)

library(Hmisc)
#number of bookings per publishhers
bookwk <- c(769,1548,129,140,372,289,662,208)
lbls <- c("Google-US", "Google-Global","MSN-Global","MSN-US","overture-Global","Overture-US","Yahoo-US","Kayak")
#percentage of distribution
pct <- round(bookwk/sum(bookwk)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%", sep="")
#Get pie chart
pie(bookwk, labels=lbls, col=heat.colors(length(lbls)), main = "Bookings")

library(scales)
#Clicks per bookings per publishers
Clicks_Bookings <- c(91.48,123.38,86.95,77.20,163.71,412.88,68.88,13.64)
Publishers <- paste(publisher$publisher)
# Make dataframe with relevant data
RRdf <- data.frame(Publishers, Clicks_Bookings)
# Make bar plot for clicks per bookings by publishers
RRdf.plot <- ggplot(data=RRdf, aes(x=Publishers, y=Clicks_Bookings, fill=Publishers)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "blue", "red", "blue","blue", "blue", "blue", "blue")) +
  geom_text(aes(y = Clicks_Bookings + 20, label = Clicks_Bookings)) +
  xlab(colnames(RRdf)[1]) +
  ylab(colnames(RRdf)[2]) +
  ggtitle("") 
RRdf.plot

# Make bar plot for cost per bookings by publishers
Cost_Bookings <- c(151.92,228,94.27,114.99,172.84,491.27,69.79,17.14)
Publishers <- paste(Publishers)
RRdf <- data.frame(Publishers, Cost_Bookings)

RRdf.plot <- ggplot(data=RRdf, aes(x=Publishers, y=Cost_Bookings, fill=Publishers)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "blue", "red", "blue","blue", "blue", "blue", "blue")) +
  geom_text(aes(y = Cost_Bookings + 20, label = Cost_Bookings)) +
  xlab(colnames(RRdf)[1]) +
  ylab(colnames(RRdf)[2]) +
  ggtitle("") 
RRdf.plot
# Make bar plot for rate of return by publishers
Return_Rate <- c(7.68, 4.94,11.97,11.28,6.69,2.45,19.10,65.5)
Publishers <- paste(Publishers)
RRdf <- data.frame(Publishers, Return_Rate)

RRdf.plot <- ggplot(data=RRdf, aes(x=Publishers, y=Return_Rate, fill=Publishers)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "blue", "red", "blue","blue", "blue", "blue", "blue")) +
  geom_text(aes(y = Return_Rate +5, label = Return_Rate)) +
  xlab(colnames(RRdf)[1]) +
  ylab(colnames(RRdf)[2]) +
  ggtitle("") 
RRdf.plot





