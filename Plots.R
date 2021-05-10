


#######################################################------------- PLOTS  -----------------------###################################################

#Plot Boxplots/ Stacked Barcharts / Histogram / Map (since there is country data).
# Grouped Boxplots  --  Categorical variables on x-axis and numerical variables on y-axis.

# Cleaned resort data and city data seperately if needed for plots
resort_data <- combined_data %>% dplyr::filter(HotelType == "resort")
city_data <- combined_data %>% dplyr::filter(HotelType == "city")


#######################---------- pieplots-----------#######################
### Shubham Malpani


#plot piechat for visitor type
#View(combined_data)
#View(visitortable)
visitortable <- data.frame(table(combined_data$VisitorType))
visitortable$percent <- round(visitortable$Freq/sum(visitortable$Freq) * 100,digits = 2)
visitorplot <- ggplot(visitortable, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +
  geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Visitor Type")) + 
  theme_void()
visitorplot

#meal,visitortype,customertype,season,deposit#plot
#plot piechart for mealplan
mealplan <-  data.frame(table(combined_data$Meal))
View(mealplan)
mealplan$percent <- round(mealplan$Freq/sum(mealplan$Freq) * 100,digits = 2)
mealplanplot <- ggplot(mealplan, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Meal Plan"))+ theme_void()
mealplanplot

#plot piechart for customertype
customertypeframe <-  data.frame(table(combined_data$CustomerType))
View(customertypeframe)
customertypeframe$percent <- round(customertypeframe$Freq/sum(customertypeframe$Freq) * 100,digits = 2)
customertypeplot <- ggplot(customertypeframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Customer Type"))+ theme_void()
customertypeplot

#plot piechart for season
seasonframe <-  data.frame(table(combined_data$Season))
View(seasonframe)
seasonframe$percent <- round(seasonframe$Freq/sum(seasonframe$Freq) * 100,digits = 2)
seasonframeplot <- ggplot(seasonframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Season"))+ theme_void()
seasonframeplot

#plot piechart for deposit
depositframe <-  data.frame(table(combined_data$DepositType))
View(seasonframe)
depositframe$percent <- round(depositframe$Freq/sum(depositframe$Freq) * 100,digits = 2)
depositframeplot <- ggplot(depositframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1,color = "white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Deposit Status"))+ theme_void()
depositframeplot






############### -------------- Histogram, Barcharts and Boxplots -------------#################################


######################-------------------- Plot 1 - Analyzing ADR by type of visitors and season for resort and City --------------- ##########################

### RESORT
# Grouping by visitortype and season. Once you see the dataframe, it will be clear what I did using the below command. 
Season_VisitorType_ADR_resort <- resort_data %>% group_by(VisitorType, Season) %>% summarise(mean_ADR = mean(ADR)) 
View(Season_VisitorType_ADR_resort)

#Creating plot
Season_VisitorType_ADR_resort_plot <-
  ggplot(data = Season_VisitorType_ADR_resort,
         mapping = aes(x = Season, y = mean_ADR, fill = VisitorType)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Resort",
       x = "Season",
       y = "Average Daily Rate (in Euros)",
       fill = "Type of Visitor") +
  scale_y_continuous(breaks = seq(0,200,25))

#Plotting figure
Season_VisitorType_ADR_resort_plot


###### CITY
Season_VisitorType_ADR_city <- city_data %>% group_by(VisitorType, Season) %>% summarise(mean_ADR = mean(ADR)) 
View(Season_VisitorType_ADR_city)

#Creating plot
Season_VisitorType_ADR_city_plot <-
  ggplot(data = Season_VisitorType_ADR_city,
         mapping = aes(x = Season, y = mean_ADR, fill = VisitorType)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "City",
       x = "Season",
       y = "Average Daily Rate (in Euros)",
       fill = "Type of Visitor") +
  scale_y_continuous(breaks = seq(0,200,25))

#Plotting figure
Season_VisitorType_ADR_city_plot


# Creating both plots in same figure
grid.arrange(Season_VisitorType_ADR_resort_plot, Season_VisitorType_ADR_city_plot, nrow = 2)

####### WRITE THE BELOW INTERPRETATION AGAIN!!!

# ADR for visitor types of families is the highest regardless of the season, so offering discounts for 
###families to attract more family travelers, especially in the summer, is a good way to increase overall ADR.
###2. In addition, the ADR for visitor types of couples and singles is similar in most seasons, higher for 
###couples in the spring and higher for singles in the summer. Therefore, we should make it a secondary 
###priority to attract couple type visitors to stay in spring, and try to attract more single visitors in summer.

################------------------Plot 1 ends-----------------------#########################################



############## ----------  Plot 2 - # Does booking earlier mean more cancelations? - Lead Time vs IsCancelled ------- #########

ggplot(data = combined_data, aes(x = HotelType, y = LeadTime, fill = IsCanceled)) + 
  geom_boxplot(position = position_dodge()) + 
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Does booking earlier mean more Cancelation?",
       x = "Type of Hotel",
       y = "How many days before the booking was made?",
       fill = "Cancelation Rate") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(0,600,40))


########## --------------- Plot 2 ends -----------------#############



######## ------------- Plot 3 Season_TotalNumberOfDaysStayed_ADR ------------ #############################


## RESORT
Season_TotalNumberOfDaysStayed_ADR_resort <- resort_data %>% group_by(Season, TotalNumberOfDaysStayed) %>% summarise(mean_ADR = mean(ADR))
View(Season_TotalNumberOfDaysStayed_ADR_resort)

Season_TotalNumberOfDaysStayed_ADR_resort_plot <-
  ggplot(
    data = Season_TotalNumberOfDaysStayed_ADR_resort,
    mapping = aes(x = TotalNumberOfDaysStayed, y = mean_ADR, color = Season)
  ) +
  geom_line(size = 1.3) +
  labs(title = "How does number of days stayed affect Price per night in Resort hotels",
       x = "Total number of days stayed",
       y = "Average Daily Rate (in Euros)",
       fill = "Season") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(0,15,1)) + 
  scale_y_continuous(breaks = seq(0,200,10)) 

Season_TotalNumberOfDaysStayed_ADR_resort_plot


#CITY
## RESORT
Season_TotalNumberOfDaysStayed_ADR_city <- city_data %>% group_by(Season, TotalNumberOfDaysStayed) %>% summarise(mean_ADR = mean(ADR))
View(Season_TotalNumberOfDaysStayed_ADR_city)

Season_TotalNumberOfDaysStayed_ADR_city_plot <-
  ggplot(
    data = Season_TotalNumberOfDaysStayed_ADR_city,
    mapping = aes(x = TotalNumberOfDaysStayed, y = mean_ADR, color = Season)
  ) +
  geom_line(size = 1.3) +
  labs(title = "How does number of days stayed affect Price per night in city hotels",
       x = "Total number of days stayed",
       y = "Average Daily Rate (in Euros)",
       fill = "Season") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(0,15,1)) + 
  scale_y_continuous(breaks = seq(80,200,10)) 

Season_TotalNumberOfDaysStayed_ADR_city_plot


#### WRITE THE BELOW INTERPRETATION AGAIN!!!

# In spring, ADR is highest for stays around 20 days and lower for stays around 16 or 17 days and after 23 
###days. Therefore, visitors with stays around 16 or 17 days, or greater than 23 days should be enticed to
###upgrade their stays to around 20 days by adjusting the price or offering some free services.
###2. In summer, when the length of stay is 10-20 days, the ADR fluctuates sharply. Therefore, hotels can 
###control the length of stay at 10, 13 or 16 days by offering package deals.
###3. In fall, the ADR is at its highest at 14 days of stay, so hotels should offer trips with 14 days of stay.
###Also when the number of stay days is greater than 18, ADR drops significantly and is at a lower level 
###until 26 days when it returns to normal. Therefore for visitors who stay in greater than 18 but less than 
###26 days, hotel should increase the rates to avoid the number of days of stay in this time period.
###4. In winter, ADR rises rapidly when the number of days of stay is greater than 28 days, so hotels should 
###attract visitors for longer stays (greater than 28 days) in winter.



############## ------------- Plot 3 ends ------------------ ##############################





################### ------------------- Plot 4 - Visitor type vs number of days stayed  -------------- ###################


ggplot(data = combined_data, aes(x = VisitorType, y = TotalNumberOfDaysStayed, fill = HotelType)) + 
  geom_boxplot(position = position_dodge(0.75), width = 0.6, outlier.shape = NA) + 
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Visitor Type vs Number of Days",
       x = "Type of Visitor",
       y = "Number of Days Stayed",
       fill = "Hotel Type") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(0,15,1))



# Ratio of cancelation in city Hotel is higher. 


################### ------------------- Plot 4 ends -------------- ###################




###########-------- Plot 5 ------------- Total No of special Requests vs Number of adults (irrespective of whether they bring babies/Children) ###########

table(combined_data$TotalOfSpecialRequests)
table(combined_data$Adults)

# Who makes more requests. 

# Can filter, group_by and summarise in same command (AMAZING!!)
Requests_Adults <- combined_data %>% filter(Adults<4) %>% group_by(AnySpecialRequest, Adults) %>% summarise(count = n(), mean_ADR = mean(ADR))


ggplot(data = Requests_Adults, aes(x = Adults, y = count, fill = AnySpecialRequest)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Adults vs Number of Special Requests",
       x = "Number of Adults",
       y = "Count",
       fill = "Any Special Requests?") +
  scale_y_continuous(breaks = seq(0, 50000, 5000)) +
  scale_x_continuous(breaks = seq(0,3,1)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 10))



# As the number of Adults increases, requests also increases.

################## ----------- ADR -------------- #################


ggplot(data = Requests_Adults, aes(x = Adults, y = mean_ADR, fill = AnySpecialRequest)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Adults vs Number of Special Requests",
       x = "Number of Adults",
       y = "Average Daily Rate (in Euros)",
       fill = "Any Special Requests?") +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  scale_x_continuous(breaks = seq(0,3,1)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 10))


# If any special requests are made, then ADR is more. 


############################---------------- Plot 5 ends-----------######################################




###### CAN ADD ALL PLOTS FROM Month_wise_analysis.R




##################### ---------------- MAP PLOT ------------------ #########################################
#########-----  Hanwen -------##########
# Map —— Number Of Reservations From Different Countries
Country_city <- city_data[-which(city_data$Country=="NULL"),]
table(Country_city$Country)
Country_city <- data.frame(table(Country_city$Country))
Country_city$Var1 <- as.character(Country_city$Var1)
colnames(Country_city) <- c("Country","NumberOfReservation")
world <- map_data('world')
library(countrycode)
Country_city$Country[which(Country_city$Country=="CN")] <- "CHN"
Country_city$Country[which(Country_city$Country=="TMP")] <- "TLS"
Country_city$Country_Name <- countrycode(Country_city$Country,"iso3c","country.name")
Country_city$Country_Name[which(Country_city$Country=="USA")] <- "USA"
dfNew_city <- merge(world, Country_city, all.x=TRUE, by.x="region", by.y="Country_Name")
dfNew_city <- dfNew_city[order(dfNew_city[,5]),]
mp_city <- ggplot(dfNew_city, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= NumberOfReservation), colour = "white") +
  scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
  scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
  scale_fill_gradient(low = "blue", high="red") +
  labs(title="Number Of Reservations From Different Countries",
       y="Latitude", x="Longitude") +
  theme_light() 
mp_city

Country_resort <- resort_data[-which(resort_data$Country=="NULL"),]
table(Country_resort$Country)
Country_resort <- data.frame(table(Country_resort$Country))
Country_resort$Var1 <- as.character(Country_resort$Var1)
colnames(Country_resort) <- c("Country","NumberOfReservation")
world <- map_data('world')
library(countrycode)
Country_resort$Country[which(Country_resort$Country=="CN")] <- "CHN"
Country_resort$Country[which(Country_resort$Country=="TMP")] <- "TLS"
Country_resort$Country_Name <- countrycode(Country_resort$Country,"iso3c","country.name")
Country_resort$Country_Name[which(Country_resort$Country=="USA")] <- "USA"
dfNew_resort <- merge(world, Country_resort, all.x=TRUE, by.x="region", by.y="Country_Name")
dfNew_resort <- dfNew_resort[order(dfNew_resort[,5]),]
mp_resort <- ggplot(dfNew_resort, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= NumberOfReservation), colour = "white") +
  scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
  scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
  scale_fill_gradient(low = "blue", high="red") +
  labs(title="Number Of Reservations From Different Countries",
       y="Latitude", x="Longitude") +
  theme_light() + coord_cartesian()
mp_resort
##################################################################################


