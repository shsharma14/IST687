
# RESPONSE/DEPENDENT/OUTCOME Variable - ADR
# CATEGORICAL COLUMNS (By Default) - ReservationStatus, Meal, Country, MarketSegment, DistributionChannel, ReservedRoomType, AssignedRoomType, DepositType, 
# CustomerType
# Numerical Columns - Lead Time, StaysInWeekendNights, StaysInWeekNights, Adults, Children, Babies, PreviousCancellations, PreviousBookingsNotCanceled, BookingChanges,
# BookingChanges, DaysInWaitingList, ADR, RequiredCarParkingSpaces, TotalOfSpecialRequests
# Date Columns - Arrival.Date, ReservationStatusDate
# Ambiguous Columms - Agent (is a character column containing agent IDs), Company


cat('\014')
rm(list=ls())

EnsurePackage <- function(library) {
  library <- as.character(library)
  if(!require(library, character.only = T)){
    install.packages(pkgs = library,  repos="http://cran.r-project.org")
    require(library, character.only = T)
  }
}

EnsurePackage("gdata")
EnsurePackage("ggplot2")
EnsurePackage("readxl")
EnsurePackage("dplyr")
EnsurePackage("gridExtra")
EnsurePackage("ggmap")
EnsurePackage("ggrepel")

#################################### Reading in the Data #################

getwd()
setwd("C:/Users/shubh/Desktop/IST 687/Final_Project")   # Set working directory according to your config

resort_data <- data.frame(read_excel("H1-Resort.xlsx"))
city_data <- read_excel("H2-City.xlsx")


# colnames(resort_data)
# colnames(city_data)
"Group       "

########### Combining both dataset for better analysis.

# To combine rows using rbind, we need to match the row names. In both dataset, one column name is different, "Arrival.Date" in resort data & "Arrival Date" in city data

# converting  columns having dates as character to date data type. (Only 2 columns.....) and renaming arrival date column in both to apply rbind.

resort_data$ArrivalDate <- c(1:nrow(resort_data))
resort_data$ArrivalDate <- as.Date(resort_data$Arrival.Date)
resort_data$Arrival.Date <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)


city_data$ArrivalDate <- c(1:nrow(city_data))
city_data$ArrivalDate <- as.Date(city_data$'Arrival Date')
city_data$'Arrival Date' <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)

city_data$ReservationStatusDate <- as.Date(city_data$ReservationStatusDate)


resort_data$HotelType <- "resort"
city_data$HotelType <- "city"


# Now, we can combine the data.
combined_data <- rbind(city_data, resort_data)

#################################  CLEANING  DATA #########################################################################################################################
# -------------------------------------------------------------------------------------------------
###########################################################################################################################################################################


# View(combined_data)

# str(combined_data)

# summary(combined_data)  # Checking descriptive stats of columns.


# removing white spaces from categorical columns (https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r)
# because Deposit type column contains values like "No Deposit       " instead of "No Deposit".      
cols_to_be_rectified <- names(combined_data)[vapply(combined_data, is.character, logical(1))]
combined_data[,cols_to_be_rectified] <- lapply(combined_data[,cols_to_be_rectified], trimws)



############ MAKING COLUMNS PROPER DATATYPE  #######

# https://datascience.stackexchange.com/questions/12018/when-to-choose-character-instead-of-factor-in-r

####### Convert character columns to factors and character columns containing numeric values to num.
#combined_data[sapply(combined_data, is.character)] <-
#  lapply(combined_data[sapply(combined_data, is.character)], as.factor)


combined_data$IsCanceled <- factor(combined_data$IsCanceled, labels = c("Not Canceled", "Canceled"))
combined_data$ReservationStatus <- as.factor(combined_data$ReservationStatus)
combined_data$Children <- as.numeric(combined_data$Children)
combined_data$Meal <- as.factor(combined_data$Meal)
combined_data$Country <- as.factor(combined_data$Country)
combined_data$MarketSegment <- as.factor(combined_data$MarketSegment)
combined_data$DistributionChannel <- as.factor(combined_data$DistributionChannel)
combined_data$IsRepeatedGuest <- factor(combined_data$IsRepeatedGuest,  labels = c("Not Repeated", "Repeated"))
combined_data$ReservedRoomType <- as.factor(combined_data$ReservedRoomType)
combined_data$AssignedRoomType <- as.factor(combined_data$AssignedRoomType)
combined_data$DepositType <- as.factor(combined_data$DepositType)
combined_data$CustomerType <- as.factor(combined_data$CustomerType)
combined_data$HotelType <- as.factor(combined_data$HotelType)


# str(combined_data)


#----------------------- checking for missing data  -------------------------

colSums(is.na(combined_data)) # Checking which columns have NA values.
# Arrival date has 39270 missing values. 
# we can get those values using ReservationStatusDate and TotalNumberOfDaysStayedWeekNIghts and TotalNumberOfDaysStayedWeekendNights where reservationstatus is checkout.

combined_data$ArrivalDate[which(is.na(combined_data$ArrivalDate) & combined_data$ReservationStatus == 'Check-Out')] <-
  combined_data$ReservationStatusDate - (combined_data$StaysInWeekendNights + combined_data$StaysInWeekNights)

# Now, 1519 rows remain that have NA in arrival date. We will remove those NA values using na.omit().
colSums(is.na(combined_data))  

# Removing all NA data.
combined_data <- na.omit(combined_data)   

anyNA(combined_data)   # FALSE meaning no NA values in dataset.


#################------------------- Adding new columns based on arrival date  -----------------##########################

#### SEASON COLUMN ######

EnsurePackage("lubridate")
combined_data$Season <- quarter(combined_data$ArrivalDate)
combined_data$Season[combined_data$Season == 1] <- "Spring"
combined_data$Season[combined_data$Season == 2] <- "Summer"
combined_data$Season[combined_data$Season == 3] <- "Fall"
combined_data$Season[combined_data$Season == 4] <- "Winter"

combined_data$Season <- as.factor(combined_data$Season)


### Arrival Year column ######

combined_data$ArrivalYear <- as.factor(format(combined_data$ArrivalDate, "%Y"))

#### Arrival Month Column ######

combined_data$ArrivalMonth <- as.factor(month.name[as.numeric(format(combined_data$ArrivalDate, "%m"))])


#######-----------------------Adding new categorical column, ParkingSpaceNeeded and AnySpecialRequests based on column 
# RequiredCarParkingSpaces and TotalOfSpecialRequests ----------------------------###########################
### Might be useful for association rule mining

combined_data$ParkingSpaceNeeded <- as.factor(ifelse(combined_data$RequiredCarParkingSpaces > 0 , "Yes", "No"))

combined_data$AnySpecialRequest <- as.factor(ifelse(combined_data$TotalOfSpecialRequests > 0 , "Yes", "No"))



###################----------------- adding new column Visitor Type. ----------------####################
########### Visitor Type can be Business Traveler, Solo Traveler, Couple and Family ########

table(combined_data$Adults)  # Can remove 0 adults column since babies and children won't visit alone assuming children need to be accompanied by an adult.

# Removing using filter command.
combined_data <- combined_data %>% dplyr::filter(Adults != 0)

# ADR is zero for adults greater than 4.  ( 16 rows)
combined_data$ADR[combined_data$Adults > 4]

# Removing those 16 rows as well.
combined_data <- combined_data %>% dplyr::filter(Adults < 5)

'%notin%' <- Negate('%in%')

# Adding the column
combined_data$VisitorType <- c(1:nrow(combined_data))   # Initializing

# SOLO TRAVELER
combined_data$VisitorType[which(combined_data$Adults == 1 & combined_data$Babies == 0 & combined_data$Children == 0  & 
                                  combined_data$CustomerType %notin% "Group" )] <- "Solo Traveler"

# COUPLE
combined_data$VisitorType[which(combined_data$Adults == 2 & combined_data$Babies == 0 & combined_data$Children == 0  & 
                                  combined_data$CustomerType %notin% "Group" )] <- "Couple"

# FAMILY
combined_data$VisitorType[which(combined_data$Adults >= 2 & (combined_data$Babies != 0 | combined_data$Children != 0 ) & combined_data$DistributionChannel != "Corporate" &
                                  combined_data$CustomerType %notin% "Contract")] <- "Family"

# BUSINESS TRAVEL
combined_data$VisitorType[which(combined_data$Babies == 0 & combined_data$Children == 0 & combined_data$DistributionChannel == "Corporate")] <- "Business Travel"


dataframe <-
  combined_data[which(
    combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")
  ),
  c("Adults",
    "Children",
    "Babies",
    "DistributionChannel",
    "CustomerType")]

#Analyzing above dataframe, remaining rows are majorly 3 adults having either 1 or more children or 1 or more babies and distribution channel as TA/TO - can classify 
# them as families 

# FAMILY
combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel") & combined_data$Adults >= 3 
                                & combined_data$DistributionChannel != "Corporate" & combined_data$CustomerType %notin% "Contract")] <- "Family"
# Families rarely will have contract bookings, so excluded Customer Type as Contract.

# Checking rows which are left to be labeled a visitor type
length(combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family"))])      # 1214 rows.

# Removing remaining 1214 rows.
combined_data <- combined_data %>% slice(-which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")))





#########################-------------Analyzing IsCanceled, ReservationStatus for any mismatched data-------------------##############################


#                         ReservationStatus
# IsCancelled      
#                 Check-Out         No Show        Canceled
# Canceled				   X                 
# Not Canceled                                        X 

# There shouldn't be any rows where ReservationStatus is Check-Out and Iscanceled shows Canceled. 
length(which(combined_data$IsCanceled == "Canceled" & combined_data$ReservationStatus %in% "Check-Out"))  # 0 rows. THIS IS FINE!!!!

# Similarly, there shouldn't be any rows where reservation Status is Canceled and IsCanceled contains Not Canceled.
length(which(combined_data$IsCanceled == "Not Canceled" & combined_data$ReservationStatus %in% "Canceled")) # 0 rows. THIS IS FINE!!!!   


####################----------------creating new Columns 1. Total Number of Days Stayed and 2. Average Revenue per Stay -------------############################### 

# Summing up no of days stayed in Weekend and Weekdays
combined_data$TotalNumberOfDaysStayed <- combined_data$StaysInWeekendNights + combined_data$StaysInWeekNights

ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = TotalNumberOfDaysStayed)) + coord_flip() # So many outliers.

# Checking how many people stayed for more than 15 days.
length(which(combined_data$TotalNumberOfDaysStayed > 14))  # 419 rows

# We can remove these 419 rows since TotalNumberOfDaysStayedg at a hotel for a max of 2 weeks seems logical. 

combined_data <- combined_data %>% filter(TotalNumberOfDaysStayed <15)

# Checking how many rows have total number of days stayed as zero.
length(which(combined_data$TotalNumberOfDaysStayed == 0))   # Does it make sense to have this as 0? I dont think so.

# Removing rows where TotalNumberOfDaysStayed = 0
combined_data <- combined_data %>% slice(-which(combined_data$TotalNumberOfDaysStayed == 0))  

# Before creating new column, let's get rid of outliers in ADR.
summary(combined_data$ADR) # This shows 3rd quartile is 126 while max is 5400.

# Boxplot of ADR is difficult to analyze because of that max value. We will remove that big outlier.
ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = ADR)) + coord_flip() 

# removing that one big outlier of ADR
combined_data <- combined_data[-which(combined_data$ADR > 5000),] 

# Now draw boxplot again.
ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = ADR)) + coord_flip()  

#New Column 
combined_data$AvgRevenuePerStay <- combined_data$TotalNumberOfDaysStayed * combined_data$ADR


####################----------------creating new Column ADRType (Categorizing ADR values by a range) -------------###############################

# Histogram of ADR 
ggplot(data = combined_data) + geom_histogram(mapping = aes(x = ADR),color = "white") +
  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) + 
  geom_vline(xintercept = quantile(combined_data$ADR, 0.75), col = "red") +
  geom_vline(xintercept = quantile(combined_data$ADR, 0.15), col = "blue")

# Based on the plot, maybe we can create a new categorical column where 0-50 can be low ADR, 50-150 can be medium ADR and above 150 can be high ADR.
combined_data$ADRType <- cut(combined_data$ADR, breaks = c(0,50,150, Inf), labels = c("Low", "Medium", "High"), include.lowest = T)



######################   ------- Final Check for NA values after creating new columns ###################################

colSums(is.na(combined_data))   # ADRType has 1 NA.

# Removing that 1 NA value
combined_data <- na.omit(combined_data)


################---------------------- Looking for Outliers --------------------------------------------#########################

EnsurePackage("dlookr")
diagnose_outlier(combined_data)

str(combined_data)



################################################   CLEANING DONE!!!!!! READY TO MAKE PLOTS ############################################
# -------------------------------------------------------------------------------------------------------------------------------------
#######################################################################################################################################





############ Tables for variables. #################
str(combined_data)

sort(table(combined_data$IsCanceled), decreasing = T)
sort(table(combined_data$ReservationStatus), decreasing = T)

sort(table(combined_data$StaysInWeekendNights), decreasing = T)
sort(table(combined_data$StaysInWeekNights), decreasing = T)
sort(table(combined_data$Adults), decreasing = T)
sort(table(combined_data$Children), decreasing = T)
sort(table(combined_data$Babies), decreasing = T)

sort(table(combined_data$Meal), decreasing = T)
sort(table(combined_data$Country), decreasing = T)
sort(table(combined_data$MarketSegment), decreasing = T)
sort(table(combined_data$DistributionChannel), decreasing = T)
sort(table(combined_data$IsRepeatedGuest), decreasing = T)

sort(table(combined_data$PreviousCancellations), decreasing = T)
sort(table(combined_data$PreviousBookingsNotCanceled), decreasing = T)

sort(table(combined_data$ReservedRoomType), decreasing = T)
sort(table(combined_data$AssignedRoomType), decreasing = T)
sort(table(combined_data$BookingChanges), decreasing = T)
sort(table(combined_data$DepositType), decreasing = T)
sort(table(combined_data$DaysInWaitingList), decreasing = T)
sort(table(combined_data$CustomerType), decreasing = T)
sort(table(combined_data$RequiredCarParkingSpaces), decreasing = T)
sort(table(combined_data$TotalOfSpecialRequests), decreasing = T)
sort(table(combined_data$HotelType), decreasing = T)

sort(table(combined_data$ArrivalYear), decreasing = T)
sort(table(combined_data$ArrivalMonth), decreasing = T)
sort(table(combined_data$Season), decreasing = T)

sort(table(combined_data$ParkingSpaceNeeded), decreasing = T)

sort(table(combined_data$AnySpecialRequest), decreasing = T)


sort(table(combined_data$VisitorType), decreasing = T)
sort(table(combined_data$ADRType), decreasing = T)







########################## 


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



################### ------------------- Plot 4 - Which hotel type had a better ADR? City or resort? -------------- ###################


mean_stats_hotelType <- combined_data %>% group_by(HotelType) %>% summarise(mean = mean(ADR),count = n())

# create the plot
ggplot(data = combined_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(HotelType ~ .) + scale_x_continuous(breaks = seq(0,500,50)) +
  scale_fill_gradient("Count", low="orange", high="red") + 
  geom_vline(data = mean_stats_hotelType, aes(xintercept = mean, color = HotelType), size = 1) + 
  theme_bw() +
  labs(title = "Distribution of ADR for each type of Hotel",
       x = "Average Daily Rate (in Euros)",
       y = "Count") + 
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) 


################### ------------------- Plot 4 ends -------------- ###################



################### ------------------- Plot 5 - Which Hotel had more cancelations? - Resort or City?  -------------- ###################


IsCanceled_HotelType <- combined_data %>% group_by(HotelType, IsCanceled) %>% summarise(count = n())

ggplot(data = IsCanceled_HotelType, aes(x = HotelType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Cancelation Rate Hotel Wise",
       x = "Type of Hotel",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,50000,5000))


# Ratio of cancelation in city Hotel is higher. 


################### ------------------- Plot 5 ends -------------- ###################





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





#####################------------------ NOT SO USEFUL PLOTS (IGNORE)   ----------- ####################################################################

###### Multiple Histograms from Grouped Data   ###############
# https://r-graphics.org/recipe-distribution-multi-hist

# 1. Analyzing ADR based on season (Seeing the distribution and Mean(ADR) season wise) ###################

# Calculate mean of ADR season wise to add a line to plot 
# https://stackoverflow.com/questions/63016568/how-to-plot-multiple-mean-lines-in-a-single-histogram-with-multiple-groups-prese
mean_stats_season_wise <- resort_data %>% group_by(Season) %>% summarise(mean = mean(ADR),n = n())

# create the plot
ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(Season ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  #  scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_season_wise, aes(xintercept = mean, color = Season), size = 1) + 
  theme_grey()


# Fall season, ADR is high as compared to other seasons. 
# maybe, we can think of something that will increase ADR in spring season and winter season. 


#2. Analyzing ADR based on VisitorType (Seeing the distribution and Mean(ADR) season wise) ###################

# Calculate mean of ADR VisitorType wise to add a line to plot 
# https://stackoverflow.com/questions/63016568/how-to-plot-multiple-mean-lines-in-a-single-histogram-with-multiple-groups-prese
mean_stats_VisitorType_wise <- resort_data %>% group_by(VisitorType) %>% summarise(mean = mean(ADR),n = n())

# create the plot
ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(VisitorType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_VisitorType_wise, aes(xintercept = mean, color = VisitorType), size = 1)

