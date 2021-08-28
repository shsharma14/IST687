



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
EnsurePackage("scales")
EnsurePackage("patchwork")
EnsurePackage("arules")
EnsurePackage("arulesViz")
EnsurePackage("lubridate")
EnsurePackage("caTools")
EnsurePackage('caret')
EnsurePackage("kernlab")

#################################### Reading in the Data #################

getwd()
setwd("C:/Users/shubh/Desktop/IST 687/Final_Project")   # Set working directory according to your config

resort_data <- data.frame(read_excel("H1-Resort.xlsx"))
city_data <- read_excel("H2-City.xlsx")


summary(resort_data)

# colnames(resort_data)
# colnames(city_data)


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



############ Tables for variables. #################
str(combined_data)

sort(table(combined_data$IsCanceled), decreasing = T)

sort(table(combined_data$ReservationStatus), decreasing = T)
# Remove that one row containing '`` in Reservation Status
combined_data <- combined_data %>% filter(ReservationStatus != "`")


sort(table(combined_data$StaysInWeekendNights), decreasing = T)
sort(table(combined_data$StaysInWeekNights), decreasing = T)
sort(table(combined_data$Adults), decreasing = T)
sort(table(combined_data$Children), decreasing = T)
sort(table(combined_data$Babies), decreasing = T)

sort(table(combined_data$Meal), decreasing = T)
## Meal Type Undefined and SC are same. Will rename Undefined as SC.
combined_data$Meal[which(combined_data$Meal == "Undefined")] <- "SC"


sort(table(combined_data$Country), decreasing = T)
sort(table(combined_data$MarketSegment), decreasing = T)
# Removing 2 undefined rows for Market Segment - 
combined_data <- combined_data %>% filter(MarketSegment != "Undefined")

sort(table(combined_data$DistributionChannel), decreasing = T)
# Removing that 1 undefined row for distribution Channel - 
combined_data <- combined_data %>% filter(DistributionChannel != "Undefined")


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

## After removing undefined and ambiguous rows --- 
############ MAKING COLUMNS PROPER DATATYPE  ####### (Converting into factor)

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

rm(dataframe)

# FAMILY
combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel") & combined_data$Adults >= 3 
                                & combined_data$DistributionChannel != "Corporate" & combined_data$CustomerType %notin% "Contract")] <- "Family"
# Families rarely will have contract bookings, so excluded Customer Type as Contract.

# Checking rows which are left to be labeled a visitor type
length(combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family"))])      # 1214 rows.

# Removing remaining 1214 rows.
combined_data <- combined_data %>% slice(-which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")))


table(combined_data$VisitorType)
combined_data$VisitorType <- as.factor(combined_data$VisitorType)

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



# Cleaned resort data and city data seperately if needed for plots
resort_data <- combined_data %>% dplyr::filter(HotelType == "resort")
city_data <- combined_data %>% dplyr::filter(HotelType == "city")


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


####################################################################################################






#######################################################------------- PLOTS  -----------------------###################################################

#Plot Boxplots/ Stacked Barcharts / Histogram / Map (since there is country data).
# Grouped Boxplots  --  Categorical variables on x-axis and numerical variables on y-axis.

# Cleaned resort data and city data seperately if needed for plots
resort_data <- combined_data %>% dplyr::filter(HotelType == "resort")
city_data <- combined_data %>% dplyr::filter(HotelType == "city")


#######################---------- Basic Plots-----------#######################

#plot piechat for visitor type and Season. Plot barplots for other categorical columns. 


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=13, face="bold"),
    legend.title=element_text(size=13, face="bold")
  )

# ------------------------------------------------------------------------------------------------
# Visitor Type   ( This is fine!!)

# resort
resort_VisitorType_count <- resort_data %>% group_by(VisitorType) %>% summarise(count = n())

resort_VisitorType_count_plot <- ggplot(data = resort_VisitorType_count, aes(x ="", y = count, fill = VisitorType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("Resort Hotels") +
  geom_text(aes(x = 1.7, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 


#city
city_VisitorType_count <- city_data %>% group_by(VisitorType) %>% summarise(count = n())

city_VisitorType_count_plot <- ggplot(data = city_VisitorType_count, aes(x ="", y = count, fill = VisitorType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("City Hotels") +
  geom_text(aes(x = 1.7, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 


combined <- resort_VisitorType_count_plot + city_VisitorType_count_plot
combined + plot_layout(guides = "collect")

# ------------------------------------------------------------------------------------------------


# Season   ( This is fine!!)

# resort
resort_Season_count <- resort_data %>% group_by(Season) %>% summarise(count = n())

resort_Season_count_plot <- ggplot(data = resort_Season_count, aes(x ="", y = count, fill = Season)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("Resort Hotels") +
  geom_text(aes(x = 1.7, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 


#city
city_Season_count <- city_data %>% group_by(Season) %>% summarise(count = n())

city_Season_count_plot <- ggplot(data = city_Season_count, aes(x ="", y = count, fill = Season)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("City Hotels") +
  geom_text(aes(x = 1.7, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 


combined <- resort_Season_count_plot + city_Season_count_plot
combined + plot_layout(guides = "collect")



# --------------------------------------------------------------------------------------------------------------------


# Market Segment   (Draw a barchart showing % on y-axis, Pi chart not looking good)

# Resort
resort_MarketSegment_count <- resort_data %>% group_by(MarketSegment) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


resort_MarketSegment_count_plot <- ggplot(resort_MarketSegment_count, aes(MarketSegment, pct, fill = MarketSegment)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Market Segment for Resort", x = "Market Segment", y = "Percentage")


# City
city_MarketSegment_count <- city_data %>% group_by(MarketSegment) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


city_MarketSegment_count_plot <-  ggplot(city_MarketSegment_count, aes(MarketSegment, pct, fill = MarketSegment)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Market Segment for city", x = "Market Segment", y = "Percentage")

combined <- resort_MarketSegment_count_plot + city_MarketSegment_count_plot
combined + plot_layout(guides = "collect")


# --------------------------------------------------------------------------------------------------------------------


# Customer Type   (Draw a barchart showing % on y-axis, Pi chart not looking good)

# Resort
resort_CustomerType_count <- resort_data %>% group_by(CustomerType) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


resort_CustomerType_count_plot <- ggplot(resort_CustomerType_count, aes(CustomerType, pct, fill = CustomerType)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Customer Type for Resort", x = "Customer Type", y = "Percentage")


# City
city_CustomerType_count <- city_data %>% group_by(CustomerType) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


city_CustomerType_count_plot <-  ggplot(city_CustomerType_count, aes(CustomerType, pct, fill = CustomerType)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Customer Type for city", x = "Customer Type", y = "Percentage")

combined <- resort_CustomerType_count_plot + city_CustomerType_count_plot
combined + plot_layout(guides = "collect")


# --------------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------------------


# Meal   (Draw a barchart showing % on y-axis, Pi chart not looking good)

# Resort
resort_Meal_count <- resort_data %>% group_by(Meal) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))
resort_Meal_count$Meal <- factor(resort_Meal_count$Meal, levels=c("BB", "FB", "HB", "SC"), labels=c("Bed & Breakfast", "Full Board", "Half-Board","Not Known"))


resort_Meal_count_plot <- ggplot(resort_Meal_count, aes(Meal, pct, fill = Meal)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Meal for Resort", x = "Meal Type", y = "Percentage")


# City
city_Meal_count <- city_data %>% group_by(Meal) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))
city_Meal_count$Meal <- factor(city_Meal_count$Meal, levels=c("BB", "FB", "HB", "SC"), labels=c("Bed & Breakfast", "Full Board", "Half-Board","Not Known"))


city_Meal_count_plot <-  ggplot(city_Meal_count, aes(Meal, pct, fill = Meal)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Meal Type for City Hotels", x = "Meal Type", y = "Percentage")

combined <- resort_Meal_count_plot + city_Meal_count_plot
combined + plot_layout(guides = "collect")

# -----------------------------------------------------------------------------------------------------------------------------------
# Reservation Status
# Resort
resort_ReservationStatus_count <- resort_data %>% group_by(ReservationStatus) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


resort_ReservationStatus_count_plot <- ggplot(resort_ReservationStatus_count, aes(ReservationStatus, pct, fill = ReservationStatus)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Reservation Status for Resort", x = "Reservation Status", y = "Percentage")


# City
city_ReservationStatus_count <- city_data %>% group_by(ReservationStatus) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


city_ReservationStatus_count_plot <-  ggplot(city_ReservationStatus_count, aes(ReservationStatus, pct, fill = ReservationStatus)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Reservation Status for city", x = "Reservation Status", y = "Percentage")

combined <- resort_ReservationStatus_count_plot + city_ReservationStatus_count_plot
combined + plot_layout(guides = "collect")


############### -------------- Complex Plots -------------#################################


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
  scale_fill_brewer(palette = "Blues") +
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
  scale_fill_brewer(palette = "Blues") +
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
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Does booking earlier mean more Cancelation?",
       x = "Type of Hotel",
       y = "How many days before the booking was made?",
       fill = "Cancelation Rate") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13, face = "bold")) +
  scale_y_continuous(breaks = seq(0,600,40))


# Lead Time vs days in waiting list

# Resort
resort_lead_time_daysinwaiting <- ggplot(data = resort_data, aes(y = LeadTime, x = DaysInWaitingList)) +
  geom_point(color = "steelblue") +
  scale_y_continuous(breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = seq(0,200, 50)) +
  labs(title = "How early a booking is made vs Days in Waiting List for Resort",
       x = "Number of days people wait for Booking Confirmation",
       y = "Lead Time") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13, face = "bold"))

# City
city_lead_time_daysinwaiting <- ggplot(data = city_data, aes(y = LeadTime, x = DaysInWaitingList)) +
  geom_jitter(color = "steelblue") +
  scale_y_continuous(breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = seq(0,500, 50)) +
  labs(title = "How early a booking is made vs Days in Waiting List for City",
       x = "Number of days people wait for Booking Confirmation",
       y = "Lead Time") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13, face = "bold"))

grid.arrange(resort_lead_time_daysinwaiting, city_lead_time_daysinwaiting, nrow = 2)






########## --------------- Plot 2 ends -----------------#############



######## ------------- Plot 3 HotelType_TotalNumberOfDaysStayed_ADR and Count ------------ #############################



# Total number of days stayed 

HotelType_TotalNumberOfDaysStayed_ADR <- combined_data %>% group_by(HotelType, TotalNumberOfDaysStayed) %>% summarise(count = n(), mean_ADR = mean(ADR))
# View(HotelType_TotalNumberOfDaysStayed_ADR)

HotelType_TotalNumberOfDaysStayed_ADR_plot <-
  ggplot(
    data = HotelType_TotalNumberOfDaysStayed_ADR,
    mapping = aes(x = TotalNumberOfDaysStayed, y = count, color = HotelType)
  ) +
  geom_line(size = 1.3) +
  labs(title = "How namy number of days do people prefer to stay?",
       x = "Total number of days stayed",
       y = "Number of Visitors",
       fill = "HotelType") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(0,15,1)) + 
  scale_y_continuous(breaks = seq(0,20000,2500)) 

HotelType_TotalNumberOfDaysStayed_ADR_plot




## RESORT
HotelType_TotalNumberOfDaysStayed_ADR <- combined_data %>% group_by(HotelType, TotalNumberOfDaysStayed) %>% summarise(mean_ADR = mean(ADR))
#View(HotelType_TotalNumberOfDaysStayed_ADR)

HotelType_TotalNumberOfDaysStayed_ADR_plot <-
  ggplot(
    data = HotelType_TotalNumberOfDaysStayed_ADR,
    mapping = aes(x = TotalNumberOfDaysStayed, y = mean_ADR, color = HotelType)
  ) +
  geom_line(size = 1.3) +
  labs(title = "How does number of days stayed affect Price per night?",
       x = "Total number of days stayed",
       y = "Average Daily Rate (in Euros)",
       fill = "HotelType") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(0,15,1)) + 
  scale_y_continuous(breaks = seq(0,200,10)) 

HotelType_TotalNumberOfDaysStayed_ADR_plot


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
  scale_fill_brewer(palette = "Blues") +
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

# Resort
# Can filter, group_by and summarise in same command (AMAZING!!)
Requests_Adults_resort <- resort_data %>% filter(Adults<4) %>% group_by(AnySpecialRequest, Adults) %>% summarise(count = n(), mean_ADR = mean(ADR))


Requests_Adults_resort_plot <- ggplot(data = Requests_Adults_resort, aes(x = Adults, y = count, fill = AnySpecialRequest)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
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


# City
Requests_Adults_city <- city_data %>% filter(Adults<4) %>% group_by(AnySpecialRequest, Adults) %>% summarise(count = n(), mean_ADR = mean(ADR))

Requests_Adults_city_plot <- ggplot(data = Requests_Adults_city, aes(x = Adults, y = count, fill = AnySpecialRequest)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
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


combined <- Requests_Adults_resort_plot + Requests_Adults_city_plot & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# As the number of Adults increases, requests also increases.

################## ----------- ADR -------------- #################


ggplot(data = Requests_Adults, aes(x = Adults, y = mean_ADR, fill = AnySpecialRequest)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
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


#######################---------------- Plot 6 : Are people being Assigned rooms that they Reserve? ------------ ##################


resort_assigned_reserved <- ggplot(data = resort_data, aes(x = ReservedRoomType, y = AssignedRoomType)) +
  geom_jitter(shape = 21,
              size = 1,
              color = "steelblue") +
  labs(title = "Resort Hotels",
       x = "Reserved Room",
       y = "Assigned Room") +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )



city_assigned_reserved <- ggplot(data = city_data, aes(x = ReservedRoomType, y = AssignedRoomType)) +
  geom_jitter(shape = 21,
              size = 1,
              color = "steelblue") +
  labs(title = "Cities Hotels",
       x = "Reserved Room",
       y = "Assigned Room") +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )



combined <- resort_assigned_reserved + city_assigned_reserved & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")


############################---------------- Plot 6 ends-----------######################################


#####################
Season_ADR_combined <- combined_data %>% group_by(Season, HotelType) %>% summarise(mean_ADR = mean(ADR))

ggplot(
  data = (Season_ADR_combined) ,
  mapping = aes(x = Season, fill = HotelType, y = mean_ADR)
) +
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,150,25)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "Season wise Price per night(in Euros) for City and Resort Hotel",
       x = "Season",
       y = "Average Daily Rate (in Euros)",
       fill = "Hotel Type") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


###### # Month wise analysis  - 

######## ------- Cancelation Status ------------ ##########

# RESORT
ArrivalMonth_IsCanceled_resort <- resort_data %>% group_by(ArrivalMonth, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_resort, aes(x = ArrivalMonth, fill = IsCanceled, y = count)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Cancelation Status by Arrival Month for Resort hotels",
       x = "Arrival Month",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0, 3000, 300)) +
  scale_x_discrete(limits = month.name)

# For resorts, no major change is observed in cancelation ratio. 


# CITY
ArrivalMonth_IsCanceled_city <- city_data %>% group_by(ArrivalMonth, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_city, aes(x = ArrivalMonth, fill = IsCanceled, y = count)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Cancelation Status by Arrival Month for City hotels",
       x = "Arrival Month",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  scale_x_discrete(limits = month.name)

# For City Hotels, we see that Cancellations are much higher than non canceled hotels for April, May and June. 

####### ------------------------------------------- #############################

########### ------------------------- ADR -----------------###############


ArrivalMonth_IsCanceled_combined <- combined_data %>% group_by(ArrivalMonth, HotelType) %>% summarise(mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_combined, aes(x = ArrivalMonth, y = mean_ADR, fill = HotelType)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,200,25)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Average Daily Rate over different months",
       x = "Arrival Month",
       y = "Average Daily Rate (in Euros)",
       fill = "Hotel Type")

# see in June and July, resort ADR shoots up.

######################------------------------------#############################


############ ----------------  visitor types_ArrivalMonth ---------------- #############################


VisitorType_ArrivalMonth_combined <- combined_data %>% group_by(VisitorType, ArrivalMonth) %>% summarise(count = n())


ggplot(data = VisitorType_ArrivalMonth_combined, aes(x = ArrivalMonth, fill = VisitorType, y = count)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,8000,1000)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Number of Visitors over different months",
       x = "Arrival Month",
       y = "Number of Visitors",
       fill = "Visitor Type")

# Family visits - Increase in July and August. Very less in November.
# Solo travelers - MOre in september and October.



######################------------------------------#############################

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
  scale_fill_gradient(low = "steelblue", high="yellow") +
  labs(title="Number Of Reservations From Different Countries for cities?",
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
  scale_fill_gradient(low = "steelblue", high="yellow") +
  labs(title="Number Of Reservations From Different Countries for Resorts?",
       y="Latitude", x="Longitude") +
  theme_light() + coord_cartesian()
mp_resort
##################################################################################

# ----------------Business Question 1 : What season has the most cancellations, and how can we prevent other seasons from having less cancellations? 

# Resort
#using dplyr package, making a new dataframe that contains count 
Season_IsCancelled_resort <- resort_data %>% group_by(Season, IsCanceled) %>% summarise(count = n())

# Making the plot
Season_IsCancelled_resort_plot <-
  ggplot(
    data = (Season_IsCancelled_resort) ,
    mapping = aes(x = Season, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(breaks = seq(0,30000,2500)) +
  scale_fill_brewer(palette = "Reds") +
  theme_grey() +
  labs(title = "Resort",
       x = "Season",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 

# City
#using dplyr package, making a new dataframe that contains count 
Season_IsCancelled_city <- city_data %>% group_by(Season, IsCanceled) %>% summarise(count = n())

# Making the plot

Season_IsCancelled_city_plot <-
  ggplot(
    data = (Season_IsCancelled_city) ,
    mapping = aes(x = Season, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_brewer(palette = "Reds") +
  theme_grey() +
  labs(title = "City",
       x = "Season",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


# Arranging both plots in same figure
grid.arrange(Season_IsCancelled_resort_plot,
             Season_IsCancelled_city_plot,
             nrow = 2)


# For city, summer has more canceled hotel than not canceled hotel.
# In other seasons also, people tend to cancel more in city than resort


# ---------------------------------------------------------------------------------------------------------------

#-------- Business Question 2 - What visitor type has the most cancellations, and how can we prevent certain types from not canceling? 

EnsurePackage("ggplot2")
EnsurePackage("arules")
EnsurePackage("arulesViz")

# Resort
#using dplyr package, making a new dataframe that contains count 
VisitorType_IsCancelled_resort <- resort_data %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

# Making the plot
VisitorType_IsCancelled_resort_plot <-
  ggplot(
    data = (VisitorType_IsCancelled_resort) ,
    mapping = aes(x = VisitorType, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_y_continuous(breaks = seq(0,30000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  labs(title = "Resort",
       x = "VisitorType",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 

# City
#using dplyr package, making a new dataframe that contains count 
VisitorType_IsCancelled_city <- city_data %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

# Making the plot

VisitorType_IsCancelled_city_plot <-
  ggplot(
    data = (VisitorType_IsCancelled_city) ,
    mapping = aes(x = VisitorType, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  labs(title = "City",
       x = "VisitorType",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

# Arranging both plots in same figure
combined <- VisitorType_IsCancelled_resort_plot + VisitorType_IsCancelled_city_plot & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")
# Couples in Cities are canceling  more and that too in month of April, May and June (we saw in Month_wise_analysis.R) 

# Visitor Wise ADR


Visitor_ADR <- combined_data %>% group_by(VisitorType, HotelType) %>% summarise(mean_ADR = mean(ADR))

ggplot(
  data = (Visitor_ADR) ,
  mapping = aes(x = VisitorType, fill = HotelType, y = mean_ADR)
) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  scale_y_continuous(breaks = seq(0,200,50)) +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  labs(title = "Visitor Wise Price per night (in Euros)",
       x = "VisitorType",
       y = "Average Daily rate",
       fill = "Hotel Type") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 



#### Now we need to get some rules why couples are canceling in the city more?
### USE Association Rules Mining

city_couples <- city_data %>% dplyr::filter(VisitorType == "Couple")  # Using dplyr package, filtering rows for couples only.

# now, we create a dataframe containing only factor columns so that we can apply association rules mining. 
city_couples <- city_couples %>% select_if(is.factor)
str(city_couples)

# Remove column ReservationStatus
city_couples <- city_couples %>% dplyr::select(-c("ReservationStatus", "Country", "HotelType", "ArrivalYear", "Season", "ADRType", "ArrivalMonth",
                                                  "CustomerType", "DepositType", "VisitorType"))
# table(city_couples$IsCanceled[city_couples$DepositType == "Non Refund"])
str(city_couples)
# Need to convert our dataframe to a transaction matrix 
city_couples <- as(city_couples, "transactions")

# Inspecting the first transaction
inspect(city_couples[1])

# create a frequency plot
itemFrequencyPlot(city_couples, topN = 20, cex.names = 0.75)


rules <- apriori(data = city_couples, parameter = list(support = 0.05, confidence = 0.7, minlen = 4), appearance = list(rhs = c("IsCanceled=Canceled")))

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10))  # or inspect(sort(top.support)[1:10])



### Running regression on City - Couples data

city_couples_regression <- city_data %>% dplyr::filter(VisitorType == "Couple")  # Using dplyr package, filtering rows for couples only. 
city_couples_regression$LeadTime <- cut(city_couples_regression$LeadTime, breaks = c(-1,7,90,600), labels = c("Low lead time", "Optimum lead time", "High lead time"))
str(city_couples_regression)


lmOut_2 <- lm(formula = ADR ~ Meal + AssignedRoomType + DepositType + LeadTime + PreviousCancellations + ReservedRoomType
              , data = city_couples_regression)
summary(lmOut_2)


# --------------Business question 2 ends -----------------------------------------------------------------------------------------------------------------


#############################################################################################################################################
#------ BUSINESS QUESTION 3 --- What can we learn from repeated guests that we can apply to first time customers to ensure they return? Is this the same for the city 
# and resort properties? 

# Create a dataframe that only contains repeatedGuest.....
RepeatedGuests <- combined_data %>% filter(IsRepeatedGuest %in% "Repeated")
RepeatedGuests_city <- RepeatedGuests %>% filter(HotelType == "city")
RepeatedGuests_resort <- RepeatedGuests %>% filter(HotelType == "resort")

# Most of the repeated Guests are Business Travelers. 
table(RepeatedGuests$VisitorType)   

#########----------- Showing This as pie plot -----------############

# Creating a blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=13, face="bold"),
    legend.title=element_text(size=13, face="bold")
  )

RepeatedGuests_VisitorType_count <- RepeatedGuests %>% group_by(VisitorType) %>% summarise(count = n())

ggplot(data = RepeatedGuests_VisitorType_count, aes(x ="", y = count, fill = VisitorType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("Percentage of Repeated Guests by Visitors") +
  geom_text(aes(x = 1.8, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 

# ------------------------------------------------------------------------------------------------------------------------------
# Q1 - What hotel do repeated Guests prefer and Cancelation Status?

IsCanceled_HotelType_RepeatedGuest <- RepeatedGuests %>% group_by(HotelType, IsCanceled) %>% summarise(count = n())

ggplot(data = IsCanceled_HotelType_RepeatedGuest, aes(x = HotelType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Cancelation Rate Hotel Wise for Repeated Guests",
       x = "Type of Hotel",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,1500,150)) +
  geom_text(aes(label=count), color = "black", size = 4,position=position_dodge(width=0.8), vjust=-0.5)

##### If a guest is repeated, he is likely to cancel a City hotel than a resort hotel.


# ------------------------------------------------------------------------------------------------------------------------------
# Q2 - What type of visitors are repeated guests?

##### 2(i) Segregating by Hotel Type for Count and For mean(ADR) - 

HotelType_VisitorType_RepeatedGuest <- RepeatedGuests %>% group_by(VisitorType, HotelType) %>% summarise(count = n(), mean_ADR = mean(ADR))

# Cancelation Count
ggplot(data = HotelType_VisitorType_RepeatedGuest, aes(x = VisitorType, fill = HotelType, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "What type of visitors are repeated guests?",
       x = "Type of Visitor",
       y = "Count",
       fill = "Hotel Type") +
  scale_y_continuous(breaks = seq(0,1500,150)) +
  geom_text(aes(label=count), color = "black", size = 4,position=position_dodge(width=0.8), vjust=-0.5)

# For business travelers who repeat themselves, ratio of City to Resort is more since they tend to visit cities repeatedly and visit resorts mostly during company parties
# For couples, singles and Family, they tend to visit resorts repeatedly than cities. 


# ADR 
ggplot(data = HotelType_VisitorType_RepeatedGuest, aes(x = VisitorType, fill = HotelType, y = mean_ADR)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "How much ADR is generated for repeated Guests based on visitors?",
       x = "Type of Visitor",
       y = "Average Daily Rate (in Euros)",
       fill = "Hotel Type") +
  scale_y_continuous(breaks = seq(0,150,25))

# ------------------------------------------------------------------------------------------------------------------------------

#### 2 (ii)   - By Cancellation rate  ( For both resorts and Hotels)

# Resort 
IsCanceled_VisitorType_RepeatedGuest_resort <- RepeatedGuests_resort %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

IsCanceled_VisitorType_RepeatedGuest_resort_plot <- ggplot(data = IsCanceled_VisitorType_RepeatedGuest_resort, aes(x = VisitorType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Resort hotels",
       x = "Type of Visitor",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,550,50)) 
#geom_text(aes(label=count), color = "black", size = 4,position=position_dodge(width=0.8), vjust=-0.5)


# City
IsCanceled_VisitorType_RepeatedGuest_city <- RepeatedGuests_city %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

IsCanceled_VisitorType_RepeatedGuest_city_plot <- ggplot(data = IsCanceled_VisitorType_RepeatedGuest_city, aes(x = VisitorType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "City hotels",
       x = "Type of Visitor",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,1000,100)) 
#geom_text(aes(label=count), color = "black", size = 4,position=position_dodge(width=0.8), vjust=-0.5)

# Arrange both plots together 
grid.arrange(IsCanceled_VisitorType_RepeatedGuest_resort_plot, IsCanceled_VisitorType_RepeatedGuest_city_plot, nrow = 2)

# Same plot in a different way. ( Which one to show?)
combined <- IsCanceled_VisitorType_RepeatedGuest_city_plot + IsCanceled_VisitorType_RepeatedGuest_resort_plot & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# If the repeated guests are couples, they are more likely to cancel in city. 


# Repeated guests reserved which room and got which room?
ggplot(data = RepeatedGuests_city,aes(x= ReservedRoomType, y = AssignedRoomType)) + geom_jitter(shape = 21,size = 1, color = "steelblue") + ggtitle("Room Type : Reserved vs Assigned") + xlab("Reserved Room") + ylab("Assigned Room") + 
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))

# ------------------------------------------------------------------------------------------------------------------------------

## Linear MOdeling - 
# Predicting ADR of repeated guests when they are in family and in city is less

names <- c( "IsCanceled", "ReservationStatusDate", "ReservationStatus", "Country", "IsRepeatedGuest", "Agent", "Company", "RequiredCarParkingSpaces", 
            "TotalOfSpecialRequests", "TotalNumberOfDaysStayed", "AvgRevenuePerStay", "ADRType", "ArrivalYear", "HotelType", "ArrivalDate", "ArrivalMonth", "ADRType" )

city_data_family <- RepeatedGuests_city %>% dplyr::filter(VisitorType == "Family") %>% dplyr::select(-names)
str(city_data_family)

linearModel1 <- lm( ADR ~ Meal + ReservedRoomType + AssignedRoomType + DepositType + LeadTime + DepositType, data = city_data_family)
summary(linearModel1)


linearModel1 <- lm(ADR ~ ., data = city_data)
summary(linearModel1)


# ----------------------------------------------------------------------------------------------

### TRIED ASSOCIATION RULES BUT NO CONCLUSIVE RESULTS because only 3200 rows.
####---- ASSOCIATION RULES TO FIND GOOD RULES FOR REPEATED GUESTS.
## On what basis a guest is likely to repeat itself?


# Converting numeric columns to factors (which are logical to convert into factors)
names <- c("Adults", "Babies", "Children", "TotalNumberOfDaysStayed", "VisitorType")
RepeatedGuests[,names] <- lapply(RepeatedGuests[,names], factor)

# Keeping only factor columns for Association Rule Mining - 

RepeatedGuests_Factor <- RepeatedGuests %>% select_if(is.factor)

str(RepeatedGuests_Factor)

# Removing some columns since that will Bias our data. 
names <- c("Country", "IsCanceled", "ReservationStatus")
RepeatedGuests_Factor <- RepeatedGuests_Factor %>% select(-names)

rules <- apriori(data = RepeatedGuests_Factor, parameter = list(support = 0.005, confidence = 0.95, minlen = 3, maxlen = 5), appearance = list(rhs = c("IsRepeatedGuest=Repeated")))

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10))  # or inspect(sort(top.support)[1:10])

View(RepeatedGuests)
str(RepeatedGuests)


# Since repeated Guests are less, we can analyze some commonality between Non-Repeated Guests using SVM. We will train on resort data and city data separately.

targetIndex <- createDataPartition(resort_data$IsRepeatedGuest, p = 0.8, list = F) # Getting the index values to be used in creating the training data.

training_Repeated_resort <- dplyr::slice(resort_data, targetIndex) # Using slice function to get all rows for numbers that are contained in targetIndex vector.
testing_Repeated_resort <- dplyr::slice(resort_data, -targetIndex)

str(training_Repeated_resort)
trivial_cols_to_be_removed <- c("ReservationStatusDate", "ReservationStatus", "AvgRevenuePerStay", "ArrivalYear", "ArrivalMonth", "HotelType", "Country", "")

svmOutput <- svm( IsRepeatedGuest ~ ., data = cred, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)  


### ----------- Business question 3 ends ---------------------- $##########################################


#-------------------------  Couples only City hotels dataset ---------------------------------
str(city_data) 

# City and only couples
couples_city <- city_data %>% filter(VisitorType == "Couple")

table(couples_city$ReservedRoomType)
table(couples_city$AssignedRoomType)


# Removing some room types since very less rows
couples_city <- couples_city[which(couples_city$ReservedRoomType %in% c("A", "B", "D", "E", "F")),]
couples_city <- couples_city[which(couples_city$AssignedRoomType %in% c("A", "B", "D", "E", "F")),]


# Q1. What type of rooms are assigned and getting canceled by couples in cities?

IsCanceled_AssignedRoomType_Couples_City <- couples_city %>% group_by(IsCanceled, AssignedRoomType) %>% summarise(count = n())


ggplot(
  data = (IsCanceled_AssignedRoomType_Couples_City) ,
  mapping = aes(x = AssignedRoomType, fill = IsCanceled, y = count)
) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6)  +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  labs(title = "What type of rooms are Assigned to Couples in City hotels?",
       x = "Asssigned Room Type",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
#geom_text(aes(label=count), color = "black", size = 3,position=position_dodge(width=0.8), vjust=-0.5)


# So, if AssignedRoomType is A, then cancellation ratio is much higher as compared to other rooms.  


# Q2 Analyzing mean(ADR) with ReservedRoomtype, we see that in cities, if couples tend to reserve room D, E and F, ADR is much higher than other rooms. --- #####



ggplot(data = couples_city, aes(x = ReservedRoomType, y = ADR, fill = ReservedRoomType)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "ADR according to Reserved Room Type for Couples in City hotels",
       x = "Reserved Room type",
       y = "Average Daily Rate (in Euros)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(0,400,50)) +
  theme(legend.position = "none")


# Couples canceling in city
#### -----------CLASSIFYING THROUGH SVM MODEL
couples_city <- city_data %>% filter(VisitorType == "Couple")

# Removing columns
names <- c("Country", "ReservationStatusDate", "Agent", "Company", "ArrivalDate", "ArrivalMonth", "ArrivalYear", "AvgRevenuePerStay", "HotelType", 
           "RequiredCarParkingSpaces", "TotalOfSpecialRequests", "ReservationStatus","IsRepeatedGuest")

# Filtering data
couples_city_svm <- couples_city %>% dplyr::select(-names)

# Create training and test data 
targetIndex <- createDataPartition(couples_city_svm$IsCanceled, p = 0.8, list = F) # Getting the index values to be used in creating the training data.

target_col <- c("IsCanceled")
couples_city_svm <- one_hot(as.data.table(couples_city_svm %>% dplyr::select(-target_col)))
IsCanceled <- couples_city$IsCanceled
couples_city_svm <- data.frame(couples_city_svm, IsCanceled)

training_couples_city_svm <- dplyr::slice(couples_city_svm, targetIndex) # Using slice function to get all rows for numbers that are contained in targetIndex vector.
testing_couples_city_svm <- dplyr::slice(couples_city_svm, -targetIndex)

# Performing SVM
svmOutput <- ksvm( IsCanceled ~ ., data = training_couples_city_svm, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)  
svmOutput

svmPred <- predict(svmOutput,testing_couples_city_svm)
View(svmPred)

comptable <- data.frame(svmPred,testing_couples_city_svm$IsCanceled)
table(comptable)

confusionmatrixIsRepeatedGuest <- confusionMatrix(svmPred,testing_couples_city_svm$IsCanceled)
confusionmatrixIsRepeatedGuest


#------------------------ SVM Model starts ---------------------------# 


# installing and loading the required package in R
# enables usage of SVM in R 
install.packages("kernlab")
library(kernlab)
install.packages("caret")
library(caret)

# Viewing the dataset
str(combined_data)


# Creating two datasets - one for training and other for testing

trainList <- createDataPartition(y=combined_data$IsCanceled,p=.60,list=FALSE)
# Here we are creating a partition on the combined_data dataset,
# The trainlist contains 69113 indices of the randomly selected rows from the dataset.
trainSet <- combined_data[trainList,]
# selecting all the columns by the row indices of trainList and storing the output in trainSet.
testSet <- combined_data[-trainList,]
# putting rest of the rows (which are not in train Set) in test Set


# making sure that the dataset contains the required columns and rows in both the training set and test set.
dim(trainSet)           # Output : 69113    38
dim(testSet)            # Output : 46075    38


# Building SVM Models : 
# We started by building the model, including all the possible numeric variables in the first 1st model.
# Then, we started filtering out the parameters to examine what were the top variables impacting the cancellation rate
# We also experimented with some individual dependent variables like lead time to test how the model behaves.



# Building Model 1 using using all the numeric variables - 

svmModel1 <- ksvm(IsCanceled ~ LeadTime + DaysInWaitingList + ReservationStatusDate +
                    StaysInWeekendNights + StaysInWeekNights + PreviousCancellations +
                    PreviousBookingsNotCanceled + BookingChanges + DaysInWaitingList + ADR +
                    RequiredCarParkingSpaces + TotalOfSpecialRequests + TotalNumberOfDaysStayed +
                    AvgRevenuePerStay,  data=trainSet,
                  kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)
# The first argument, "IsCanceled", specifies the model we want to test.
# Using the word "IsCanceled" in this expression means that we want to have the "IsCanceled" variable 
# (i.e., whether the message is canceled or not canceled) as the outcome variable that our model predicts.
# tilde character ("~") in an R expression simply separates the left hand side of the expression from 
# the right hand side
# we are using all the numeric variables for predicting the cancellation in this model.

# The "data" parameter lets us specify which training set to use in the analysis

# The C argument refers to "cost of constraints. If we specify a large value of C we 
# may possibly get fewer mistakes, 
# On the other hand if we have a low value of C we will get a generalizable model, but one that makes 
# more classification mistakes.

# Cross refers to the cross validation model that the algorithm uses. "prob.model=TRUE," dictates that we
# use three-fold cross validation

#analyzing our model - 
svmModel1               # printing our ksvm model 
# As we can verify that the cross validation error is approximately near to 0.21 by looking at the output value.


svmPrediction1 <- predict(svmModel1, testSet)
# this command uses our model output from before, namely svmModel1, as the parameters for prediction. 
# It uses the "testSata," which the support vectors have never seen before, to generate predictions, 
# The output from the predict() command is a two dimensional list.


# creating confusion matrix using our table function
confusionMatrix(svmPrediction1,testSet$IsCanceled)
# Output Accuracy of the Model: 0.7833

# However this model contains unnecessary parameters to calculate the cancellation variable.
# For example, it does not make sense to keep data as input/independent variable. So, we need to remove few variables
# and build new models.




# Building Model 2 for cancellation using Lead Time and DaysInWaitingList - 
# We thought we would also check how the model behaves with only input variables
svmModel2 <- ksvm(IsCanceled ~ LeadTime + DaysInWaitingList, data=trainSet,
                  kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)

#analyzing our model - 
svmModel2           # validation error and training error = 0.31

svmPrediction2 <- predict(svmModel2, testSet)
# predicting the cancellation variable on the basis of the new model


# creating confusion matrix using
confusionMatrix(svmPrediction2,testSet$IsCanceled)  
# Model Accuracy : 0.6812
# We can see that only use these predictors decreased the accuracy of the model.
# Let's take into consideration other parameters from our earlier analysis to increase the accuracy of the model




# Building Model 3 using LeadTime, DaysInWaitingList, BookingChanges, PreviousCancellations, 
#               PreviousBookingsNotCanceled, TotalOfSpecialRequests

svmModel3 <- ksvm(IsCanceled ~ LeadTime + DaysInWaitingList +
                    PreviousCancellations + PreviousBookingsNotCanceled + BookingChanges + 
                    TotalOfSpecialRequests,  data=trainSet,
                  kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)
svmModel3          # analyzing the model

# predicting the cancellation on the test data set
svmPrediction3 <- predict(svmModel3, testSet)

confusionMatrix(svmPrediction3,testSet$IsCanceled) # Model Accuracy : 0.74%

# We have taken this model as the final predictor for cancellation. The top 5 attributes contributing towards cancellation.
# The top 5 independent variables are as follows - 
# 1. Lead Time: directly proportional to the probability of cancellation.  The higher the lead time, 
#               the customer is more likely to cancel.
# 2. Days in Waiting List are also impacting the cancellation - the higher the waiting period, the more likely 
#               they are to cancel and find better alternatives.
# 3. Previous Cancellations/Previous Bookings not cancelled: is obviously a key factor in determining their 
#               future moves of our customers.
# 4. Booking Changes: if they are making too many booking changes, it likely that they not sure of their choices 
#               and we just cant rely on them
# 5. Total number of special requests - also impacts the cancellations because if customers with a list of special
#               requests  about certain amenities and services at the hotel go unfulfilled, they are much likely to cancel.



# We also built two more models. One specific to city and other specific to resort.

# Filtering the City Hotel Data
cityHotelData <- subset(combined_data, HotelType == "city")
dim(cityHotelData)

# Filtering the Resort Hotel Data
resortHotelData <- subset(combined_data, HotelType == "resort")
dim(resortHotelData)

# Creating the training and test data set for City Hotel
trainListCity <- createDataPartition(y=cityHotelData$IsCanceled,p=.60,list=FALSE)
trainSetCity <- cityHotelData[trainListCity,]
testSetCity <- cityHotelData[-trainListCity,]
dim(trainSetCity)
dim(testSetCity)

# Creating the training and test data set for Resort Hotel
trainListResort <- createDataPartition(y=resortHotelData$IsCanceled,p=.60,list=FALSE)
trainSetResort <- resortHotelData[trainListResort,]
testSetResort <- resortHotelData[-trainListResort,]
dim(trainSetResort)
dim(testSetResort)



# Model 4 - On City Hotel Data

svmModel4 <- ksvm(IsCanceled ~ LeadTime + DaysInWaitingList +
                    PreviousCancellations + PreviousBookingsNotCanceled + BookingChanges + 
                    TotalOfSpecialRequests,  data=trainSetCity,
                  kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)
svmModel4         # analyzing the model
# training and cross validation error = 0.25

# predicting the cancellation on the test data set
svmPrediction4 <- predict(svmModel4, testSetCity)

confusionMatrix(svmPrediction4,testSetCity$IsCanceled) # Model Accuracy : 75% for City Hotels



# Model 5 - On Resort Hotel Data 

svmModel5 <- ksvm(IsCanceled ~ LeadTime + DaysInWaitingList +
                    PreviousCancellations + PreviousBookingsNotCanceled + BookingChanges + 
                    TotalOfSpecialRequests,  data=trainSetResort,
                  kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)
svmModel5         # analyzing the model
# training and cross validation error = 0.25

# predicting the cancellation on the test data set
svmPrediction5 <- predict(svmModel5, testSetResort)

confusionMatrix(svmPrediction5,testSetResort$IsCanceled) # Model Accuracy : 74%

# So, we can conclude that our svm model is 74% efficient overall because - 
# when we ran the model on city data, the accuracy came out to be 75%
# when we ran the model on resort data, the accuracy came out to be 74%
# So, it's almost similar. And we can confirm the overall accuracy to be 74%


# --------------------------------- SVM Model ends ------------------------------ #









#  -------------------------------------- Linear Modeling--------------------------- ###

EnsurePackage("caret")
corMatrix <- cor(resort_data_linear_regression %>% select_if(is.numeric))
highlyCor <- findCorrelation(corMatrix, cutoff = 0.5)
print(highlyCor)

# Select numerical columns.
# Check for outliers and remove them. Linear regression assumes normality of data. 
# find out which columns are correlated. Remove correlated columns. 
# Start by choosing single independent variable. 

############ ------------------  RESORT -------------- ####################

str(resort_data)
resort_data_linear_regression <- resort_data
str(resort_data_linear_regression)
# Removing repetitive columns and not required columns such as Date, country which can bias the data.
names <-
  c(
    "IsCanceled",
    "ReservationStatusDate",
    "ReservationStatus",
    "Country",
    "IsRepeatedGuest",
    "Agent",
    "Company",
    "RequiredCarParkingSpaces",
    "TotalOfSpecialRequests",
    "TotalNumberOfDaysStayed",
    "AvgRevenuePerStay",
    "ADRType",
    "ArrivalYear",
    "HotelType",
    "ArrivalDate",
    "ArrivalMonth",
    "ADRType"
  )
resort_data_linear_regression <- resort_data_linear_regression %>% select(-names)

# Categorizing lead time
resort_data_linear_regression$LeadTime <- cut(resort_data_linear_regression$LeadTime, breaks = c(-1,7,90,600), labels = c("Low lead time", "Med lead time", "High lead time"))


#############-------------------- CITY  ------------ ######################

str(city_data)
city_data_linear_regression <- city_data
str(city_data_linear_regression)
# Removing repetitive columns and not required columns such as Date, country which can bias the data.
names <-
  c(
    "IsCanceled",
    "ReservationStatusDate",
    "ReservationStatus",
    "Country",
    "IsRepeatedGuest",
    "Agent",
    "Company",
    "RequiredCarParkingSpaces",
    "TotalOfSpecialRequests",
    "TotalNumberOfDaysStayed",
    "AvgRevenuePerStay",
    "ADRType",
    "ArrivalYear",
    "HotelType",
    "ArrivalDate",
    "ArrivalMonth",
    "ADRType"
  )
city_data_linear_regression <- city_data_linear_regression %>% select(-names)


# Categorizing lead time
city_data_linear_regression$LeadTime <- cut(city_data_linear_regression$LeadTime, breaks = c(-1,7,90,600), labels = c("Low lead time", "Med lead time", "High lead time"))



############ ------------- Start Modeling ----------------- ##############

# Seeing one by one. 


# 1. LeadTime
lmOut_LeadTime <- lm(formula = ADR ~ LeadTime, data = resort_data_linear_regression)
summary(lmOut_LeadTime)
# Significant but no variance in ADR based on lead time.
# Medium lead time(1-3 months) increases ADR value more than High Lead Time (more than 3 months.)

#2. Adults
lmOut_Adults <- lm(formula = ADR ~ Adults, data = resort_data_linear_regression)
summary(lmOut_Adults)
# Significant. If Adult is increased by 1, ADR goes up by 49.77$.
# Adults Explains about 13% of the variability in ADR.


#3. Meal
lmOut_Meal <- lm(formula = ADR ~ Meal, data = resort_data_linear_regression)
summary(lmOut_Meal)
# HB, FB, BB are significant. If visitors choose BB, we can give discounts for upgrading them tp HB since that increases ADR by 32$.
# But meal explains only 4% variability in ADR.


#4. AssignedRoomType
lmOut_AssignedRoomType <- lm(formula = ADR ~ AssignedRoomType, data = resort_data_linear_regression)
summary(lmOut_AssignedRoomType)
# If Assigned room type is A, upgrading it to C,F, G and H will be a better idea. Assigning room L and I will be a bad idea. 


#5. ReservedRoomType
lmOut_ReservedRoomType <- lm(formula = ADR ~ ReservedRoomType, data = resort_data_linear_regression)
summary(lmOut_ReservedRoomType)
# 20% variability is good. If reserved room type is A, upgrade it to G and H will boost ADR a lot. 

#6. DepositType
lmOut_DepositType <- lm(formula = ADR ~ DepositType, data = resort_data_linear_regression)
summary(lmOut_DepositType)
# Taking a deposit would be a bad idea. If people tend to book without giving any deposit, that would be a better idea.
# Not much variability.


#7. Babies 
lmOut_Babies <- lm(formula = ADR ~ Babies, data = resort_data_linear_regression)
summary(lmOut_Babies)


#8. Children 
lmOut_Children <- lm(formula = ADR ~ Children, data = resort_data_linear_regression)
summary(lmOut_Children)



#9. AssignedRoomType and ReservedRoomType
lmOut_AssignedRoomType_ReservedRoomType <- lm(formula = ADR ~ AssignedRoomType + ReservedRoomType, data = resort_data_linear_regression)
summary(lmOut_AssignedRoomType_ReservedRoomType)


#10. Based on number of people - 
lmOut_Adults_Children_Babies <- lm(formula = ADR ~ Adults + Children + Babies, data = resort_data_linear_regression)
summary(lmOut_Adults_Children_Babies)



# Combined: 

lmOut_1 <- lm(formula = ADR ~ . , data = resort_data_linear_regression)
summary(lmOut_1)




lmOut_2 <- lm(formula = ADR ~ Meal + AssignedRoomType + ReservedRoomType + LeadTime + PreviousCancellations + BookingChanges + 
                DaysInWaitingList + ParkingSpaceNeeded + AnySpecialRequest + Season
              + StaysInWeekNights + StaysInWeekendNights + Adults + Children  , data = resort_data_linear_regression)
summary(lmOut_2)


############ --------------------------- Linear Modeling ends ---------------- ########################




