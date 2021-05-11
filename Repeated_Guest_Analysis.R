# What can we learn from repeated guests that we can apply to first time customers to ensure they return? Is this the same for the city and resort properties? 




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
  labs(title = "How much ADR is generated based on visitors?",
       x = "Type of Visitor",
       y = "Average Daily Rate (in Euros)",
       fill = "Hotel Type") +
  scale_y_continuous(breaks = seq(0,150,25))




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


# ----------------------------------------------------------------------------------------------

### TRIED ASSOCIATION RULES BUT NO CONCLUSIVE RESULTS.
####---- ASSOCIATION RULES TO FIND GOOD RULES FOR REPEATED GUESTS.
## On what basis a guest is likely to repeat itself?

View(RepeatedGuests)
str(RepeatedGuests)



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

  
  
  
  



