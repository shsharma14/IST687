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
  scale_fill_brewer(palette = "Reds") +
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
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Cancelation Rate Hotel Wise for Repeated Guests",
       x = "Type of Hotel",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,1500,150))

# If a guest is repeated, he is likely to cancel a City hotel than a resort hotel.


# Q2 - What type of visitors are repeated guests?


##### 2(i) Segregating by Hotel Type - 

HotelType_VisitorType_RepeatedGuest <- RepeatedGuests %>% group_by(VisitorType, HotelType) %>% summarise(count = n())

ggplot(data = HotelType_VisitorType_RepeatedGuest, aes(x = VisitorType, fill = HotelType, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "What type of visitors are repeated guests?",
       x = "Type of Visitor",
       y = "Count",
       fill = "Hotel Type") +
  scale_y_continuous(breaks = seq(0,1500,150))


#### 2 (ii)   - By Cancellation rate  ( For both resorts and Hotels)

# Resort 
IsCanceled_VisitorType_RepeatedGuest_resort <- RepeatedGuests_resort %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

IsCanceled_VisitorType_RepeatedGuest_resort_plot <- ggplot(data = IsCanceled_VisitorType_RepeatedGuest_resort, aes(x = VisitorType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Resort hotels",
       x = "Type of Visitor",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,500,50))


# City
IsCanceled_VisitorType_RepeatedGuest_city <- RepeatedGuests_city %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

IsCanceled_VisitorType_RepeatedGuest_city_plot <- ggplot(data = IsCanceled_VisitorType_RepeatedGuest_city, aes(x = VisitorType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "City hotels",
       x = "Type of Visitor",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,1000,100))


# Arrange both plots together 
grid.arrange(IsCanceled_VisitorType_RepeatedGuest_resort_plot, IsCanceled_VisitorType_RepeatedGuest_city_plot, nrow = 2)

combined <- IsCanceled_VisitorType_RepeatedGuest_city_plot + IsCanceled_VisitorType_RepeatedGuest_resort_plot & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# If the repeated guests are couples, they are more likely to cancel in city. 










