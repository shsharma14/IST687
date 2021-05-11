# Couples only dataset.
str(city_data) 

# City and only couples
couples_city <- city_data %>% filter(VisitorType == "Couple")

table(couples_city$ReservedRoomType)
table(couples_city$AssignedRoomType)
table(couples_city)






# Removing some room types since very less rows
couples_city <- couples_city[which(couples_city$ReservedRoomType %in% c("A", "B", "D", "E", "F")),]
couples_city <- couples_city[which(couples_city$AssignedRoomType %in% c("A", "B", "D", "E", "F")),]

# 
# 
# 
# Q1. What type of rooms are assigned and getting canceled by couples in cities?

IsCanceled_AssignedRoomType_Couples_City <- couples_city %>% group_by(IsCanceled, AssignedRoomType) %>% summarise(count = n())


ggplot(
  data = (IsCanceled_AssignedRoomType_Couples_City) ,
  mapping = aes(x = AssignedRoomType, fill = IsCanceled, y = count)
) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6)  +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "What type of rooms are Assigned to Couples in City hotels?",
       x = "Asssigned Room Type",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=count), color = "black", size = 3,position=position_dodge(width=0.8), vjust=-0.5)


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






