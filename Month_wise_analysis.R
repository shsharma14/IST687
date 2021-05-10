# Month wise analysis  - 

######## ------- Cancelation Status ------------ ##########

ArrivalMonth_IsCanceled_resort <- resort_data %>% group_by(ArrivalMonth, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_resort, aes(x = ArrivalMonth, fill = IsCanceled, y = count)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
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


ArrivalMonth_IsCanceled_city <- city_data %>% group_by(ArrivalMonth, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_city, aes(x = ArrivalMonth, fill = IsCanceled, y = count)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
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

####### ------------------------------------------- #############################


########### ------------------------- ADR -----------------###############


ArrivalMonth_IsCanceled_combined <- combined_data %>% group_by(ArrivalMonth, HotelType) %>% summarise(mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_combined, aes(x = ArrivalMonth, y = mean_ADR, fill = HotelType)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
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



####################------------------------------ Lead time --------------------- ##########################


ggplot(data = combined_data, aes(x = ArrivalMonth, group = ArrivalMonth, y = LeadTime)) +
  geom_boxplot() + 
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,600,40)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Month wise Analysis of how many days earlier people book hotel?",
       x = "Arrival Month",
       y = "How many days earlier people book hotel?")
  


# Lead time is more in May, June. July (Summer Months) as people start planning for vacations beforehand. 


######################------------------------------#############################






############ ----------------  visitor types ---------------- #############################


VisitorType_ArrivalMonth_combined <- combined_data %>% group_by(VisitorType, ArrivalMonth) %>% summarise(count = n())


ggplot(data = VisitorType_ArrivalMonth_combined, aes(x = ArrivalMonth, fill = VisitorType, y = count)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
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



