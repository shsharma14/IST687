# Month wise analysis  - 

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


#### --- 
# # Now, we can analyze which visitor type is canceling in month of April, May and June for City hotels.
# 
# ArrivalMonth_VisitorType_IsCanceled <- city_data %>% group_by(ArrivalMonth, VisitorType, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))
# 
# ArrivalMonth_VisitorType_IsCanceled_Apr_May_June <- ArrivalMonth_VisitorType_IsCanceled %>% filter(ArrivalMonth %in% c("April", "May", "June"))
# 
# # Plot the cancellations by visitor Type
# 
# ggplot(data = ArrivalMonth_VisitorType_IsCanceled_Apr_May_June, aes(x = ArrivalMonth, fill = VisitorType, y = count)) +
#   geom_bar(stat = "identity",
#            position = position_dodge(0.75),
#            width = 0.6) +
#   theme_bw() +
#   scale_fill_brewer(palette = "Blues") +
#   theme(
#     axis.title.x = element_text(size = 15),
#     axis.title.y = element_text(size = 15),
#     legend.title = element_text(size = 15),
#     axis.text.x = element_text(angle = 90, face = "bold")
#   ) +
#   labs(title = "Cancelation Status by Arrival Month for City hotels",
#        x = "Arrival Month",
#        y = "Count",
#        fill = "Cancelation Status") +
#   scale_y_continuous(breaks = seq(0, 6000, 500)) 



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
















####################------------------------------ Lead time (City vs Resort) --------------------- ##########################

# RESORT
ggplot(data = resort_data, aes(x = ArrivalMonth, group = ArrivalMonth, y = LeadTime, fill = ArrivalMonth)) +
  geom_boxplot(outlier.shape = NA, fill = "steelblue") + 
  theme_bw() +
  #scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,600,50)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Month wise Analysis of how many days earlier people book hotel?",
       x = "Arrival Month",
       y = "How many days earlier people book hotel?") +
  theme(legend.position = "None")
  


# CITY
ggplot(data = city_data, aes(x = ArrivalMonth, group = ArrivalMonth, y = LeadTime, fill = ArrivalMonth)) +
  geom_boxplot(outlier.shape = NA, fill = "steelblue") + 
  theme_bw() +
  #scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,600,50)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Month wise Analysis of how many days earlier people book hotel?",
       x = "Arrival Month",
       y = "How many days earlier people book hotel?") +
  theme(legend.position = "None")


######################------------------------------#############################




















############ ----------------  visitor types ---------------- #############################


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




















