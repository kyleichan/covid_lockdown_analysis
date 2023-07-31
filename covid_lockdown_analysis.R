library(datetime)
library(foreign)
library(stats)
library(ggplot2)
library(tidyverse)
library(jsonlite)
library(readxl)
library(lubridate)
library(drc)
library(zoo)
library(rddtools)
library(reshape2)

#---------------------------------------------------------------------------------------------------
# Apple Data
#---------------------------------------------------------------------------------------------------

apple_data = read.csv("./Original Data/applemobilitytrends-2020-04-26.csv")
names(apple_data) <- gsub("X", "", names(apple_data)) # Remove Xs in column names
names(apple_data) <- gsub("\\.", "-", names(apple_data)) # Replace periods with dashes

transport_mode <- "transit" # Pick transport mode to focus on: driving, walking, transit
apple_data <- apple_data %>%
  filter(transportation_type == transport_mode)

apple_data <- apple_data %>%
  dplyr::select(-c(geo_type, transportation_type)) # Drop location type and transportation type

apple_data <- melt(apple_data) # Recast data in "tall" format
names(apple_data) <- c("city", "date", "y")
apple_data$date <- as.Date(apple_data$date)

city_list <- c(
  "Atlanta",
  "Boston",
  "Chicago",
  "Houston",
  "Los Angeles",
  "New York City",
  "Philadelphia",
  "San Francisco",
  "Seattle",
  "Washington DC"
)

apple_data_selected <- apple_data %>%
  filter(city %in% city_list)

windows()
apple_data_selected %>%
  ggplot( aes(x=date, y=y, group=city, color=city)) +
  geom_line(size=1) +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(size = 0.5), plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(limits=c(0,130), breaks = seq(0, 130, 10)) +
  labs(title = "Apple Maps Transit") +
  geom_hline(yintercept=100)

#---------------------------------------------------------------------------------------------------
# OpenTable Data
#---------------------------------------------------------------------------------------------------

opentable_data = read.csv("./Original Data/OpenTable_data.csv")

opentable_data <- opentable_data %>%
  filter(Type == "city") %>%
  dplyr::select(-1)

opentable_data <- melt(opentable_data)
names(opentable_data) <- c("city", "date", "y")
opentable_data$date <- gsub("X", "", opentable_data$date) # Remove Xs in column names
opentable_data$date <- gsub("\\.", "-", opentable_data$date) # Replace periods with dashes
opentable_data$date <- paste0("2020-", opentable_data$date)
opentable_data$date <- as.Date(opentable_data$date)

city_list_opentable <- c(
  "Atlanta",
  "Boston",
  "Chicago",
  "Houston",
  "Los Angeles",
  "New York",
  "Philadelphia",
  "San Francisco",
  "Seattle",
  "Washington"
  )

opentable_data_selected <- opentable_data %>%
  filter(city %in% city_list_opentable)

# Plot: OpenTable data for selected cities
windows()
opentable_data_selected %>%
  ggplot( aes(x=date, y=y, group=city, color=city)) +
  geom_line(size=1) +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(size = 0.5), plot.title = element_text(hjust = 0.5)) +
  scale_x_date(limits = c(as.Date("2020-03-01"), as.Date("2020-03-20")), date_breaks = "1 day", date_labels = "%m/%d") +
  scale_y_continuous(limits=c(-100,60), breaks = seq(-100, 60, 10)) +
  labs(title = "OpenTable: 10 US Cities") +
  geom_hline(yintercept=0)


# Plot: OpenTable data for all cities
windows()
opentable_data %>%
  ggplot( aes(x=date, y=y, group=city, color=city)) +
  geom_line(size=1) +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(size = 0.5), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_y_continuous(limits=c(-100,60), breaks = seq(-100, 60, 10)) +
  labs(title = "OpenTable: All Cities") +
  geom_hline(yintercept=0)


# For a given threshold, find the first date for each city that y dipped below the threshold
cutpoint <- -25 # Set the threshold

filtered <- opentable_data_selected %>%
  filter(y <= cutpoint) %>%
  group_by(city) %>%
  slice(1)


# Read in csv data for lockdown timeline events: date of first covid case, date of state of emergency, etc.
events = read.csv("./covid_events.csv")
events <- events %>%
  rename(city = 1) %>% # Rename first column as "city"
  mutate(turning_point = filtered$date) %>%
  mutate(emergency_state = as.Date(emergency_state)) %>%
  mutate(emergency_city = as.Date(emergency_city))

events2 <- events %>%
  dplyr::select(c(city, turning_point, emergency_state, emergency_city))

events2 <- melt(events2, id.vars = "city")

windows()
events2 %>%
  ggplot(aes(x=value, y=city, group=variable, color=variable)) +
  scale_y_discrete(limits = rev(filtered$city)) +
  geom_point(size = 5) +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(size = 0.5), plot.title = element_text(hjust = 0.5)) +
  scale_x_date(limits = c(as.Date("2020-03-01"), as.Date("2020-03-20")), date_breaks = "1 day", date_labels = "%m/%d") +
  labs(title = paste0("OpenTable Drops More Than ", cutpoint, "%"))


windows()
opentable_data_selected %>%
  ggplot(aes(x = date)) + geom_line(aes(y = y, color = city)) + 
  facet_grid(city ~ ., scales = "free_y") + theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  theme_classic(base_size = 14)


events$first_case_national

opentable_data_selected %>%
  group_by(city) %>%
  filter(date == as.Date(events$first_case_national))



#---------------------------------------------------------------------------------------------------
# Old code
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------



# Function for extracting a single row of time series data for a specific region and mode of transport
get_time_series <- function(data, region_name, transport_name) {
  region_data <- apple_data %>%
    filter(region==region_name)
  
  region_transport_data <- region_data %>%
    filter(transportation_type==transport_name) %>%
    dplyr::select(-c(1:3)) %>% # Remove first few non-date columns 
    gather() %>% # Transpose data
    rename(date=key, !!region_name :=value) # To get the variable as a name, we use !! and :=
  
  region_transport_data$date <- as.Date(region_transport_data$date)
  
  return(region_transport_data)
}

apple_data = read.csv("./Original Data/applemobilitytrends-2020-04-26.csv")
names(apple_data) <- gsub("X", "", names(apple_data)) # Remove Xs in column names
names(apple_data) <- gsub("\\.", "-", names(apple_data)) # Replace periods with dashes

region_name1 <- "Atlanta"
region_name2 <- "Boston"
region_name3 <- "Chicago"
region_name4 <- "Houston"
region_name5 <- "Los Angeles"
region_name6 <- "New York City"
region_name7 <- "Philadelphia"
region_name8 <- "San Francisco - Bay Area"
region_name9 <- "Seattle"
region_name10 <- "Washington DC"
transport_name <- "transit"

region1 <- get_time_series(data=apple_data, region_name=region_name1, transport_name=transport_name)
region2 <- get_time_series(data=apple_data, region_name=region_name2, transport_name=transport_name)
region3 <- get_time_series(data=apple_data, region_name=region_name3, transport_name=transport_name)
region4 <- get_time_series(data=apple_data, region_name=region_name4, transport_name=transport_name)
region5 <- get_time_series(data=apple_data, region_name=region_name5, transport_name=transport_name)
region6 <- get_time_series(data=apple_data, region_name=region_name6, transport_name=transport_name)
region7 <- get_time_series(data=apple_data, region_name=region_name7, transport_name=transport_name)
region8 <- get_time_series(data=apple_data, region_name=region_name8, transport_name=transport_name)
region9 <- get_time_series(data=apple_data, region_name=region_name9, transport_name=transport_name)
region10 <- get_time_series(data=apple_data, region_name=region_name10, transport_name=transport_name)

combined <- region1 %>%
  left_join(region2) %>%
  left_join(region3) %>%
  left_join(region4) %>%
  left_join(region5) %>%
  left_join(region6) %>%
  left_join(region7) %>%
  left_join(region8) %>%
  left_join(region9) %>%
  left_join(region10)

windows()
ggplot(combined, aes(x=date)) +
  geom_line(aes(y=Atlanta, color="darkred")) +
  geom_line(aes(y=Boston, color="steelblue")) +
  geom_line(aes(y=Chicago, color="gray70")) +
  theme_classic() +
  scale_color_discrete(name = "Location", labels = c(region_name1, region_name2, region_name3))


# Rolling averages
combined <- combined %>%
  mutate(location1_avg = rollmean(Stockholm, k=7, fill=NA)) %>%
  mutate(location1_avg2 = rollmean(Stockholm, k=14, fill=NA))

windows()
ggplot(combined, aes(x=date)) +
  geom_line(aes(y=Stockholm, color="darkred")) +
  geom_line(aes(y=location1_avg, color="steelblue")) +
  geom_line(aes(y=location1_avg2, color="gray70")) +
  theme_classic() +
  scale_color_discrete(name = "Location", labels = c(region_name1, "7-day avg", "14-day avg"))


#-------------------------------------------------------------------------------------------------
# OpenTable Data
#-------------------------------------------------------------------------------------------------

opentable_data = read.csv("./Original Data/OpenTable_data.csv")
opentable_data <- opentable_data %>%
  filter(Type == "city") %>% # Get cities only (exclude countries and states)
  dplyr::select(-1) %>% # Drop location type variable
  column_to_rownames(var = "Name") # Set the row names to the city names

opentable_data <- data.frame(t(opentable_data)) # Transpose dataframe
date <- rownames(opentable_data)
date <- gsub("X", "", date) # Remove Xs in column names
date <- gsub("\\.", "-", date) # Replace periods with dashes
date <- paste0("2020-", date)
date <- as.Date(date)

opentable_data <- cbind(date, opentable_data)

windows()
ggplot(opentable_data, aes(x=date)) +
  geom_line(aes(y=Chicago, color="darkred")) +
  geom_line(aes(y=New.York, color="steelblue")) +
  geom_line(aes(y=Baltimore, color="gray70")) +
  theme_classic() +
  scale_color_discrete(name = "Location", labels = c("Chicago", "New York", "Baltimore"))


opentable_data %>%
  filter(Atlanta < -20) %>%
  slice(1)



events = read.csv("./covid_events.csv")

#-------------------------------------------------------------------------------------------------
# Regression Discontinuity
#-------------------------------------------------------------------------------------------------

city <- opentable_data %>%
  dplyr::select(date, y=New.York)

cutpoint = as.Date("2020-03-12")

cutoff_days <- 10 # Number of days to include in regression before and after cutpoint date

city2 <- city %>%
  filter(date <= cutpoint + cutoff_days & date >= cutpoint - cutoff_days)

rdd_data(city2$y, city2$date, cutpoint = cutpoint) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

city2 %>% 
  dplyr::select(date, y) %>% 
  mutate(D = as.factor(ifelse(date >= cutpoint, 1, 0))) %>% 
  ggplot(aes(x = date, y = y, color = D)) +
  geom_point() + 
  geom_smooth(method = "lm")

#-------------------------------------------------------------------------------------------------

lm1 <- lm(Stockholm ~ date, data=region1)
summary(lm1)
predict <- data.frame(predict=predict(lm1))
test <- cbind(region1, predict)

ggplot(test, aes(x=date)) +
  geom_line(aes(y=Stockholm, color="darkred")) +
  geom_line(aes(y=predict, color="steelblue")) +
  theme_classic()

########################################################################################################
data <- region1 %>%
  rename(n=Stockholm) %>%
  mutate(day=as.numeric(date-region1[1,1])+1)

b <- -1
mid <- 50
c <- 100
data$y <- c/(1+exp(b*(mid-data$day)))

c <- 100
a <- 2
b <- -1
c/(1+a*exp(-(b*data$day)))

a <- 0
k <- 1
b <- -.002
t <- data$day
q <- 5

data$y <- a + (k-a)/(1+q*exp(-b*t))

ggplot(data, aes(x=day)) +
  geom_line(aes(y=n/100, color="darkred")) +
  geom_line(aes(y=y, color="steelblue")) +
  theme_classic()

model <- drm(n ~ day, fct=L.3(), data=data)
summary(model)
windows()
plot(model, log="", main = "Logistic function")


nls1 <- nls(Stockholm ~ date, data=region1)

nls1 <- nls(region1$Stockholm~a*(1-exp(-c*region1$date)),start=list(a=120,c=0.064))


