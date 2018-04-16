setwd("C:\\Users\\dudey\\Desktop\\DTLAnalysis\\DowntownLineAnalysis\\csv\\resale_flat_prices")
residential_price_data <- read.csv("final.csv")
residential_price_index <- read.csv("residential-property-price-index-base-quarter-2009-q1-100-quarterly.csv")

library(dplyr)
library(lubridate)
library(zoo)

base_quarter <- "2000-Q1"
# Taking only non-landed and rebasing it for base_quarter
rpi2 <- residential_price_index %>% 
  filter(property_type == "Non-Landed") %>% 
  mutate(rebased_index = index / residential_price_index[residential_price_index$quarter == base_quarter & residential_price_index$property_type == "Non-Landed", ]$index) %>% 
  select(quarter, index, rebased_index) %>%
  mutate(quarter = as.yearqtr(as.character(quarter), "%Y-Q%q"))

# Sanity Check
head(rpi2)
rpi2[rpi2$quarter == "2000 Q1",]
tail(rpi2)

# Preparing Residential Price Data
residential_price_data_2 <- residential_price_data %>%
  mutate(month = as.yearmon(month)) %>%
  mutate(quarter = as.yearqtr(month))

# Sanity Check
head(residential_price_data_2)

# Joining
residential_price_data_3 <- merge(residential_price_data_2, 
                                  rpi2, 
                                  c("quarter", "quarter"))

# Sanity Check
head(residential_price_data_3)

# Residential Price Data 4
residential_price_data_4 <- residential_price_data_3 %>%
  mutate(resale_price_rebased = resale_price / rebased_index) 

# Sanity Check
head(residential_price_data_4)
tail(residential_price_data_4)
residential_price_data_4 %>% 
  filter(month == as.yearmon("1990-01")) %>%
  filter(town == "TAMPINES") %>%
  group_by(flat_type, storey_range) %>%
  summarise(mean_rebased_price = mean(resale_price_rebased), mean_price = mean(resale_price))

residential_price_data_4 %>% 
  filter(month == as.yearmon("2010-01")) %>%
  filter(town == "TAMPINES") %>%
  group_by(flat_type, storey_range) %>%
  summarise(mean_rebased_price = mean(resale_price_rebased), mean_price = mean(resale_price))

write.csv(x = residential_price_data_4, file = "residential_property_price_base_Q1_2000.csv")
