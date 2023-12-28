#################################
#
# Library load and setup 
#
#################################

library(tidyverse)
library(viridis)
library(lubridate)
library(janitor)
library(gghighlight)
library(gganimate)


#################################
#
# Read in ANP Weather Data
#
#################################

raw_weather_data <- read_csv("/Users/KSHAN006/OneDrive - The Hershey Company/Desktop/anp_weather_data.csv")

cleaned_weather_data <- raw_weather_data %>%
  clean_names() %>%
  mutate(date = mdy(date),
         year = year(date),
         month = month(date),
         day = day(date),
         numday = yday(date)) %>%
  select(date,year,month,day,numday,tavg,tmax,tmin)

#################################
#
# Highlight just 2023 to visualize 
#
#################################

cleaned_weather_data %>%
  ggplot(aes(x = numday, y = tavg, color = year)) + 
  geom_line(color = "blue") + 
  gghighlight(year == 2023) +
  theme_bw() + 
  xlab("Day of the Year") + 
  ylab("Average Daily Temperature (F)") + 
  labs(title = "Daily Average Temperature by Day of Year",
       subtitle = "2003-2023, data from ANP Climate Data, 2023 Highlighted, 2021 Data Missing") + 
  guides(color="none")

#################################
#
# t-test
# against DAILY AVERAGES
#
#################################

cleaned_weather_data %>%
  mutate(group = ifelse(year == 2023, "2023","Not 2023")) %>%
  filter(month == 12) %>%
  {t.test(.$tavg ~ .$group, var.equal=FALSE)}

          
cleaned_weather_data %>% 
  group_by(year) %>%
  filter(month == 12) %>%
  summarize(avg = mean(tavg),
            n = n(),
            se = sd(tavg)/sqrt(n)) %>%
  ggplot(aes(x = year, y = avg)) + 
  geom_point(color="blue") + 
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2,
                position=position_dodge(.9)) +
  gghighlight(year == 2023,
              unhighlighted_params = list(colour = alpha("grey30", 0.3))) +
  theme_bw() + 
  xlab("Year") + ylab("Average Temperature in December (F)") + 
  labs(title = "Average December Temperatures",
       subtitle = "2003-2023, data from ANP Climate Data, Standard Errors Added") + 
  ylim(0,50)

#################################
#
# t-test
# against DAILY MAXIMUMS
#
#################################

cleaned_weather_data %>%
  mutate(group = ifelse(year == 2023, "2023","Not 2023")) %>%
  filter(month == 12) %>%
  {t.test(.$tmax ~ .$group, var.equal=FALSE)}

cleaned_weather_data %>% 
  group_by(year) %>%
  filter(month == 12) %>%
  summarize(avg = mean(tmax),
            n = n(),
            se = sd(tavg)/sqrt(n)) %>%
  ggplot(aes(x = year, y = avg)) + 
  geom_point(color="blue") + 
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2,
                position=position_dodge(.9)) +
  gghighlight(year == 2023,
              unhighlighted_params = list(colour = alpha("grey30", 0.3))) +
  theme_bw() + 
  xlab("Year") + ylab("Average Maximum Temperature in December (F)") + 
  labs(title = "Average Maximum December Temperatures",
       subtitle = "2003-2023, data from ANP Climate Data, Standard Errors Added") + 
  ylim(0,50)

#################################
#
# t-test
# against DAILY SPREAD
#
#################################


cleaned_weather_data %>%
  mutate(group = ifelse(year == 2023, "2023","Not 2023"),
         daily_spread = tmax - tmin) %>%
  filter(month == 12) %>%
  {t.test(.$daily_spread ~ .$group, var.equal=FALSE)}

cleaned_weather_data %>% 
  group_by(year) %>%
  filter(month == 12) %>%
  summarize(avg = mean(tmax - tmin),
            n = n(),
            se = sd(tavg)/sqrt(n)) %>%
  ggplot(aes(x = year, y = avg)) + 
  geom_point(color="blue") + 
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.2,
                position=position_dodge(.9)) +
  gghighlight(year == 2023,
              unhighlighted_params = list(colour = alpha("grey30", 0.3))) +
  theme_bw() + 
  xlab("Year") + ylab("Average Maximum Temperature in December (F)") + 
  labs(title = "Average Maximum December Temperatures",
       subtitle = "2003-2023, data from ANP Climate Data, Standard Errors Added") + 
  ylim(0,50)

#################################
#
# Pretty Chart :)
#
#################################


cleaned_weather_data %>%
  mutate(year = as.factor(year)) %>%
  group_by(year) %>%
  filter(month == 12) %>%
  ggplot(aes(x=year, y = tavg, color = tavg)) + 
  geom_jitter(width = 0.2) + 
  theme_bw() + 
  scale_color_viridis_c(option = "G", direction = 1) + 
  ylab("Temperature (Fahrenheit)") + 
  xlab("") + 
  labs(title = "Daily Average December Temperatures in Bar Harbor",
       subtitle = "2003-2023, Data from ANP Climate Data, 2021 Data Missing") + 
  theme(legend.position = "none")
