#################################
#
# Library load and setup 
#
#################################

library(tidyverse)
library(tidytext)
library(viridis)
library(purrr)
library(tibble)
library(stringr)
library(janitor)
library(tidycensus)
library(readxl)
library(scales)
library(maps)
library(sf)
library(patchwork)
library(lubridate)


#################################
#
# Get Historical Mill Rates
#
#################################


raw_mill_rates <- read_excel(path = "/Users/KSHAN006/Downloads/fullvaluerates.xlsx")

clean_mill_rates <- raw_mill_rates %>%
  mutate(county = tolower(COUNTY),
         town = tolower(TOWN)) %>% 
  select(-c(COUNTY,TOWN)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  replace(is.na(.), 0) %>% 
  pivot_longer(names_to = "year", -c(county,town)) %>%
  mutate(year = as.integer(year),
         rate = value) %>%
  select(-value) %>%
  # clean up some bad names on the ME state tax data
  mutate(across('county', str_replace,'aroostok','aroostook')) %>%
  mutate(across('county', str_replace,'piscatquis','piscataquis')) %>%
  mutate(across('town', str_replace,'hbr','harbor')) %>%
  mutate(across('town', str_replace,'soh','south')) %>%
  mutate(across('town', str_replace, 'plt','plantation')) %>%
  mutate(across('town', str_replace, 'deer islande', 'deer isle')) %>%
  mutate(across('town', str_replace, 'brooklyn', 'brooklin')) %>% 
  mutate(across('town', str_replace, 'swans isl', 'swans island')) %>%
  mutate(across('town', str_replace, 'isleboro','islesboro')) %>%
  mutate(across('town', str_replace, 'new glouster','new gloucester')) %>%
  mutate(across('town', str_replace, 'sabbattus','sabattus')) %>%
  mutate(across('town', str_replace, 'parsonfield','parsonsfield')) %>%
  mutate(across('town', str_replace, 'saint george','st. george')) %>%
  mutate(across('town', str_replace,'falmoh','falmouth')) %>% 
  mutate(across('town', str_replace,'yarmoh','yarmouth')) %>%
  mutate(across('town', str_replace,'monmoh','monmouth')) %>%
  mutate(across('town', str_replace,'verona','verona island')) %>%
  mutate(town = str_remove_all(town, "'")) %>%
  mutate(across('town', str_replace, 'saint agatha','st. agatha')) %>%
  mutate(across('town', str_replace, 'saint francis','st. francis')) %>%
  mutate(across('town', str_replace, 'saint john plantation','st. john plantation')) %>%
  mutate(across('town', str_replace,'isle au ha','isle au haut'))
  
#################################
#
# Get Historical State Valuations
#
#################################

raw_state_valuations <- read_excel(path = "/Users/KSHAN006/Downloads/statevaluation_cleaned.xlsx")

clean_state_valuations <- raw_state_valuations %>%
  mutate(county = tolower(COUNTY),
         town = tolower(MUNICIPALITY)) %>% 
  select(-c(COUNTY,MUNICIPALITY)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  replace(is.na(.), 0) %>% 
  pivot_longer(names_to = "year", -c(county,town)) %>%
  mutate(year = as.integer(year),
         valuation = value) %>%
  select(-value) %>%
  # clean up some bad names on the ME state tax data
  mutate(across('county', str_replace,'aroostok','aroostook')) %>%
  mutate(across('county', str_replace,'piscatquis','piscataquis')) %>%
  mutate(across('town', str_replace,'hbr','harbor')) %>%
  mutate(across('town', str_replace,'soh','south')) %>%
  mutate(across('town', str_replace, 'plt','plantation')) %>%
  mutate(across('town', str_replace, 'deer islande', 'deer isle')) %>%
  mutate(across('town', str_replace, 'brooklyn', 'brooklin')) %>% 
  mutate(across('town', str_replace, 'swans isl', 'swans island')) %>%
  mutate(across('town', str_replace, 'isleboro','islesboro')) %>%
  mutate(across('town', str_replace, 'new glouster','new gloucester')) %>%
  mutate(across('town', str_replace, 'sabbattus','sabattus')) %>%
  mutate(across('town', str_replace, 'parsonfield','parsonsfield')) %>%
  mutate(across('town', str_replace, 'saint george','st. george')) %>%
  mutate(across('town', str_replace,'falmoh','falmouth')) %>% 
  mutate(across('town', str_replace,'yarmoh','yarmouth')) %>%
  mutate(across('town', str_replace,'monmoh','monmouth')) %>%
  mutate(across('town', str_replace,'verona','verona island')) %>%
  mutate(town = str_remove_all(town, "'")) %>%
  mutate(across('town', str_replace, 'saint agatha','st. agatha')) %>%
  mutate(across('town', str_replace, 'saint francis','st. francis')) %>%
  mutate(across('town', str_replace, 'saint john plantation','st. john plantation')) %>%
  mutate(across('town', str_replace,'isle au ha','isle au haut'))

#################################
#
# Get Town Parcels
#
#################################

raw_town_parcels <- read_csv("/Users/KSHAN006/Downloads/Maine_Parcels_Organized_Towns_Feature.csv")

clean_town_parcels <- raw_town_parcels %>%
  mutate(county = tolower(COUNTY),
         town = tolower(TOWN)) %>% 
  mutate(across('county', str_replace,'piscatquis','piscataquis')) %>%
  select(-c(COUNTY,TOWN)) %>%
  group_by(town, county) %>%
  tally()

#################################
#
# Join to generate working data
# for analysis - limit to 2016
# to avoid weird behavior in the underlying
# shapefiles from the Census
#
#################################

working_data <-  clean_mill_rates %>%
  left_join(., clean_state_valuations, by = c("town","year","county")) %>%
  left_join(., clean_town_parcels, by = c("town","county")) %>% 
  mutate(parcel_count = n,
         fixed_valuation = valuation * 1000,
         total_property_levy = (rate*fixed_valuation)/1000,
         avg_property_valuation = (fixed_valuation)/parcel_count,
         avg_individual_property_levy = (rate * avg_property_valuation)/1000) %>%
  select(county,town,year,rate,fixed_valuation,total_property_levy,parcel_count,avg_property_valuation,avg_individual_property_levy)


#################################
#
# CHART | 
# % Total Property Value
#
#################################

working_data %>%
  select(year, fixed_valuation) %>%
  group_by(year) %>%
  summarize(total_valuation = sum(fixed_valuation,na.rm=T)) %>%
  ggplot(aes(x = year, y = total_valuation)) +
  geom_bar(stat = "identity", color = I("black")) + 
  theme_bw() + 
  scale_y_continuous(labels = dollar_format()) +
  xlab("") + ylab("") + 
  labs(title = "Total Property Valuation By Year") + 
  theme(legend.position = "none") 


#################################
#
# CHART | 
# Median Mill Rate 
#
#################################

working_data %>%
  select(year, rate) %>%
  group_by(year) %>%
  summarize(n = n(),
            avg_mill_rate = mean(rate,na.rm=T),
            se_mill_rate = sd(rate, na.rm=T)/sqrt(n)) %>%
  ggplot(aes(x = year, y = avg_mill_rate)) +
  geom_line(color = "darkred") + 
  geom_point(color = "darkred") + 
  geom_errorbar(aes(min = avg_mill_rate - se_mill_rate, ymax = avg_mill_rate + se_mill_rate),
                color = "darkred") + 
  theme_bw() + 
  scale_y_continuous(labels = dollar_format()) +
  xlab("") + ylab("") + 
  labs(title = "Avgerage Mill Rate By Year",
       subtitle = "Standard Errors Added") + 
  theme(legend.position = "none") 


#################################
#
# CHART | 
# % Change in Total Property Tax by Town and County
#
#################################


working_data %>%
  select(county,town,year,avg_individual_property_levy) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  mutate(pct_chg = (avg_individual_property_levy-lag(avg_individual_property_levy))/lag(avg_individual_property_levy)) %>% 
  ungroup() %>%
  group_by(county,year) %>%
  filter(!is.nan(pct_chg),
         !is.infinite(pct_chg)) %>% 
  mutate(mean_pct_chg = median(pct_chg, na.rm=T))  %>%
  ungroup() %>% 
  filter(year>2012) %>%
  ggplot(aes(x = year, y = pct_chg, group = town)) + 
  geom_line(linewidth = 1, color="grey90",alpha=0.9) + 
  geom_line(aes(x = year, y = mean_pct_chg, group = county),
            col = "darkred", linewidth = 1) + 
  geom_point(aes(x=year, y = mean_pct_chg, group = county),
             col = "darkred") + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format()) + 
  xlab("") + ylab("% Change in Total Property Tax") + 
  facet_wrap(~county, ncol = 3, scales = "free_y") +
  labs(title = "Percentage Change in Estimated Average Property Tax by Town and County",
       subtitle = "Median % Change by County Highlighted, Unincorporated/Deincorporated Territories Removed")



#################################
#
# CHART | 
# Absolute Dollar Change in Average Property Tax by Town and County
#
#################################


working_data %>%
  select(county,town,year,avg_individual_property_levy) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,year) %>%
  filter(!is.nan(avg_individual_property_levy),
         !is.infinite(avg_individual_property_levy),
         avg_individual_property_levy > 0) %>%
  mutate(mean_individual_property_levy = median(avg_individual_property_levy, na.rm=T))  %>%
  ungroup() %>%
  ggplot(aes(x = year, y = avg_individual_property_levy, group = town)) + 
  geom_line(linewidth = 1, color="grey90",alpha=0.9) + 
  geom_line(aes(x = year, y = mean_individual_property_levy, group = county),
            col = "darkred", linewidth = 1) + 
  geom_point(aes(x=year, y = mean_individual_property_levy, group = county),
             col = "darkred") + 
  theme_bw() + 
  scale_y_continuous(labels = dollar_format()) + 
  xlab("") + ylab("Estimated Avg. Individual Property Tax") + 
  facet_wrap(~county, ncol = 3, scales = "free_y") +
  labs(title = "Estimated Average Property Tax by Town and County",
       subtitle = "County-level Yearly Median Highlighted, Unincorporated/Deincorporated Territories Removed")


#################################
#
# CHART | 
# Change in State Avg. Mill Rate & 
# Change in State Avg. Town Valuation
#
#################################

value <- working_data %>%
  select(county,town,year,fixed_valuation, rate) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  filter(fixed_valuation > 0) %>%
  mutate(pct_chg_property = (fixed_valuation-lag(fixed_valuation))/lag(fixed_valuation)) %>%
  ungroup() %>%
  filter(!is.nan(pct_chg_property),
         !is.infinite(pct_chg_property)) %>% 
  filter(year>2012) %>%
  ggplot(aes(x=year, y = pct_chg_property, color = pct_chg_property)) + 
  geom_jitter(width = 0.2) + 
  theme_bw() + 
  scale_color_viridis_c(option = "G", direction = -1) + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format(accuracy=0.1)) +
  xlab("") + ylab("% Change") + 
  labs(title = "% Change in Total Property Valuation",
       subtitle = "By Town, By Year, UTs Excluded") + 
  theme(legend.position = "none") 

rate <- working_data %>%
  select(county,town,year,fixed_valuation, rate) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  filter(fixed_valuation > 0) %>%
  mutate(pct_chg_rate = (rate-lag(rate))/lag(rate)) %>%
  ungroup() %>%
  filter(!is.nan(pct_chg_rate),
         !is.infinite(pct_chg_rate)) %>% 
  filter(year>2012) %>%
  ggplot(aes(x=year, y = pct_chg_rate, color = pct_chg_rate)) + 
  geom_jitter(width = 0.2) + 
  theme_bw() + 
  scale_color_viridis_c(option = "B", direction = -1) + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format(accuracy=0.1)) +
  xlab("") + ylab("% Change") + 
  labs(title = "% Change in Mill Rate",
       subtitle = "By Town, By Year, UTs Excluded") + 
  theme(legend.position = "none") 

value_avg <- working_data %>%
  select(county,town,year,fixed_valuation, rate) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  filter(fixed_valuation > 0) %>%
  mutate(pct_chg_property = (fixed_valuation-lag(fixed_valuation))/lag(fixed_valuation)) %>%
  ungroup() %>%
  filter(!is.nan(pct_chg_property),
         !is.infinite(pct_chg_property)) %>% 
  filter(year>2012) %>%
  group_by(year) %>%
  summarize(avg_value_chg = median(pct_chg_property)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y = avg_value_chg)) + 
  geom_line(aes(group = 1), color = "grey80") +
  geom_point(color = "darkblue") + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format(accuracy=0.1)) +
  xlab("") + ylab("% Change") + 
  labs(title = "Median % Change in Total Property Valuation",
       subtitle = "Entire State, By Year, UTs Dropped") + 
  theme(legend.position = "none") 

rate_avg <- working_data %>%
  select(county,town,year,fixed_valuation, rate) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  filter(fixed_valuation > 0) %>%
  mutate(pct_chg_rate = (rate-lag(rate))/lag(rate)) %>%
  ungroup() %>%
  filter(!is.nan(pct_chg_rate),
         !is.infinite(pct_chg_rate)) %>% 
  filter(year>2012) %>%
  group_by(year) %>%
  summarize(avg_rate_chg = median(pct_chg_rate)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y = avg_rate_chg)) + 
  geom_line(aes(group = 1), color = "grey80") +
  geom_point(color = "darkred") + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format(accuracy=0.1)) +
  xlab("") + ylab("% Change") + 
  labs(title = "Median % Change in Mill Rates",
       subtitle = "Entire State, By Year, UTs Dropped") + 
  theme(legend.position = "none") 

(value + rate) /
(value_avg + rate_avg)

#################################
#
# CHART | 
# % Total Tax Take
#
#################################

working_data %>%
  select(year, total_property_levy) %>%
  group_by(year) %>%
  summarize(total_property_tax = sum(total_property_levy,na.rm=T)) %>%
  ggplot(aes(x = year, y = total_property_tax, fill = total_property_tax)) +
  geom_bar(stat = "identity", color = I("black")) + 
  scale_color_viridis_c(option = "G", direction = -1) + 
  theme_bw() + 
  scale_y_continuous(labels = dollar_format()) +
  xlab("") + ylab("Total Property Tax") + 
  labs(title = "Total Property Tax By Year") + 
  theme(legend.position = "none") 


#################################
#
# CHART | 
# % Change in Total Property Tax
#
#################################


working_data %>%
  select(county,town,year,total_property_levy) %>%
  mutate(year = as.character(year)) %>%
  group_by(town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  mutate(pct_chg = (total_property_levy-lag(total_property_levy))/lag(total_property_levy)) %>% 
  ungroup() %>%
  group_by(year) %>%
  filter(!is.nan(pct_chg),
         !is.infinite(pct_chg)) %>% 
  mutate(mean_pct_chg = mean(pct_chg, na.rm=T))  %>%
  ungroup() %>% 
  filter(year>2012) %>%
  ggplot(aes(x = year, y = pct_chg, group = town)) + 
  geom_line(linewidth = 1, color="grey90",alpha=0.9) + 
  geom_line(aes(x = year, y = mean_pct_chg, group = county),
            col = "darkblue", linewidth = 1) + 
  geom_point(aes(x=year, y = mean_pct_chg, group = county),
             col = "darkblue") + 
  geom_hline(aes(x=year, yintercept=0), color = "black") + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format(),
                     limits = c(-0.5,0.5)) + 
  xlab("") + ylab("% Change in Total Property Tax") + 
  labs(title = "Percentage Change in Total Property Tax",
       subtitle = "Median % Change Highlighted, Outliers Removed for Visualization")


working_data %>%
  select(county,town,year,total_property_levy) %>%
  mutate(year = as.character(year)) %>%
  group_by(town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  mutate(pct_chg = (total_property_levy-lag(total_property_levy))/lag(total_property_levy)) %>% 
  ungroup() %>%
  group_by(year) %>%
  filter(!is.nan(pct_chg),
         !is.infinite(pct_chg)) %>% 
  summarize(mean_pct_chg = mean(pct_chg, na.rm=T)) %>%
  mutate(avg = mean(mean_pct_chg, na.rm=T))

working_data %>%
  select(year,total_property_levy) %>%
  group_by(year) %>%
  summarize(avg_levy = mean(total_property_levy, na.rm=T)) %>%
  mutate(avg = mean(avg_levy, na.rm=T))

#################################
#
# CHART | 
# Absolute Dollars in Property Tax by Town and County
#
#################################

working_data %>%
  select(county,town,year,total_property_levy) %>%
  mutate(year = as.character(year)) %>%
  group_by(county,town) %>%
  arrange(year, .by_group = TRUE) %>%
  drop_na() %>%
  mutate(pct_chg = (total_property_levy-lag(total_property_levy))/lag(total_property_levy)) %>% 
  ungroup() %>%
  group_by(county,year) %>%
  filter(!is.nan(pct_chg),
         !is.infinite(pct_chg)) %>% 
  mutate(mean_pct_chg = median(pct_chg, na.rm=T))  %>%
  ungroup() %>% 
  filter(year>2012) %>%
  ggplot(aes(x = year, y = pct_chg, group = town)) + 
  geom_line(linewidth = 1, color="grey90",alpha=0.9) + 
  geom_line(aes(x = year, y = mean_pct_chg, group = county),
            col = "darkblue", linewidth = 1) + 
  geom_point(aes(x=year, y = mean_pct_chg, group = county),
             col = "darkblue") + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format()) + 
  xlab("") + ylab("% Change in Total Property Tax") + 
  facet_wrap(~county, ncol = 3, scales = "free_y") +
  labs(title = "Percentage Change in Total Property Tax by Town and County",
       subtitle = "Median % Change by County Highlighted, Unincorporated/Deincorporated Territories Removed")

#################################
#
# CHART | 
# $$ Change on MDI Towns
#
#################################

working_data %>%
  select(county,town,year,avg_individual_property_levy) %>%
  filter(town %in% c("bar harbor","mount desert","southwest harbor","tremont")) %>% 
  ggplot(aes(x = year, y = avg_individual_property_levy, group = town, color = town)) + 
  geom_line() + 
  geom_point() + 
  theme_bw() + 
  scale_y_continuous(labels = dollar_format()) + 
  xlab("") + ylab("Estimated Avg. Individual Property Tax") + 
  labs(title = "Estimated Average Property Tax by MDI Town")


working_data %>%
  select(town,year,avg_individual_property_levy) %>%
  filter(town %in% c("bar harbor","mount desert","southwest harbor","tremont")) %>%
  filter(year %in% c(2012,2021)) %>%
  pivot_wider(names_from = year, values_from = avg_individual_property_levy) %>%
  mutate(incremental = `2021` - `2012`) %>%
  select(town, incremental)
