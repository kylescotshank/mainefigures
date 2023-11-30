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


# Obtain an API Key for the Census API here:
# https://api.census.gov/data/key_signup.html

api_key <- #####

tidycensus::census_api_key(api_key, install=TRUE)

options(tigris_use_cache=TRUE)


#################################
#
# Get Census Data (total pop.)
# for towns in ME, 2017-2021
#
#################################

my_vars <- c(
  total_pop = "DP05_0001"
)

years <- tibble::lst(2017,2018,2019,2020,2021)

raw_census_data <- purrr::map_dfr(
  years,
  ~ get_acs(
    geography = "county subdivision",
    variables = my_vars,
    geometry = TRUE,
    state = "ME",
    year = .x,
  ),
  .id = "year"
)


clean_census_data <- raw_census_data %>% 
  mutate(town = str_extract(NAME, "[^,]+")) %>%
  mutate(clean_town = str_remove_all(town, "city|town")) %>%
  distinct() %>%
  clean_names() %>%
  mutate(total_population = estimate,
         clean_town = tolower(clean_town),
         clean_town = str_trim(clean_town),
         town = clean_town,
         year = as.numeric(year)) %>% 
  select(geoid, year, town, total_population)


#################################
#
# Get Annual Sales Tax data
# from Maine Revenue Services
# https://www.maine.gov/revenue/taxes/tax-policy-office/sales-tax-reports
#
#################################


tmp = tempfile(fileext = ".xlsx")
download.file(url = "https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/TownAnnual021323.xlsx", destfile = tmp, mode="wb")

raw_sales_data <- read_excel(tmp, skip=1)

clean_sales_data <- raw_sales_data %>%
  clean_names() %>%
  filter(year <= 2021) %>%
  mutate(economic_area = tolower(economic_area),
         town = tolower(town)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  replace(is.na(.), 0) %>%
  # clean up some bad names on the ME state tax data
  mutate(across('town', str_replace,'hbr','harbor')) %>%
  mutate(across('town', str_replace, 'plt','plantation')) %>%
  mutate(across('town', str_replace, 'deer islande', 'deer isle')) %>%
  mutate(across('town', str_replace, 'brooklyn', 'brooklin')) %>% 
  mutate(across('town', str_replace, 'swans isl', 'swans island')) %>%
  mutate(across('town', str_replace, 'isleboro','islesboro')) %>%
  mutate(across('town', str_replace, 'new glouster','new gloucester')) %>%
  mutate(across('town', str_replace, 'sabbattus','sabattus')) %>%
  mutate(across('town', str_replace, 'parsonfield','parsonsfield'))


#################################
#
# Join to generate working data
# for analysis
#
#################################

working_data <- clean_census_data %>% 
  left_join(., clean_sales_data, by = c("town","year"))

plot_data <- working_data %>%
  select(year, town, lodging, restaurant, total_population, total) %>%
  mutate(total_revenue_per_capita = total / total_population,
         lodging_revenue_per_capita = lodging / total_population,
         restaurant_revenue_per_capita = restaurant / total_population)

#################################
#
# Header Chart | 
# Total 2021 Pop and Total 2021
# Taxable Sales
#
#################################

pop_2021_chart <- plot_data %>% 
  filter(year == 2021) %>%
  mutate(total_popluation_category = cut(total_population, breaks = c(-Inf,100,500,1000,5000,10000,Inf), 
                                         labels = c("< 100","100 - 500","500 -1,000","1,000-5,000","5,000-10,000","10,000+"),
                                         ordered_result = TRUE)) %>%
  ggplot(aes(fill = total_popluation_category)) + 
  geom_sf(color = I("gray70")) + 
  scale_fill_viridis_d(option = "G", direction = -1, name = "Total Population") +
  theme_bw() + 
  labs(title = "Total Population by Town",
       subtitle = "2021 Census Data")+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "left") + 
  coord_sf()


sales_2021_chart <- plot_data %>% 
  filter(year == 2021) %>%
  mutate(total = if_else(is.na(total),0,total)) %>% 
  mutate(total_sales_category = cut(total, breaks = c(-Inf,50000000,100000000,500000000,1000000000,2000000000,Inf), 
                                         labels = c("< $50M","$50M - $100M","$100M-$500M","$500M - $1B","$1B - $2B",">$2B"),
                                         ordered_result = TRUE)) %>%
  ggplot(aes(fill = total_sales_category)) + 
  geom_sf(color = I("gray70")) + 
  scale_fill_viridis_d(option = "E", direction = -1, name = "Total Taxable Sales") +
  theme_bw() + 
  labs(title = "Total Taxable Sales by Town",
  subtitle = "2021 Sales Data") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") + 
  coord_sf()

pop_2021_chart + sales_2021_chart

sales_2021_chart



#################################
#
# Line Chart | Top 20 Towns
# by Total Sales 2017-2021
#
#################################
clean_sales_data %>%
  select(year, town, total) %>%
  group_by(year) %>% 
  top_n(10,total) %>%
  ungroup %>%
  ggplot(aes(x = year, y = total, color = town)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(y = "Total Taxable Sales",
       x = "Year",
       title = "Total Taxable Sales by Town (2007-2021)") + 
  theme_bw() + 
  guides(color=guide_legend(title="Town"))

#################################
#
# Proportional Bar Chart | Revenue Types
# by Total Taxable Revenue
#
#################################

clean_sales_data %>%
  select(-c(town, economic_area, restaurant_lodging,total)) %>% 
  group_by(year) %>% 
  pivot_longer(!year, names_to = "revenue_type", values_to = "revenue") %>%
  ggplot(aes(x = year, y = revenue, fill = revenue_type)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% of All Taxable Sales",
       x = "Year",
       title = "% of Taxable Sales by Revenue Type (2007-2021)") + 
  theme_bw() + 
  guides(fill=guide_legend(title="Revenue Type"))



#################################
#
# Bar Chart | Top 10 Towns
# by Sales per Capita 2017-2021
#
#################################

plot_data %>%
  filter(year %in% 2017:2021) %>%
  select(year, town, total_revenue_per_capita,total_population) %>%
  group_by(year) %>% 
  top_n(10,total_revenue_per_capita) %>%
  ungroup %>%
  mutate(year = as.factor(year),
         town = reorder_within(town, total_revenue_per_capita, year)) %>%
  ggplot(aes(town, total_revenue_per_capita, fill = year)) +
  geom_col(show.legend = FALSE, col=I("black")) +
  geom_text(aes(label=total_population),color="white",size=5,position=position_stack(vjust=0.5)) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(y = "Total Taxable Sales per Capita",
       x = NULL,
       title = "Total Taxable Sales by Town (2017-2021)",
       subtitle = "ACS Population Estimates Added") + 
  theme_bw()

#################################
#
# Choropleth | Top Towns
# by Sales per Capita 2017-2021
#
#################################

plot_data %>% 
  filter(year == 2021) %>%
  mutate(total_revenue_per_capita = if_else(is.na(total_revenue_per_capita),0,total_revenue_per_capita)) %>% 
  mutate(total_sales_category = cut(total_revenue_per_capita, breaks = c(-Inf,10000,25000,50000,100000,150000,Inf), 
                                    labels = c("< $10k","$10k - $25k","$25k-$50k","$50k - $100k","$100k - $150k",">$150k"),
                                    ordered_result = TRUE)) %>%
  ggplot(aes(fill = total_sales_category)) + 
  geom_sf(color = I("gray70")) + 
  scale_fill_viridis_d(option = "F", direction = -1, name = "Total Taxable Sales per Capita") +
  theme_bw() + 
  labs(title = "Total Taxable Sales per Capita by Town",
       subtitle = "2021 Sales + Census Data") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") + 
  coord_sf()

#################################
#
# Choropleth | Top Towns
# by Sales per Capita (Lodging and Restaurants) 2017-2021
#
#################################

lodging_per_capita <- plot_data %>% 
  filter(year == 2021) %>%
  mutate(lodging_revenue_per_capita = if_else(is.na(lodging_revenue_per_capita),0,lodging_revenue_per_capita)) %>% 
  mutate(total_sales_category = cut(lodging_revenue_per_capita, breaks = c(-Inf,1000,5000,10000,20000,50000,Inf), 
                                    labels = c("< $1k","$1k - $5k","$5k-$10k","$10k - $20k","$20k - $50k",">$50k"),
                                    ordered_result = TRUE)) %>%
  ggplot(aes(fill = total_sales_category)) + 
  geom_sf(color = I("gray70")) + 
  scale_fill_viridis_d(option = "C", direction = -1, name = "Total Lodging Sales per Capita") +
  theme_bw() + 
  labs(title = "Total Lodging Sales per Capita",
       subtitle = "2021 Sales + Census Data") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") + 
  coord_sf()

restaurant_per_capita <- plot_data %>% 
  filter(year == 2021) %>%
  mutate(restaurant_revenue_per_capita = if_else(is.na(restaurant_revenue_per_capita),0,restaurant_revenue_per_capita)) %>% 
  mutate(total_sales_category = cut(restaurant_revenue_per_capita, breaks = c(-Inf,1000,5000,10000,20000,50000,Inf), 
                                    labels = c("< $1k","$1k - $5k","$5k-$10k","$10k - $20k","$20k - $50k",">$50k"),
                                    ordered_result = TRUE)) %>%
  ggplot(aes(fill = total_sales_category)) + 
  geom_sf(color = I("gray70")) + 
  scale_fill_viridis_d(option = "G", direction = -1, name = "Total Restaurant Sales per Capita") +
  theme_bw() + 
  labs(title = "Total Restaurant Sales per Capita",
       subtitle = "2021 Sales + Census Data") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") + 
  coord_sf()

lodging_per_capita + restaurant_per_capita

#################################
#
# Bar Chart | Top 25 Towns
# by Estimated Tax Collected 
#
#################################

plot_data %>%
  filter(year == 2021) %>%
  mutate(select_revenue = total - restaurant - lodging,
         restaurant_revenue = restaurant * 0.08,
         lodging_revenue = lodging * 0.09,
         ao_revenue = select_revenue * 0.05,
         total_tax_collected = restaurant_revenue + lodging_revenue + ao_revenue,
         total_tax_collected_per_capita = total_tax_collected / total_population) %>% 
  select(town, total_tax_collected_per_capita) %>%
  top_n(25,total_tax_collected_per_capita) %>%
  arrange(desc(total_tax_collected_per_capita)) %>%
  as_tibble() %>%
  select(-geometry) %>% 
  ggplot(aes(reorder(town, total_tax_collected_per_capita), total_tax_collected_per_capita)) +
  geom_bar(stat = "identity", col = I("black")) + 
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(y = "Total Est. Tax Revenue Collected per Capita",
       x = NULL,
       title = "Total Estimated Tax Revenue Collected per Capita",
       subtitle = "Top 25 in 2021") + 
  theme_bw()

#################################
#
# Choropleth | Top Towns
# by Estimated Tax Collected per Capita
#
#################################

plot_data %>% 
  filter(year == 2021) %>%
  mutate(select_revenue = total - restaurant - lodging,
         restaurant_revenue = restaurant * 0.08,
         lodging_revenue = lodging * 0.09,
         ao_revenue = select_revenue * 0.05,
         total_tax_collected = restaurant_revenue + lodging_revenue + ao_revenue,
         total_tax_collected_per_capita = total_tax_collected / total_population) %>% 
  mutate(total_tax_collected_per_capita = if_else(is.na(total_tax_collected_per_capita),0,total_tax_collected_per_capita)) %>% 
  mutate(total_sales_category = cut(total_tax_collected_per_capita, breaks = c(-Inf,1000,2000,5000,10000,15000,Inf), 
                                    labels = c("< $1k","$1k - $2k","$2k-$5k","$5k - $10k","$10k - $15k",">$15k"),
                                    ordered_result = TRUE)) %>%
  ggplot(aes(fill = total_sales_category)) + 
  geom_sf(color = I("gray70")) + 
  scale_fill_viridis_d(option = "I", direction = -1, name = "Total Estimated Tax Revenue per Capita") +
  theme_bw() + 
  labs(title = "Total Estimated Tax Revenue per Capita",
       subtitle = "2021 Sales + Census Data") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") + 
  coord_sf()

#################################
#
# Average difference
#
#################################

plot_data %>%
  filter(year == 2021) %>%
  mutate(select_revenue = total - restaurant - lodging,
         restaurant_revenue = restaurant * 0.08,
         lodging_revenue = lodging * 0.09,
         ao_revenue = select_revenue * 0.05,
         total_tax_collected = restaurant_revenue + lodging_revenue + ao_revenue,
         total_tax_collected_per_capita = total_tax_collected / total_population) %>% 
  select(town, total_population, total_tax_collected_per_capita) %>%
  mutate(mdi = ifelse(town %in% c("bar harbor","southwest harbor","tremont","mount desert"),"mdi","not_mdi")) %>%
  do(tidy(t.test(total_tax_collected_per_capita ~ mdi, data = ., na.rm=T)))

plot_data %>%
  as_tibble() %>%
  filter(year == 2021) %>%
  mutate(select_revenue = total - restaurant - lodging,
         restaurant_revenue = restaurant * 0.08,
         lodging_revenue = lodging * 0.09,
         ao_revenue = select_revenue * 0.05,
         total_tax_collected = restaurant_revenue + lodging_revenue + ao_revenue,
         total_tax_collected_per_capita = total_tax_collected / total_population) %>% 
  select(town, total_population, total_tax_collected_per_capita) %>%
  mutate(mdi = ifelse(town %in% c("bar harbor","southwest harbor","tremont","mount desert"),"mdi","not_mdi")) %>%
  group_by(mdi) %>%
  summarize(avg_ttcpc = mean(total_tax_collected_per_capita, na.rm = T))
