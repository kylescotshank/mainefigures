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

#################################
#
# Get Annual Sales Tax data for 2022
#
#################################


raw_assessor_data <- read_excel("/Users/KSHAN006/OneDrive - The Hershey Company/Desktop/assessor_database.xlsx")

clean_assessor_data <- raw_assessor_data %>%
  mutate(PARCEL_TOTAL_VALUE = as.numeric(TOTAL_VAL),
         ACREAGE = as.numeric(ACREAGE),
         LAST_SALE_PRICE = as.numeric(PRICE),
         OCCUPANCY = as.numeric(OCCUPANCY),
         PARCEL_ID = cama_id,
         NUM_BEDRM = as.numeric(NUM_BEDRM)) %>%
  clean_names() %>%
  mutate(owner_1 = gsub("'", '', owner_1),
         owner_2 = gsub("'", '', co_owner)) %>%
  select(parcel_id, acreage, occupancy, use_desc, use_code, zone, zone_desc, owner_1, co_owner, mail_state, mail_zip,num_bedrm)

#################################
#
# High Level Analysis
#
#################################

# Total Number of Parcels 

clean_assessor_data %>% 
  summarize(count_of_parcels = length(parcel_id))

# Total Number of Parcels ex-ANP and BH Municipal

clean_assessor_data %>%
  filter(!(owner_1 %in% c('ACADIA NATIONAL PARK','ACADIA NATL PARK VISITORS CTR','UNITED STATES OF AMERICA','BAR HARBOR, TOWN OF',
                          'BAR HARBOR TOWN OF',
                          'MDI REGIONAL SCHOOL DISTRICT',
                          'BAR HARBOR HOUSING AUTHORITY'))) %>%
  summarize(count_of_parcels = length(parcel_id))

# Set data for analysis 

analysis_data <- clean_assessor_data %>%
  filter(!(owner_1 %in% c('ACADIA NATIONAL PARK','ACADIA NATL PARK VISITORS CTR','UNITED STATES OF AMERICA','BAR HARBOR, TOWN OF',
                          'BAR HARBOR TOWN OF',
                          'MDI REGIONAL SCHOOL DISTRICT',
                          'BAR HARBOR HOUSING AUTHORITY')))

#################################
#
# How many properties are there
# by Overall Property Type? 
#
#################################

analysis_data %>%
  group_by(use_desc) %>%
  tally() %>%
  summarize(total_number_uses = length(use_desc))
  
#################################
#
# How many properties are there
# by Overall Property Type? 
#
#################################


analysis_data %>%
  group_by(use_desc) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(perc = prop.table(n))

#################################
#
# How many properties are there
# by Overall Property Type? 
#
#################################

analysis_data %>%
  group_by(use_desc) %>%
  tally() %>%
  filter(n == 1) %>%
  summarize(total_parcels_using_equals_one = length(use_desc))


#################################
#
# What are the "Housing" Uses? 
#
#################################

analysis_data %>%
  group_by(use_desc) %>%
  tally() %>%
  arrange(desc(n)) %>%
  View()

housing_uses <- c("SINGLE FAM",
                  "OCN FT",
                  "CONDO  MDL-05",
                  "MULTI HSES",
                  "TWO FAMILY",
                  "THREE FAM",
                  "MULTI HSES OCEAN",
                  "CONDO MAIN",
                  "RTL CONDO",
                  "MOBILE HM",
                  "APT 4-UNT", 
                  "APT 5 UNIT",
                  "APT OVER 8  MDL-94",
                  "APT 6 UNIT",
                  "APT 7 UNIT",
                  "APT OVER 8  MDL-01",
                  "CONDO NL",
                  "CONDO LCE",
                  "LAKE FT")

analysis_data %>%
  filter(!(use_desc %in% housing_uses)) %>%
  filter(occupancy > 0) %>%
  View()

#################################
#
# Set final analysis data 
#
#################################

housing_data <- analysis_data %>%
  filter(use_desc %in% housing_uses | num_bedrm > 0) %>%
  drop_na(num_bedrm) %>%
  filter(use_desc != "CHARITABLE  MDL-01",
         use_desc != "CO OP RES")

housing_data %>%
  summarize(total_number_parcels = length(parcel_id))

#################################
#
# Which Zone has the mouse housing 
#
#################################

housing_data %>%
  group_by(zone) %>%
  summarize(n = n()) %>%
  mutate(perc_parcels = prop.table(n)) %>%
  arrange(desc(n))

housing_data %>%
  group_by(zone) %>%
  summarize(n = n()) %>%
  mutate(perc_parcels = prop.table(n)) %>%
  arrange(desc(n)) %>%
  head(10) %>% 
  ggplot(aes(x = reorder(zone, -perc_parcels), y = perc_parcels, fill = zone)) + 
  geom_bar(stat = "identity", col = I("black")) + 
  scale_y_continuous(labels = percent) + 
  theme_bw() + 
  ylab("% of Parcels with Housing Activity") + 
  xlab("") + 
  labs(fill = "Zoning District") + 
  ggtitle("Top 10 Zones by % of Parcels with Housing Activity")

#################################
#
# Look at ownership and analyze
#
#################################

housing_data %>%
  View()


business_strings <- c("LLC","COLLEGE OF THE ATLANTIC", "ASSOCIATES","LODGE","ASSOC",
                      "ARTWAVES","ATKINSON BUILDERS, INC","BAR HARBOR BANK & TRUST",
                      "BAR HARBOR LIMITED PARTNERSHIP","BAR HARBOR MAIN & NEWTON",
                      "BAR HARBOR MASONIC BUILDING ASSOCIATION", "BREAKWATER PROPERTY MANAGEMENT",
                      "DIVERSIFIED REAL ESTATE CORP", "PARTNERSHIP","GREEN ISLAND CORP",
                      "HEWLETT ENTERPRISES","JACKSON LABORATORY", "KAMPGROUNDS OF AMERICA, INC",
                      "KATIES KOTTAGES","KEBO VALLEY GOLF CLUB","MAINE COAST HERITAGE TRUST",
                      "MDI BIO LAB","MDI BIO LABORATORY", "MDI BIOLIGICAL LABORATORY",
                      "MDI BIOLOGICAL LABORATORY", "MT DESERT YACHT YARD, INC",
                      "NATURE CONSERVANCY OF THE PINE","RL WHITE & SON","STRAWBERRY HILL, INC",
                      "THE JACKSON LABORATORY","THE MDI BIO LAB", "TWO HUBS, INC",
                      "WILLOWIND THERAPEUTIC RIDING CENTER INC","YWCA OF MOUNT DESERT ISLAND")

final_analysis_data <- housing_data %>% 
  mutate(business_label = ifelse(str_detect(owner_1, paste(business_strings, collapse = "|")),"Business","Person"))

final_analysis_data %>% 
  group_by(business_label) %>%
  tally()

final_analysis_data %>%
  group_by(business_label) %>%
  tally() %>%
  mutate(perc = prop.table(n)) %>% 
  ggplot(aes(x = business_label, y = n, fill = business_label)) + 
  geom_bar(stat = "identity", col = I("black")) + 
  theme_bw() + 
  ylab("# of Parcels") + 
  xlab("") + 
  ggtitle("Percentage of Residential-use Parcels Owned by Category",
          subtitle = "Classification through Analysis of Primary Owner Name") + 
  theme(legend.position = "None") + 
  geom_text(aes(label= paste0(n, " (", round(perc,2) * 100, "%)")), vjust=-0.5) + 
  ylim(0,2500)


#################################
#
# Which business owns the most? 
#
#################################

final_analysis_data %>%
  filter(business_label == "Business") %>%
  group_by(owner_1) %>%
  tally() %>%
  arrange(desc(n))

final_analysis_data %>%
  filter(business_label == "Business") %>%
  group_by(owner_1) %>%
  tally() %>%
  summarize(total = length(owner_1))

#################################
#
# Which zones have the most business-owned
# parcels?  
#
#################################

final_analysis_data %>%
  filter(business_label == "Business") %>%
  group_by(zone) %>%
  tally() %>%
  arrange(desc(n))

#################################
#
# Which people owns the most? 
#
#################################

final_analysis_data %>%
  filter(business_label == "Person") %>%
  group_by(owner_1) %>%
  tally() %>%
  arrange(desc(n))

final_analysis_data %>%
  filter(business_label == "Person") %>%
  group_by(owner_1) %>%
  tally() %>%
  summarize(total = length(owner_1))

#################################
#
# How many of these people are "Trusts"?
#
#################################

final_analysis_data  %>% 
  filter(business_label == "Person") %>% 
  mutate(trust_label = ifelse(str_detect(tolower(owner_1), "trust"),"Trust","Not Trust")) %>%
  group_by(trust_label) %>%
  tally() %>%
  mutate(perc = prop.table(n))

final_analysis_data %>%
  filter(business_label == "Person") %>%
  mutate(trust_label = ifelse(str_detect(tolower(owner_1), "trust"),"Trust","Not Trust")) %>%
  filter(trust_label == "Trust") %>% 
  group_by(owner_1) %>%
  tally() %>%
  arrange(desc(n))
