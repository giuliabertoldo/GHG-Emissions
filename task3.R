# Load packages
library(tidyverse)
library(readxl)
library(RColorBrewer)
# Only for here
library(countrycode)
library(maps)
library(patchwork)


# Read data
path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'
df_ghg_totals_by_country <- read_excel(path, sheet = 'GHG_totals_by_country')


# DATA QUALITY  (common with task 1 )-------------------------------------------------------------------------------
#### Data types checks ####
glimpse(df_ghg_totals_by_country)

# `EDGAR Country Code`
df_ghg_totals_by_country$`EDGAR Country Code`
df_ghg_totals_by_country$Country

#### Upper case country name ####
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate (Country = str_to_upper(Country))

####  Missing values ####
df_na <- df_ghg_totals_by_country %>%
  filter(if_any(everything(), is.na))
df_na

# Filter out rows with missing values
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  filter(if_all(everything(), ~ !is.na(.)))

####  Duplicates ####
duplicate_rows <- df_ghg_totals_by_country %>%
  duplicated(.) | duplicated(df_ghg_totals_by_country, fromLast = TRUE)
sum(duplicate_rows)

#### Factors####
# Convert character columns to factor
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate(across(where(is.character), factor))

#### Exclude EU27, GLOBAL TOTAL, INTERNATIONAL AVIATION, INTERNATIONAL SHIPPING ####
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  filter(!(Country %in% c('EU27', 'GLOBAL TOTAL', 'INTERNATIONAL AVIATION', 'INTERNATIONAL SHIPPING' )))

#### Pivot longer ####
df_ghg_totals_by_country_long <- df_ghg_totals_by_country %>%
  pivot_longer(cols = `1970`: `2023`, names_to = 'YEAR', values_to = 'GHG_EMM')
glimpse(df_ghg_totals_by_country_long)

#### Group by Country and sum across years ####
df_ghg_totals_by_country_long_gr <- df_ghg_totals_by_country_long %>%
  group_by(Country, `EDGAR Country Code`) %>%
  summarize(TOT_GHG_EMM = sum(GHG_EMM), .groups = "drop")
glimpse(df_ghg_totals_by_country_long_gr)

#### Add percentage total ####
grand_tot_ghg <- sum(df_ghg_totals_by_country_long_gr$TOT_GHG_EMM)

df_ghg_perc_by_country <- df_ghg_totals_by_country_long_gr %>%
  mutate(PER_GHG_EMM = round((TOT_GHG_EMM/grand_tot_ghg)*100, 4))
glimpse(df_ghg_perc_by_country)

#### Categorize percentage ####
min(df_ghg_perc_by_country$PER_GHG_EMM)
max(df_ghg_perc_by_country$PER_GHG_EMM)

df_ghg_perc_by_country$CAT_PER_GHG_EMM <- cut(
  df_ghg_perc_by_country$PER_GHG_EMM,
  breaks = seq(0, 20, by = 2.5),       # Define the breakpoints
  include.lowest = TRUE,               # Include 0 in the first bin
  right = FALSE,                       # Use left-closed intervals [a, b)
  labels = paste0("(", seq(0, 17.5, by = 2.5), "%-", seq(2.5, 20, by = 2.5), "%)") # Custom labels
)
glimpse(df_ghg_perc_by_country)

#### Add continent ####
codes <- countrycode::codelist
country_continet <- codes %>%
  select(iso3c, continent)

# Missing values
df_na <- country_continet %>%
  filter(if_any(everything(), is.na))
df_na

# Filter rows with missing values
country_continent <- country_continet %>%
  filter(if_all(everything(), ~ !is.na(.)))

glimpse(country_continent)

# Left join
df_ghg_perc_by_country_cont <- left_join(df_ghg_perc_by_country,
                                         country_continent,
                                         by = c('EDGAR Country Code' = 'iso3c' ))
glimpse(df_ghg_perc_by_country_cont)

# Missing values
df_ghg_perc_by_country_cont %>%
  filter(is.na(continent) == TRUE)

df_ghg_perc_by_country_cont$continent[df_ghg_perc_by_country_cont$`EDGAR Country Code` == 'ANT'] <- "Americas"
df_ghg_perc_by_country_cont$continent[df_ghg_perc_by_country_cont$`EDGAR Country Code` == 'SCG'] <- "Europe"

glimpse(df_ghg_perc_by_country_cont)

# Table with continent data
continent <- df_ghg_perc_by_country_cont %>%
  select(continent, PER_GHG_EMM) %>%
  group_by(continent)%>%
  summarize(CONT_PER_GHG_EMM = sum(PER_GHG_EMM)) %>%
  arrange(desc(CONT_PER_GHG_EMM))

#### Worlds map data Prep ####
world <- map_data("world")

# Upper case country name
world <- world %>%
  mutate(region = str_to_upper(region))

# Left join
df_ghg_perc_by_country_world <- left_join(world,
                                          df_ghg_perc_by_country_cont,
                                          by = c('region' = 'Country' ))
glimpse(df_ghg_perc_by_country_world)

#Find unmapped countries
codes_in <- df_ghg_perc_by_country_world %>%
  select (`EDGAR Country Code`) %>%
  distinct()

codes_out <- df_ghg_perc_by_country_cont %>%
  select(`EDGAR Country Code`, 'Country') %>%
  distinct() %>%
  filter(!(`EDGAR Country Code` %in% codes_in$`EDGAR Country Code`))
print(codes_out, n = Inf)

# Complete list of regions
regions <- world %>%
  select (region) %>%
  distinct() %>%
  arrange(region)

# Align dataset name, no corrispondence found for GIBRALTAR, HONG KONG, MACAO, CONGO

df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'ANTIGUA AND BARBUDA'] <- 'ANTIGUA'
# world$region[world$region == 'ANTIGUA'] <- 'ANTIGUA AND BARBUDA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'BRITISH VIRGIN ISLANDS'] <- 'VIRGIN ISLANDS'
# world$region[world$region == 'VIRGIN ISLANDS'] <- 'BRITISH VIRGIN ISLANDS'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'CABO VERDE'] <- 'CAPE VERDE'
# world$region[world$region == 'CAPE VERDE'] <- 'CABO VERDE'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'CÔTE D’IVOIRE'] <- 'IVORY COAST'
# world$region[world$region == 'IVORY COAST'] <- 'CÔTE D’IVOIRE'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'CURAÇAO'] <- 'CURACAO'
# world$region[world$region == 'CURACAO'] <- 'CURAÇAO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'CZECHIA'] <- 'CZECH REPUBLIC'
# world$region[world$region == 'CZECH REPUBLIC'] <- 'CZECHIA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'ESWATINI'] <- 'SWAZILAND'
# world$region[world$region == 'SWAZILAND'] <- 'ESWATINI'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'FAROES'] <- 'FAROE ISLANDS'
# world$region[world$region == 'FAROE ISLANDS'] <- 'FAROES'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'FRANCE AND MONACO'] <- 'FRANCE'
# world$region[world$region == 'FRANCE'] <- 'FRANCE AND MONACO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'FRANCE AND MONACO'] <- 'MONACO'
# world$region[world$region == 'MONACO'] <- 'FRANCE AND MONACO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'ISRAEL AND PALESTINE, STATE OF'] <- 'ISRAEL'
# world$region[world$region == 'ISRAEL'] <- 'ISRAEL AND PALESTINE, STATE OF'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'ITALY, SAN MARINO AND THE HOLY SEE'] <- 'ITALY'
# world$region[world$region == 'ITALY'] <- 'ITALY, SAN MARINO AND THE HOLY SEE'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'MYANMAR/BURMA'] <- 'MYANMAR'
# world$region[world$region == 'MYANMAR'] <- 'MYANMAR/BURMA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'RÉUNION'] <- 'REUNION'
# world$region[world$region == 'REUNION'] <- 'RÉUNION'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'AINT HELENA, ASCENSION AND TRISTAN DA CUNHA'] <- 'SAINT HELENA'
# world$region[world$region == 'SAINT HELENA'] <- 'SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SAINT KITTS AND NEVIS'] <- 'SAINT KITTS'
# world$region[world$region == 'SAINT KITTS'] <- 'SAINT KITTS AND NEVIS'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SAINT VINCENT AND THE GRENADINES'] <- 'SAINT VINCENT'
# world$region[world$region == 'SAINT VINCENT'] <- 'SAINT VINCENT AND THE GRENADINES'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SÃO TOMÉ AND PRÍNCIPE'] <- 'SAO TOME AND PRINCIPE'
# world$region[world$region == 'SAO TOME AND PRINCIPE'] <- 'SÃO TOMÉ AND PRÍNCIPE'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SERBIA AND MONTENEGRO'] <- 'SERBIA'
# world$region[world$region == 'SERBIA'] <- 'SERBIA AND MONTENEGRO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SERBIA AND MONTENEGRO'] <- 'MONTENEGRO'
# world$region[world$region == 'MONTENEGRO'] <- 'SERBIA AND MONTENEGRO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SPAIN AND ANDORRA'] <- 'SPAIN'
# world$region[world$region == 'SPAIN'] <- 'SPAIN AND ANDORRA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SPAIN AND ANDORRA'] <- 'ANDORRA'
# world$region[world$region == 'ANDORRA'] <- 'SPAIN AND ANDORRA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SUDAN AND SOUTH SUDAN'] <- 'SUDAN'
# world$region[world$region == 'SUDAN'] <- 'SUDAN AND SOUTH SUDAN'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SWITZERLAND AND LIECHTENSTEIN'] <- 'SWITZERLAND'
# world$region[world$region == 'SWITZERLAND'] <- 'SWITZERLAND AND LIECHTENSTEIN'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'SWITZERLAND AND LIECHTENSTEIN'] <- 'LIECHTENSTEIN'
# world$region[world$region == 'LIECHTENSTEIN'] <- 'SWITZERLAND AND LIECHTENSTEIN'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'THE GAMBIA'] <- 'GAMBIA'
# world$region[world$region == 'GAMBIA'] <- 'THE GAMBIA'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'TRINIDAD AND TOBAGO'] <- 'TRINIDAD'
# world$region[world$region == 'TRINIDAD'] <- 'TRINIDAD AND TOBAGO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'TRINIDAD AND TOBAGO'] <- 'TOBAGO'
# world$region[world$region == 'TOBAGO'] <- 'TRINIDAD AND TOBAGO'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'TÜRKIYE'] <- 'TURKEY'
# world$region[world$region == 'TURKEY'] <- 'TÜRKIYE'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'UNITED STATES'] <- 'USA'
# world$region[world$region == 'USA'] <- 'UNITED STATES'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'VIET NAM'] <- 'VIETNAM'
# world$region[world$region == 'VIETNAM'] <- 'VIET NAM'
df_ghg_perc_by_country_cont$Country[df_ghg_perc_by_country_cont$Country == 'UNITED KINGDOM'] <- 'UK'
# world$region[world$region == 'UK'] <- 'UNITED KINGDOM'

# Perform new join
df_ghg_perc_by_country_world <- left_join(world,
                                          df_ghg_perc_by_country_cont,
                                          by = c('region' = 'Country' ))

# VISUALIZATION WORLD MAP --------------------------------------
map_world <- ggplot() +
  theme(legend.position="bottom") +
  geom_map(data=df_ghg_perc_by_country_world,
           map=world,
           aes(map_id=region, x=long, y=lat, fill=CAT_PER_GHG_EMM), color = "black", size = 0.1) +
  scale_fill_brewer(palette = "Reds",
                    name = "GHG Emissions Contribution")+
  #scale_fill_gradient(low = "white", high = "darkred", name = "GHG Emissions") +
  coord_equal() +
  theme_light() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    legend.position = "bottom"     # Optional: Adjust legend position
  )

# For text
df_ghg_perc_by_country_cont %>%
  select(Country, PER_GHG_EMM, CAT_PER_GHG_EMM) %>%
  arrange(desc(PER_GHG_EMM)) %>%
  group_by(CAT_PER_GHG_EMM)

# CONTINENTS BARCHARTS -----------------------------------------------------------------------------
continent_bar <- ggplot(data = continent, aes(x = reorder(continent, CONT_PER_GHG_EMM), y = CONT_PER_GHG_EMM, fill = continent)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c(
    "Asia" = "#cb1818",
    "Americas" = "#ef3b2c",
    "Europe" = "#fb6a4a",
    "Africa" = "#fc9272",
    "Oceania" = "#fcbba1"
  )) +
  labs(
    x = 'Continent',
    y = 'Percentage Contribution to Worldwide GHG'
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, hjust = 1)
  )

# COMBINE GRAPHS -----------------------------------------------------------------------------
# Combine the map and bar chart using patchwork
combined_plot <- map_world + continent_bar + plot_layout(ncol = 2, widths = c(2, 1))

# Display the combined plot
combined_plot


