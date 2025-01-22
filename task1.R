library(tidyverse)
library(readxl)
library(purrr)
library(stringr)
path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'

# Read data
df_ghg_totals_by_country <- read_excel(path, sheet = 'GHG_totals_by_country')

# DATA QUALITY -------------------------------------------------------------------------------
#### Data types checks ####
glimpse(df_ghg_totals_by_country)

#### Title case country name ####
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate (Country = str_to_title(Country))

####  Missing values ####
as.data.frame(map(df_ghg_totals_by_country, ~sum(is.na(.))))

df_na <- df_ghg_totals_by_country %>%
  filter(if_any(everything(), is.na))

# Filter out two rows with missing values
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  filter(if_all(everything(), ~ !is.na(.)))

####  Duplicates ####
duplicate_rows <- df_ghg_totals_by_country %>%
  duplicated(.) | duplicated(df_ghg_totals_by_country, fromLast = TRUE)
sum(duplicate_rows)

#### Factors ####
# Convert character columns to factor
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate(across(where(is.character), factor))


## Select EU 27  ----------------------------------------------------------------------

EU27 <- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Estonia',
          'Finland', 'France And Monaco', 'Germany', 'Greece', 'Hungary', 'Ireland',
          'Italy, San Marino And The Holy See', 'Latvia', 'Lithuania', 'Luxembourg',
          'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania','Slovakia', 'Slovenia',
          'Spain And Andorra', 'Sweden' )

df_ghg_totals_by_country_EU27 <- df_ghg_totals_by_country %>%
  filter(Country %in% EU27) %>%
  arrange(Country)

df_ghg_totals_by_country %>%
  filter(grepl("IT", `EDGAR Country Code`)) %>%
  select(Country)

df_ghg_totals_by_country %>%
  filter(grepl("SP", `EDGAR Country Code`)) %>%
  select(Country)

df_ghg_totals_by_country %>%
  filter(grepl("FR", `EDGAR Country Code`)) %>%
  select(Country)

#### Add EU27 Flag ####
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate(IS_EU27 = ifelse(Country %in% EU27, 1, 0))

# Check correct Flag assignment
checkEU27 <- df_ghg_totals_by_country %>%
  select (Country, IS_EU27) %>%
  filter (Country %in% EU27)
print(checkEU27, n = Inf)


#### Pivot longer ####
df_ghg_totals_by_country_long <- df_ghg_totals_by_country %>%
  pivot_longer(cols = `1970`: `2023`, names_to = 'YEAR', values_to = 'GHG_EMM')

#### Prep table for visualization ####

df_chart1 <- df_ghg_totals_by_country_long %>%
  group_by(YEAR, IS_EU27) %>%
  summarise(AVG_GHG = mean (GHG_EMM), .groups = 'drop')

#### CHART 1 --------------------------------------------------
## Linechart with two lines

ggplot(df_chart1, aes(x = YEAR, y = AVG_GHG, color = factor(IS_EU27), group = IS_EU27)) +
  geom_line()

