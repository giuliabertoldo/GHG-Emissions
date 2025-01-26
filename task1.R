# Load packages
library(tidyverse)
library(readxl)
library(purrr)
library(stringr)

# Read data
path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'
df_ghg_totals_by_country <- read_excel(path, sheet = 'GHG_totals_by_country')

# DATA QUALITY -------------------------------------------------------------------------------
#### Data types checks ####
glimpse(df_ghg_totals_by_country)

# `EDGAR Country Code`
df_ghg_totals_by_country$`EDGAR Country Code`
df_ghg_totals_by_country$Country

#### Upper case country name ####
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate (Country = str_to_upper(Country))

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

#### Factors####
# Convert character columns to factor
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate(across(where(is.character), factor))

#### EUROZONE FLAG, EU27 FLAG, GLOBAL FLAG ####
# eurozone
eurozone <- c('AUSTRIA', 'BELGIUM', 'CROATIA', 'CYPRUS', 'ESTONIA', 'FINLAND',
              'FRANCE AND MONACO', 'GERMANY', 'GREECE', 'IRELAND',
              'ITALY, SAN MARINO AND THE HOLY SEE', 'LATVIA', 'LITHUANIA',
              'LUXEMBOURG', 'MALTA', 'NETHERLANDS', 'PORTUGAL', 'SLOVAKIA',
              'SLOVENIA', 'SPAIN AND ANDORRA')

# Keep only relevant rows
df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  filter((Country %in% c('EU27', 'GLOBAL TOTAL')) | (Country %in% eurozone))

df_ghg_totals_by_country <- df_ghg_totals_by_country %>%
  mutate(GEO_TYPE = case_when(
    Country == 'EU27' ~ 'EU27',
    Country == 'GLOBAL TOTAL' ~ 'GLOBAL',
    Country %in% eurozone ~ 'EUROZONE'
  ))

check_eurozone <- df_ghg_totals_by_country %>%
  select(Country, GEO_TYPE)
print(check_eurozone, n = Inf )

#### Pivot longer ####
df_ghg_totals_by_country_long <- df_ghg_totals_by_country %>%
  pivot_longer(cols = `1970`: `2023`, names_to = 'YEAR', values_to = 'GHG_EMM')

glimpse(df_ghg_totals_by_country_long)

#### Select column ####
df_ghg_totals_by_country_long <- df_ghg_totals_by_country_long %>%
  select(GEO_TYPE, YEAR, GHG_EMM)

#### Aggregate ####
df_ghg_totals_by_country_group <- df_ghg_totals_by_country_long %>%
  group_by(GEO_TYPE, YEAR) %>%
  summarize(TOT_GHG_EMM = sum(GHG_EMM), .groups = "drop")
glimpse(df_ghg_totals_by_country_group)

#### Geography Type as factor
df_ghg_totals_by_country_group$GEO_TYPE <- factor(df_ghg_totals_by_country_group$GEO_TYPE,
                                                  levels = c('GLOBAL',
                                                             'EU27',
                                                             'EUROZONE'),
                                                  labels = c("Worldwide",
                                                             "European Union",
                                                             "Eurozone")
)

#### CHART 1 --------------------------------------------------

custom_colors <- c("#40004B", "#1B7837", "#A6DBA0")

ggplot(df_ghg_totals_by_country_group, aes(x = YEAR, y = TOT_GHG_EMM, color = factor(GEO_TYPE), fill = factor(GEO_TYPE), group = GEO_TYPE)) +
  geom_line() +
  geom_ribbon(alpha = 0.4, aes(ymin = 0, ymax = TOT_GHG_EMM))+
  labs(
    title = 'Evolution of GHG Emissions',
    x = 'Year',
    y = 'GHG (Mt CO2eq/yr)',
    color = 'Geography'
  ) +
  scale_color_manual(values = custom_colors) +  # Use your custom color palette
  scale_fill_manual(values = custom_colors) +   # Ensure fill uses the same colors as the lines
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.position = "bottom"
  ) +
  guides(fill = "none")  +
  scale_x_discrete(breaks = df_ghg_totals_by_country_group$YEAR[seq(1, length(df_ghg_totals_by_country_group$YEAR), by = 2)]) +
  scale_y_continuous(breaks = seq(0, max(df_ghg_totals_by_country_group$TOT_GHG_EMM), by = 5000))

## Not in RMarkdown
max(df_ghg_totals_by_country_group$TOT_GHG_EMM)
min(df_ghg_totals_by_country_group$TOT_GHG_EMM)

glimpse(df_ghg_totals_by_country_group)
