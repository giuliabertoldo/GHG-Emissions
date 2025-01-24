# Load packages
library(tidyverse)
library(readxl)
library(RColorBrewer)

# Read data
path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'
df_per_capita_by_country <- read_excel(path, sheet = 'GHG_per_capita_by_country')
df_class <- read_excel('data/CLASS.xlsx')
# DATA QUALITY -------------------------------------------------------------------------------
#### Data types checks ####
glimpse(df_per_capita_by_country)
glimpse(df_class)

#### Upper case country name ####
df_per_capita_by_country <- df_per_capita_by_country %>%
  mutate (Country = str_to_upper(Country))
df_per_capita_by_country$Country

df_class <- df_class %>%
  mutate (Economy = str_to_upper(Economy))
df_class$Economy

####  Missing values ####
# GHG data
as.data.frame(map(df_per_capita_by_country, ~sum(is.na(.))))
df_na <- df_per_capita_by_country %>%
  filter(if_any(everything(), is.na))
# Filter out two rows with missing values
df_per_capita_by_country <- df_per_capita_by_country %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Income data
as.data.frame(map(df_class, ~sum(is.na(.))))
df_na_ingro <- df_class %>%
  filter(is.na(Economy) == TRUE)
df_na_ingro
# Filter out row with missing values
df_class <- df_class %>%
  filter(is.na(Economy) != TRUE)
# Venezuela is not classified into an income group + other regional areas
null_income <- df_class %>%
  filter(is.na(`Income group`) == TRUE)
print(null_income, n = Inf)

####  Duplicates ####
duplicate_rows <- df_per_capita_by_country %>%
  duplicated(.) | duplicated(df_per_capita_by_country, fromLast = TRUE)
sum(duplicate_rows)

#### Factors ####
# Convert character columns to factor
df_per_capita_by_country <- df_per_capita_by_country %>%
  mutate(across(where(is.character), factor))

#### Left Outer join ####
df_per_capita_by_country_with_income <- left_join(df_per_capita_by_country,
                                                  df_class %>%
                                                    select(Code,`Income group`),
                                                  by = c('EDGAR Country Code' = 'Code'))

# Check join results
glimpse(df_per_capita_by_country_with_income)

# Apart from Venezuela, EU27, GLOBAL, other 12 countries have not been assigned an income group
df_per_capita_by_country_with_income %>%
  filter(is.na(`Income group`)) %>%
  select(`EDGAR Country Code`, 'Country', `Income group` )

# Drop rows without income group
df_per_capita_by_country_with_income <- df_per_capita_by_country_with_income %>%
  filter(is.na(`Income group`) == FALSE)

#### Pivot longer ####
df_per_capita_by_income_long <- df_per_capita_by_country_with_income %>%
  pivot_longer(cols = `1970`: `2023`, names_to = 'YEAR', values_to = 'GHG_EMM')

glimpse(df_per_capita_by_income_long)

#### Aggregate ####
df_per_capita_by_income_long_gr <- df_per_capita_by_income_long %>%
  group_by(`Income group`, YEAR) %>%
  summarize(TOT_GHG_EMM = sum(GHG_EMM), .groups = "drop")
df_per_capita_by_income_long_gr

#### Income groups as factos ####
df_per_capita_by_income_long_gr$`Income group` <- factor(df_per_capita_by_income_long_gr$`Income group`,
                                                         levels = c("High income",
                                                                    "Upper middle income",
                                                                    "Lower middle income",
                                                                    "Low income")
)

glimpse(df_per_capita_by_income_long_gr)
#### CHART 2 --------------------------------------------------
## Choose colors
pl3 <- brewer.pal(11, 'PRGn')
custom_color <- c("#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837")

## Stacked bar chart
ggplot(df_per_capita_by_income_long_gr, aes(x = YEAR,
                                            y = TOT_GHG_EMM,
                                            fill = factor(`Income group`),
                                            group = `Income group`)) +
  geom_bar(stat = 'identity', position = 'stack',color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("High income" =  "#1B7837",
                               "Upper middle income" = "#5AAE61",
                               "Lower middle income" = "#A6DBA0",
                               "Low income" ="#D9F0D3")) +
  labs(
    title = 'Country (Income Group) Contribution to GHG Emissions',
    x = 'Year',
    y = 'GHG (t CO2eq/cap/yr)',
    fill = 'Income Group'
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.text = element_text(size = 8),          # Smaller text for legend items
    legend.title = element_text(size = 10),       # Smaller text for legend title
    legend.key.size = unit(0.3, "cm"),            # Reduce legend key size
    legend.spacing.y = unit(0.2, "cm")           # Adjust vertical spacing between items
  ) +
  scale_x_discrete(breaks = df_per_capita_by_income_long_gr$YEAR[seq(1, length(df_per_capita_by_income_long_gr$YEAR), by = 2)])


