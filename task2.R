# Loadd packages

# Read data
path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'
df_per_capita_by_country <- read_excel(path, sheet = 'GHG_per_capita_by_country')
df_class <- read_excel('data/CLASS.xlsx')
# DATA QUALITY -------------------------------------------------------------------------------
#### Data types checks ####
glimpse(df_per_capita_by_country)
glimpse(df_class)

#### Title case country name ####
df_per_capita_by_country <- df_per_capita_by_country %>%
  mutate (Country = str_to_title(Country))

df_class <- df_class %>%
  mutate (Economy = str_to_title(Economy))

####  Missing values ####
as.data.frame(map(df_per_capita_by_country, ~sum(is.na(.))))

df_na <- df_per_capita_by_country %>%
  filter(if_any(everything(), is.na))

# Filter out two rows with missing values
df_per_capita_by_country <- df_per_capita_by_country %>%
  filter(if_all(everything(), ~ !is.na(.)))

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
                                                    select(Code, `Income group`),
                                                  by = c('EDGAR Country Code' = 'Code'))
# Check join results
glimpse(df_per_capita_by_country_with_income)

df_per_capita_by_country_with_income %>%
  filter(is.na(`Income group`)) %>%
  select(`EDGAR Country Code`, 'Country', `Income group` )


df_class %>%
  filter(Economy == 'Global Total')
