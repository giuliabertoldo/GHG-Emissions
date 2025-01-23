# Loadd packages

# Read data
path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'
df_per_capita_by_country <- read_excel(path, sheet = 'GHG_per_capita_by_country')
df_class <- read_excel('data/CLASS.xlsx')
# DATA QUALITY -------------------------------------------------------------------------------
#### Data types checks ####
glimpse(df_per_capita_by_country)

####  Missing values ####
as.data.frame(map(df_per_capita_by_country, ~sum(is.na(.))))

df_na <- df_per_capita_by_country %>%
  filter(if_any(everything(), is.na))

# Filter out two rows with missing values
df_per_capita_by_country <- df_per_capita_by_country %>%
  filter(if_all(everything(), ~ !is.na(.)))
