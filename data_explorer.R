library(tidyverse)

path = 'data/EDGAR_2024_GHG_booklet_2024.xlsx'

# Read data
df_ghg_totals_by_country <- read_excel(path, sheet = 'GHG_totals_by_country')
df_ghg_by_sector_and_company <- read_excel(path, sheet = 'GHG_by_sector_and_country' )
df_per_gdp_by_country <- read_excel(path, sheet = 'GHG_per_GDP_by_country')
df_per_capita_by_country <- read_excel(path, sheet = 'GHG_per_capita_by_country')
df_lulucf_macroresiongs <- read_excel(path, sheet = 'LULUCF_macroregions')

# GHG_totals_by_country
glimpse(df_ghg_totals_by_country)
glimpse(df_ghg_by_sector_and_company)
glimpse(df_per_gdp_by_country)
glimpse(df_per_capita_by_country)
glimpse(df_lulucf_macroresiongs)
