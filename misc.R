## share of each race for each election year

library(tidyverse)
library(data.table)

candidates <- read_csv("candidates.csv")

data_filtered <- candidates %>%
  select(ANO_ELEICAO, CD_COR_RACA)

race_counts_by_year <- data_filtered %>%
  group_by(ANO_ELEICAO, CD_COR_RACA) %>%
  summarise(Count = n(), .groups = 'drop')

total_by_year <- race_counts_by_year %>%
  group_by(ANO_ELEICAO) %>%
  summarise(Total = sum(Count), .groups = 'drop')

race_share_by_year <- race_counts_by_year %>%
  left_join(total_by_year, by = "ANO_ELEICAO") %>%
  mutate(Share = Count / Total * 100)

race_share_by_year_wide <- race_share_by_year %>%
  pivot_wider(
    names_from = CD_COR_RACA, 
    values_from = Share, 
    values_fill = list(Share = 0)
  ) %>%
  group_by(ANO_ELEICAO) %>%
  summarise(across(everything(), sum, .names = "{.col}")) %>%
  arrange(ANO_ELEICAO)


unique_race_codes <- candidates %>%
  select(CD_COR_RACA, DS_COR_RACA) %>%
  distinct(CD_COR_RACA, .keep_all = TRUE)

# Create a named vector for renaming
race_names <- setNames(as.character(unique_race_codes$DS_COR_RACA), as.character(unique_race_codes$CD_COR_RACA))

# Prepare to rename columns in race_share_by_year_wide by creating new names vector
new_column_names <- sapply(colnames(race_share_by_year_wide), function(x) {
  if (x %in% names(race_names)) race_names[x] else x
})

# Apply the new names to the dataframe
colnames(race_share_by_year_wide) <- new_column_names

# Add a new column 'TOTAL' that sums up the values of the race description columns
race_share_by_year_wide <- race_share_by_year_wide %>%
  mutate(TOTAL = rowSums(select(., -c("ANO_ELEICAO", "Count", "Total"))))

# Print the updated table
print(race_share_by_year_wide)
