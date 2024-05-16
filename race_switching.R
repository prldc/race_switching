library(tidyverse)
library(data.table)
library(ggplot2)
library(tidyverse)

candidates <- read_csv("candidates.csv")

candidates <- candidates %>%
  arrange(NR_CPF_CANDIDATO, DT_ELEICAO)

# finds race in previous election
candidates <- candidates %>%
  group_by(NR_CPF_CANDIDATO) %>%
  mutate(prev_CD_COR_RACA = lag(CD_COR_RACA))


## race switching

setDT(candidates)
candidates <- candidates[order(NR_CPF_CANDIDATO, DT_ELEICAO)]

candidates[, `:=` (
  BRANCA_PARDA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 1 & CD_COR_RACA == 3, 1, 0),
  PARDA_BRANCA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 3 & CD_COR_RACA == 1, 1, 0),
  PRETA_PARDA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 2 & CD_COR_RACA == 3, 1, 0),
  PARDA_PRETA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 3 & CD_COR_RACA == 2, 1, 0),
  BRANCA_PRETA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 1 & CD_COR_RACA == 2, 1, 0),
  PRETA_BRANCA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 2 & CD_COR_RACA == 1, 1, 0),
  BRANCA_AMARELA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 1 & CD_COR_RACA == 4, 1, 0),
  AMARELA_BRANCA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 4 & CD_COR_RACA == 1, 1, 0),
  PRETA_AMARELA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 2 & CD_COR_RACA == 4, 1, 0),
  AMARELA_PRETA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 4 & CD_COR_RACA == 2, 1, 0),
  PARDA_AMARELA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 3 & CD_COR_RACA == 4, 1, 0),
  AMARELA_PARDA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 4 & CD_COR_RACA == 3, 1, 0),
  NE_BRANCA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == -3 & CD_COR_RACA == 1, 1, 0),
  BRANCA_NE = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 1 & CD_COR_RACA == -3, 1, 0),
  NE_PRETA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == -3 & CD_COR_RACA == 2, 1, 0),
  PRETA_NE = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 2 & CD_COR_RACA == -3, 1, 0),
  NE_PARDA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == -3 & CD_COR_RACA == 3, 1, 0),
  PARDA_NE = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 3 & CD_COR_RACA == -3, 1, 0),
  NE_AMARELA = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == -3 & CD_COR_RACA == 4, 1, 0),
  AMARELA_NE = fifelse(!is.na(prev_CD_COR_RACA) & !is.na(CD_COR_RACA) & prev_CD_COR_RACA == 4 & CD_COR_RACA == -3, 1, 0)
)]

  
  # Aggregate the data by year and sum the transitions
  transition_counts_by_year <- candidates %>%
  group_by(ANO_ELEICAO) %>%
  summarise(
    PRETA_BRANCA = sum(PRETA_BRANCA),
    BRANCA_PRETA = sum(BRANCA_PRETA),
    BRANCA_PARDA = sum(BRANCA_PARDA),
    PARDA_BRANCA = sum(PARDA_BRANCA),
    PARDA_PRETA = sum(PARDA_PRETA),
    PRETA_PARDA = sum(PRETA_PARDA)
  ) %>%
  pivot_longer(cols = c(PRETA_BRANCA, BRANCA_PRETA, BRANCA_PARDA, PARDA_BRANCA, PARDA_PRETA, PRETA_PARDA),
               names_to = "Transition", values_to = "Count")

ggplot(transition_counts_by_year, aes(x = ANO_ELEICAO, y = Count, color = Transition, group = Transition)) +
  geom_line() +
  geom_point() +
  labs(title = "Mudanças de Raça por Ano",
       x = "Ano",
       y = "Número de mudanças",
       color = "Tipo de mudança") +
  theme_minimal()

transition_counts_by_year_cargo <- candidates %>%
  group_by(ANO_ELEICAO, DS_CARGO) %>%
  summarise(
    PRETA_BRANCA = sum(PRETA_BRANCA, na.rm = TRUE),
    BRANCA_PRETA = sum(BRANCA_PRETA, na.rm = TRUE),
    BRANCA_PARDA = sum(BRANCA_PARDA, na.rm = TRUE),
    PARDA_BRANCA = sum(PARDA_BRANCA, na.rm = TRUE),
    PARDA_PRETA = sum(PARDA_PRETA, na.rm = TRUE),
    PRETA_PARDA = sum(PRETA_PARDA, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(PRETA_BRANCA, BRANCA_PRETA, BRANCA_PARDA, PARDA_BRANCA, PARDA_PRETA, PRETA_PARDA),
               names_to = "Transition", values_to = "Count")

ggplot(transition_counts_by_year_cargo, aes(x = ANO_ELEICAO, y = Count, color = Transition, group = Transition)) +
  geom_line() +
  geom_point() +
  labs(title = "Racial Transition Changes by Year and Position",
       x = "Election Year",
       y = "Number of Changes",
       color = "Type of Transition") +
  facet_wrap(~ DS_CARGO) +
  theme_minimal()

transition_counts_by_year_vereador <- candidates %>%
  filter(DS_CARGO == "VEREADOR") %>%
  group_by(ANO_ELEICAO, DS_CARGO) %>%
  summarise(
    PRETA_BRANCA = sum(PRETA_BRANCA, na.rm = TRUE),
    BRANCA_PRETA = sum(BRANCA_PRETA, na.rm = TRUE),
    BRANCA_PARDA = sum(BRANCA_PARDA, na.rm = TRUE),
    PARDA_BRANCA = sum(PARDA_BRANCA, na.rm = TRUE),
    PARDA_PRETA = sum(PARDA_PRETA, na.rm = TRUE),
    PRETA_PARDA = sum(PRETA_PARDA, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(PRETA_BRANCA, BRANCA_PRETA, BRANCA_PARDA, PARDA_BRANCA, PARDA_PRETA, PRETA_PARDA, NE_BRANCA, NE_PARDA, NE_PRETA),
               names_to = "Transition", values_to = "Count")

ggplot(transition_counts_by_year_vereador, aes(x = ANO_ELEICAO, y = Count, color = Transition, group = Transition)) +
  geom_line() +
  geom_point() +
  labs(title = "Racial Transition Changes by Year for Vereador",
       x = "Election Year",
       y = "Number of Changes",
       color = "Type of Transition") +
  theme_minimal()

## logged
  
transition_counts_by_year <- candidates %>%
  group_by(ANO_ELEICAO) %>%
  summarise(
    PRETA_BRANCA = log(sum(PRETA_BRANCA))+1,
    BRANCA_PRETA = log(sum(BRANCA_PRETA))+1,
    BRANCA_PARDA = log(sum(BRANCA_PARDA))+1,
    PARDA_BRANCA = log(sum(PARDA_BRANCA))+1,
    PARDA_PRETA = log(sum(PARDA_PRETA))+1,
    PRETA_PARDA = log(sum(PRETA_PARDA))+1
  ) %>%
  pivot_longer(cols = c(PRETA_BRANCA, BRANCA_PRETA, BRANCA_PARDA, PARDA_BRANCA, PARDA_PRETA, PRETA_PARDA),
               names_to = "Transition", values_to = "Count")

ggplot(transition_counts_by_year, aes(x = ANO_ELEICAO, y = Count, color = Transition, group = Transition)) +
  geom_line() +
  geom_point() +
  labs(title = "Mudanças de Raça por Ano",
       x = "Ano",
       y = "Número de mudanças (Logged)",
       color = "Tipo de mudança") +
  theme_minimal()

transition_counts_by_year_cargo <- candidates %>%
  group_by(ANO_ELEICAO, DS_CARGO) %>%
  summarise(
    PRETA_BRANCA = log(sum(PRETA_BRANCA, na.rm = TRUE))+1,
    BRANCA_PRETA = log(sum(BRANCA_PRETA, na.rm = TRUE))+1,
    BRANCA_PARDA = log(sum(BRANCA_PARDA, na.rm = TRUE))+1,
    PARDA_BRANCA = log(sum(PARDA_BRANCA, na.rm = TRUE))+1,
    PARDA_PRETA = log(sum(PARDA_PRETA, na.rm = TRUE))+1,
    PRETA_PARDA = log(sum(PRETA_PARDA, na.rm = TRUE))+1
  ) %>%
  pivot_longer(cols = c(PRETA_BRANCA, BRANCA_PRETA, BRANCA_PARDA, PARDA_BRANCA, PARDA_PRETA, PRETA_PARDA),
               names_to = "Transition", values_to = "Count")

ggplot(transition_counts_by_year_cargo, aes(x = ANO_ELEICAO, y = Count, color = Transition, group = Transition)) +
  geom_line() +
  geom_point() +
  labs(title = "Racial Transition Changes by Year and Position",
       x = "Election Year",
       y = "Number of Changes (Logged)",
       color = "Type of Transition") +
  facet_wrap(~ DS_CARGO) +
  theme_minimal()

transition_counts_by_year_vereador <- candidates %>%
  filter(DS_CARGO == "VEREADOR") %>%
  group_by(ANO_ELEICAO, DS_CARGO) %>%
  summarise(
    PRETA_BRANCA = log(sum(PRETA_BRANCA, na.rm = TRUE))+1,
    BRANCA_PRETA = log(sum(BRANCA_PRETA, na.rm = TRUE))+1,
    BRANCA_PARDA = log(sum(BRANCA_PARDA, na.rm = TRUE))+1,
    PARDA_BRANCA = log(sum(PARDA_BRANCA, na.rm = TRUE))+1,
    PARDA_PRETA = log(sum(PARDA_PRETA, na.rm = TRUE))+1,
    PRETA_PARDA = log(sum(PRETA_PARDA, na.rm = TRUE))+1
  ) %>%
  pivot_longer(cols = c(PRETA_BRANCA, BRANCA_PRETA, BRANCA_PARDA, PARDA_BRANCA, PARDA_PRETA, PRETA_PARDA),
               names_to = "Transition", values_to = "Count")

ggplot(transition_counts_by_year_vereador, aes(x = ANO_ELEICAO, y = Count, color = Transition, group = Transition)) +
  geom_line() +
  geom_point() +
  labs(title = "Racial Transition Changes by Year for Vereador",
       x = "Election Year",
       y = "Number of Changes (Logged)",
       color = "Type of Transition") +
  theme_minimal()


filtered_candidates <- candidates %>%
  arrange(NR_CPF_CANDIDATO) %>%
  group_by(NR_CPF_CANDIDATO) %>%
  filter(any(BRANCA_PARDA == 1 | PARDA_BRANCA == 1))

# View the first few rows of the filtered dataset
head(filtered_candidates)

library("openxlsx")

write.xlsx(filtered_candidates, file = "race_switchers.xlsx")
