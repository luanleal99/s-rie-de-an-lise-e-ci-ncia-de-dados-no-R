library(here)
library(dplyr)
library(tidyr)

penguins_clean <- read.csv(here("data", "processed", "penguins_clean.csv"))

# 1. Quantas observações existem para cada espécie?
qtd_especie <- penguins_clean |>
  count(species)
print(qtd_especie)

# 2. Qual é a massa corporal média de cada espécie? (em gramas)
massa_media <- penguins_clean |>
  group_by(species) |>
  summarise(massa_media_g = mean(body_mass_g))
print(massa_media)

# 3. Qual ilha tem mais indivíduos?
ilha_mais_populosa <- penguins_clean |>
  count(island) |>
  arrange(desc(n))
print(ilha_mais_populosa)

# 4. Em média, qual sexo possui a maior nadadeira para cada espécie?
nadadeira_sexo <- penguins_clean |>
  group_by(species, sex) |>
  summarise(tam_medio_flipper = mean(flipper_length_mm), .groups = "drop") |>
  group_by(species) |>
  slice_max(tam_medio_flipper, n = 1)
print(nadadeira_sexo)