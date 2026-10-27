library(here)
library(dplyr)
library(tidyr) 

# 1. Ler os dados brutos
penguins_raw <- read.csv(here("data", "raw", "penguins.csv"))

# 2. Investigar ausência por coluna
na_count <- colSums(is.na(penguins_raw))
print(na_count)

# 3. Remover linhas com ausência
penguins_clean <- penguins_raw |> 
  drop_na()

# 4. Salvar versão limpa
write.csv(penguins_clean, here("data", "processed", "penguins_clean.csv"), row.names = FALSE)