# Carrega os dados do pacote
data("penguins")

# Salva a cópia dos dados brutos sem alterar o índice de linhas
write.csv(penguins, "data/raw/penguins.csv", row.names = FALSE)