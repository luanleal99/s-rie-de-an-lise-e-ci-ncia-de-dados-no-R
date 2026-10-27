library(here)

penguins_clean <- read.csv(here("data", "processed", "penguins_clean.csv"))

# Gráfico 1: Massa corporal por espécie (Boxplot)
png(here("outputs", "massa_corporal_por_especie.png"), width = 800, height = 600)
boxplot(
  body_mass_g ~ species,
  data = penguins_clean,
  main = "Distribuição da Massa Corporal por Espécie de Pinguim",
  xlab = "Espécie",
  ylab = "Massa Corporal (g)",
  col = c("darkorange", "purple", "cyan4")
)
dev.off()

# Gráfico 2: Relação entre Comprimento do Bico e Massa Corporal (Dispersão)
png(here("outputs", "bico_vs_massa.png"), width = 800, height = 600)
plot(
  penguins_clean$bill_length_mm, 
  penguins_clean$body_mass_g,
  col = as.factor(penguins_clean$species),
  pch = 19,
  main = "Relação entre Comprimento do Bico e Massa Corporal",
  xlab = "Comprimento do Bico (mm)",
  ylab = "Massa Corporal (g)"
)
legend(
  "topleft", 
  legend = levels(as.factor(penguins_clean$species)), 
  col = 1:3, 
  pch = 19
)
dev.off()