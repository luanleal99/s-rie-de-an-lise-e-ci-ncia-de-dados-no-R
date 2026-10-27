# Atividade 3
# Alunos: Luan Leal e Renan Jordão

library(googlesheets4)
library(dplyr)
library(janitor)
library(readr)

gs4_deauth()

url <- "https://docs.google.com/spreadsheets/d/1sRwVUP3jDQANFwegg6lVuybYx9mdmGeHH_CNDVvbImM/edit?gid=703259414#gid=703259414"

sheet_names(url)
arabica <- read_sheet(url, sheet = "arabica_ratings_raw.csv")
robusta <- read_sheet(url, sheet = "robusta_ratings_raw.csv")

#- Quais foram os principais problemas encontrados? Se não há problemas, explique
#porque os dados são adequados para uma análise.
# As colunas contem diferenças entre si, nomes de variáveis, variáveis diferentes, tipos de variáveis

#- Você acha necessário remover colunas? Justifique.
# Sim. Algumas colunas contem muitas entradas vazias e não são tão importantes para a análise. Além que as duas tabelas possuem colunas
# diferentes, algumas a mais e outras a menos. Para padronizar as duas e se precaver contra possíveis problemas, remover colunas é uma boa opção.

#- Você acha necessário converter tipos de variáveis? Justifique.
# Sim. A partir de um simples teste com o comando str()é possível ver que a tabela possui diferença de tipos de variáveis pra mesma coluna
# Convertendo as variáveis, eliminamos esse erro.

#- Você acha necessário modificar medidas? Justifique.
# Sim. As tabelas possuem variações de unidades de medidas. Para garantir uma padronização das tabelas para a análise dos dados,
# é necessário modificar as medidas e adotar um padrão.

# Padronização dos nomes
arabica <- arabica |> clean_names()
robusta <- robusta |> clean_names()

# Renomeando colunas da segunda tabela com base na primeira
robusta <- robusta |>
  rename(
    aroma = fragrance_aroma,
    acidity = salt_acid,
    body = mouthfeel,
    uniformity = uniform_cup,
    sweetness = bitter_sweet
  )

# Ajuste do nome da coluna owner
if ("owner.1" %in% names(arabica)) names(arabica)[names(arabica) == "owner.1"] <- "owner_2"
if ("owner.1" %in% names(robusta)) names(robusta)[names(robusta) == "owner.1"] <- "owner_2"
if ("owner_2" %in% names(arabica)) names(arabica)[names(arabica) == "owner_2"] <- "owner_2"
if ("owner_2" %in% names(robusta)) names(robusta)[names(robusta) == "owner_2"] <- "owner_2"

# Seleção das colunas para remoção
colunas_remover <- c(
  "view_certificate_1", "view_certificate_2",
  "cupping_protocol_and_descriptors", "view_green_analysis_details",
  "request_a_sample", "na", "na_1", "na_2", "na_3",
  "unnamed_0", "notes"
)

# Remoção das colunas indesejadas
arabica <- arabica |> select(-any_of(colunas_remover))
robusta <- robusta |> select(-any_of(colunas_remover))

# Função para normalizar listas em texto
normalizar_listas <- function(df) {
  for (nome in names(df)) {
    if (is.list(df[[nome]]) && !inherits(df[[nome]], c("Date", "POSIXct", "POSIXlt"))) {
      df[[nome]] <- vapply(df[[nome]], function(valor) {
        if (length(valor) == 0 || all(is.na(valor))) return(NA_character_)
        paste(as.character(valor), collapse = " | ")
      }, character(1))
    }
  }
  df
}

arabica <- normalizar_listas(arabica)
robusta <- normalizar_listas(robusta)

# Conversão de todas as colunas para texto
arabica <- arabica |> mutate(across(everything(), as.character))
robusta <- robusta |> mutate(across(everything(), as.character))

# Criação da coluna com o tipo de café
arabica <- arabica |> mutate(coffee_type = "Arabica")
robusta <- robusta |> mutate(coffee_type = "Robusta")

# Função para converter as notas
converter_nota <- function(x) {
  if (is.list(x) && !inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    x <- vapply(x, function(valor) {
      if (length(valor) == 0 || all(is.na(valor))) return(NA_character_)
      as.character(valor[[1]])
    }, character(1))
  }
  resultado <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    valor <- x[[i]]
    if (length(valor) == 0 || is.na(valor)[1]) next
    if (inherits(valor, c("Date", "POSIXct", "POSIXlt"))) next
    texto <- as.character(valor)[1]
    if (grepl("^20[0-9]{2}[-/]", texto)) next
    resultado[i] <- suppressWarnings(parse_number(texto, locale = locale(decimal_mark = ".")))
  }
  resultado
}

# Lista de variáveis de pontuação
variaveis_pontuacao <- intersect(
  c("quality_score", "aroma", "flavor", "aftertaste", "acidity", "body",
    "balance", "uniformity", "clean_cup", "sweetness", "cupper_points",
    "total_cup_points"),
  union(names(arabica), names(robusta))
)

# Conversão das notas das duas tabelas
for (variavel in variaveis_pontuacao) {
  if (variavel %in% names(arabica)) arabica[[variavel]] <- converter_nota(arabica[[variavel]])
  if (variavel %in% names(robusta)) robusta[[variavel]] <- converter_nota(robusta[[variavel]])
}

#- 2.1 O que precisamos fazer e quais decisões precisamos tomar para juntar os dois bancos?
# Primeiro, precisamos padronizar os nomes das variáveis nas duas tabelas.
# Também precisamos verificar e corrigir os tipos das variáveis.
# As colunas que possuem o mesmo significado devem ter o mesmo nome.
# A junção será feita verticalmente com bind_rows(), pois as linhas representam avaliações diferentes.
# Também devemos criar uma coluna para identificar o tipo de café: Arábica ou Robusta.
# As colunas vazias, auxiliares ou sem correspondência podem ser removidas.

#- 2.3 Caso precise descartar variáveis em uma das tabelas, justifique essa exclusão.
# Sim. Algumas colunas podem ser removidas por serem auxiliares, estarem vazias
# ou não possuírem uma variável equivalente na outra tabela.
# A coluna Notes, presente apenas na tabela Arábica, pode ser removida para
# deixar as duas tabelas com a mesma estrutura.
# As colunas view_certificate_1, view_certificate_2, Request a Sample,
# Cupping Protocol and Descriptors e View Green Analysis Details também podem
# ser removidas, pois são links ou informações auxiliares.
# As colunas NA, NA.1, NA.2 e NA.3 podem ser removidas porque funcionam como
# separadores e não apresentam informações úteis para a análise.
# Nenhuma linha deve ser removida, pois mesmo observações com valores ausentes
# podem ser úteis em outras análises.

#- 2.4 Podemos comparar os dois tipos de café diretamente?
# Sim, podemos fazer uma comparação descritiva entre Arábica e Robusta,
# pois as duas tabelas possuem variáveis de avaliação semelhantes.
# Porém, a comparação deve ser feita com cautela, pois as tabelas possuem
# quantidades diferentes de observações, valores ausentes e diferenças nos
# países e métodos de processamento.
# Portanto, podemos comparar médias e medianas, mas não podemos afirmar que
# o tipo de café sozinho causou as diferenças encontradas.

# Junção das duas tabelas
coffe_rates <- bind_rows(arabica, robusta)

# Verificação das colunas após a junção
intersect(names(arabica), names(robusta))
setdiff(names(arabica), names(robusta))
setdiff(names(robusta), names(arabica))

# Tratamento para ignorar notas zeradas na análise
coffe_rates <- coffe_rates |>
  mutate(quality_score_analysis = if_else(quality_score == 0, NA_real_, quality_score))

# Salvamento da tabela final unificada
write_csv(
  coffe_rates |> select(-quality_score_analysis),
  "coffe_rates.csv",
  na = ""
)

#- 3.1 Que medidas você utilizaria para avaliar qual café teve o melhor desempenho?
# Eu utilizaria a média e a mediana da variável quality_score.
# Também analisaria o valor mínimo, o valor máximo e o número de observações válidas.
# A média mostra o desempenho geral, enquanto a mediana reduz a influência de valores extremos.
# O café com maior média e maior mediana de quality_score seria considerado o de melhor desempenho.

# Cálculo do desempenho geral
metricas <- coffe_rates |>
  group_by(coffee_type) |>
  summarise(
    n = n(),
    n_score_valido = sum(!is.na(quality_score_analysis)),
    quality_score_medio = mean(quality_score_analysis, na.rm = TRUE),
    quality_score_mediano = median(quality_score_analysis, na.rm = TRUE),
    qualidade_min = min(quality_score_analysis, na.rm = TRUE),
    qualidade_max = max(quality_score_analysis, na.rm = TRUE),
    .groups = "drop"
  )
print(metricas)
write_csv(metricas, "metricas_desempenho.csv")

#- 3.2 Qual café teve o melhor desempenho na avaliação?
# O café Arábica apresentou o melhor desempenho.
# Sua média de quality_score foi aproximadamente 82,15,
# enquanto a média do Robusta foi aproximadamente 80,87.
# A mediana do Arábica foi 82,50 e a do Robusta foi 81,50.
# Portanto, o Arábica apresentou resultados superiores nas duas medidas.

#- 3.3 Existem países que deveriam ser removidos?
# Não. Não é necessário remover os países com poucas observações,
# pois eles ainda podem ser úteis em outras análises.
# Para esta etapa, podemos apenas filtrar os dois países com mais observações
# dentro de cada tipo de café, sem excluir esses países da tabela principal.


# Seleção dos dois principais países por tipo
top_paises <- coffe_rates |>
  filter(!is.na(country_of_origin), country_of_origin != "") |>
  count(coffee_type, country_of_origin, sort = TRUE) |>
  group_by(coffee_type) |>
  slice_head(n = 2) |>
  ungroup()
print(top_paises)
write_csv(top_paises, "top2_paises_por_tipo.csv")


# Filtragem dos dados para os principais países
coffe_rates_top2_paises <- coffe_rates |>
  semi_join(top_paises, by = c("coffee_type", "country_of_origin"))
write_csv(
  coffe_rates_top2_paises |> select(-quality_score_analysis),
  "coffe_rates_top2_paises.csv",
  na = ""
)

# Comparação entre os principais países
comparacao_top2 <- coffe_rates_top2_paises |>
  group_by(coffee_type, country_of_origin) |>
  summarise(
    n = n(),
    quality_score_medio = mean(quality_score_analysis, na.rm = TRUE),
    quality_score_mediano = median(quality_score_analysis, na.rm = TRUE),
    processamento_principal = {
      x <- processing_method[!is.na(processing_method) & processing_method != ""]
      if (length(x) == 0) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
    },
    .groups = "drop"
  )
print(comparacao_top2)
write_csv(comparacao_top2, "comparacao_top2_paises.csv")

# Contagem dos tipos de processamento por país
processamento_top2 <- coffe_rates_top2_paises |>
  count(coffee_type, country_of_origin, processing_method, sort = TRUE)
print(processamento_top2)
write_csv(processamento_top2, "processamento_top2.csv", na = "")

#- 3.4 Qual desses países obteve o melhor desempenho para cada tipo de café?
# Entre os cafés Arábica, a Colômbia apresentou o melhor desempenho,
# com média de quality_score aproximadamente igual a 83,11.
# Entre os cafés Robusta, Uganda apresentou o melhor desempenho,
# com média de quality_score aproximadamente igual a 81,89.

#- 3.5 Esses países apresentam o mesmo tipo de processamento?
# Não. Os países não apresentam exatamente o mesmo tipo de processamento.
# Na Colômbia e no México, o processamento mais comum foi Washed / Wet.
# Na Índia, foram encontrados registros de processamento Natural / Dry e Washed / Wet.
# Em Uganda, o método de processamento não foi informado na maioria dos registros.
#
# O processamento pode ter alguma influência na qualidade do café,
# mas não é possível afirmar isso com certeza apenas com esses dados.
# Existem muitos valores ausentes e diferenças na quantidade de observações
# entre os países. Seria necessário realizar uma análise mais detalhada
# para verificar essa possível influência.

