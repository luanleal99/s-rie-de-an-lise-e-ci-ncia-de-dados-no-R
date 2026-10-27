install.packages('tidyverse')

library("tidyverse")

?table1


head(table1) # head mostra os dados da tabela no console

View(table1) # view mostra a tabela completa no lado do arquivo de código

str(table1) # str mostra os dados da tabelas no console com um formato reduzido, mostrando o tamanho de cada elemento (ex: tibble 6 x 4)

View(table2)
View(table3)
View(table4a)
View(table4b)

# para cada tabela visualizada:
# foi percebido que a table1 está de acordo com o formato tidy.
# já a table2 apresenta uma repetição desnecessária dos nomes dos países, por conta de haver uma linha para a população e para casos, separados.
# a table3 inclui dois valores(casos e população) na mesma célula.
# a table4a e table4b separam sem necessidade os casos e a população em diferentes tabelas, além de colocar valores(1999 e 2000) nas variáveis.
# portanto, apenas a table1 está em formato tidy.


table1$rate <- table1$cases / table1$population * 10000

# para o Brasil de 1999, a taxa de pessoas com tuberculose a cada 10000 habitantes é de 2.193930

# nos anos 2000, a menor taxa pertence ao afeganistão, com 1.294466.
# já a maior taxa do mesmo ano pertence ao Brasil, com 4.612363.

# para cálcular a taxa da table2 sem alterar a estrutura inicial, filtramos cada tipo e dividimos vetorialmente:

# Filtrar linhas de casos e linhas de população
cases_data <- table2[table2$type == "cases", ]
pop_data <- table2[table2$type == "population", ]

# Calcular a taxa por 10.000 habitantes
rate_val <- cases_data$count / pop_data$count * 10000

# Construir a tabela final com os resultados
result_table2 <- data.frame(
  country = cases_data$country,
  year = cases_data$year,
  rate = rate_val
)

# Visualizar o resultado
result_table2



# 2. Resolução para a taxa da table3


# Inspecionando a estrutura da tabela
str(table3)

# A última coluna 'rate' contém a fração "casos/população" como texto.
# Usamos strsplit() para separar os valores onde houver a barra "/"
split_rates <- strsplit(table3$rate, "/")

# Extraímos o 1º elemento (casos) e o 2º elemento (população) convertendo para número
table3$cases <- as.numeric(sapply(split_rates, `[`, 1))
table3$population <- as.numeric(sapply(split_rates, `[`, 2))

# Calculando a taxa por 10.000 habitantes
table3$calculated_rate <- table3$cases / table3$population * 10000

# Visualizando o resultado
head(table3)



# ==========================================
# 3. Resolução para table4a e table4b (Corrigido)
# ==========================================

# 1. Convertendo as tibbles para data.frame clássico do R
t4a_df <- as.data.frame(table4a)
t4b_df <- as.data.frame(table4b)

# 2. Transformar table4a (casos) de wide para long
table4a_long <- reshape(
  t4a_df,
  varying = c("1999", "2000"),
  v.names = "cases",
  timevar = "year",
  times = c(1999, 2000),
  idvar = "country",
  direction = "long"
)

# 3. Transformar table4b (população) de wide para long
table4b_long <- reshape(
  t4b_df,
  varying = c("1999", "2000"),
  v.names = "population",
  timevar = "year",
  times = c(1999, 2000),
  idvar = "country",
  direction = "long"
)

# 4. Unir (merge) as duas tabelas combinando país e ano
table4_merged <- merge(table4a_long, table4b_long, by = c("country", "year"))

# 5. Calcular a taxa por 10.000 habitantes
table4_merged$rate <- table4_merged$cases / table4_merged$population * 10000

# Limpar a coluna residual 'id' que o reshape cria e ordenar por país e ano
table4_merged$id <- NULL
table4_merged <- table4_merged[order(table4_merged$country, table4_merged$year), ]

# Visualizar o resultado final
table4_merged




# ==============================================================================
# ATIVIDADE 01 - PARTE 2: Banco de dados WHO (Tuberculose)
# ==============================================================================

library(tidyverse)

# Carregando os dados
data <- who

# ------------------------------------------------------------------------------
# Inspeção inicial e verificação da estrutura dos dados
# ------------------------------------------------------------------------------
dim(data)                     # Exibe o número de linhas e colunas
nrow(data)                    # Retorna 7240 observações
length(unique(data$country))  # Retorna 219 países
length(unique(data$year))     # Retorna 34 anos

# ------------------------------------------------------------------------------
# RESPOSTAS AOS QUESTIONAMENTOS DA ATIVIDADE
# ------------------------------------------------------------------------------

# 1. O que representa uma linha?
# R: Cada linha representa a combinação de um país em um ano específico,
# contendo as contagens de casos de tuberculose fracionadas em subcategorias.

# 2. Quais são as variáveis que aparecem explicitamente como colunas?
# R: Apenas 4 variáveis aparecem de forma explícita em colunas isoladas:
# - country (país)
# - iso2 (código de 2 letras do país)
# - iso3 (código de 3 letras do país)
# - year (ano)

# 3. Existem informações escondidas nos nomes das colunas?
# R: Sim. Nas outras 56 colunas (ex: new_sp_m014, newrel_f65), os nomes concatenam
# três informações distintas:
# - Método de diagnóstico/status (sp, sn, ep, rel)
# - Sexo (m para masculino, f para feminino)
# - Faixa etária (014, 1524, 2534, 3544, 4554, 5564, 65)

# 4. Existem colunas que representam valores de uma variável?
# R: Sim. Os nomes das colunas da 5ª à 60ª posição representam
# valores/categorias das variáveis de diagnóstico, sexo e idade.

# 5. Há mais de uma dimensão codificada em um nome de coluna?
# R: Sim. Há três dimensões codificadas juntas no mesmo nome de coluna
# (por exemplo: em `new_sp_m014` temos o método 'sp', o sexo 'm' e a idade '014').

# 6. Quantas observações existem?
# R: Existem 7.240 observações (linhas).

# 7. Quantos países e anos estão representados?
# R: Estão representados 219 países e 34 anos (de 1980 a 2013).


# ------------------------------------------------------------------------------
# Análise dos dados do Brasil e dificuldades com a estrutura atual
# ------------------------------------------------------------------------------

# Filtrando dados apenas do Brasil
who_br <- data[data$country == "Brazil", ]

# Calculando o total de casos por ano no R nativo:
who_br$total_casos <- rowSums(who_br[, 5:60], na.rm = TRUE)

# Dificuldades encontradas na análise:
#
# - Somar total de casos por ano: Como os dados estão espalhados em 56 colunas
#   e cheios de valores ausentes (NA), a soma exige o uso de `rowSums` com `na.rm = TRUE`.
#
# - Comparar casos por gênero (Homens vs. Mulheres): Como não há uma coluna para sexo,
#   é necessário identificar manualmente todas as colunas que contêm "m" ou "f"
#   nos nomes e somá-las isoladamente.
#
# - Análise por faixa etária ou diagnóstico: Impossível realizar agrupamentos
#   ou gráficos diretos sem antes transformar a tabela do formato 'wide' para 'long'.
#
# Conclusão: A organização dos dados faz parte do processo de análise,
# pois a estrutura atual complica operações básicas que seriam diretas em formato tidy.