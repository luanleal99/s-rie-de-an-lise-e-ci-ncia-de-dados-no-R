# ==============================================================================
# ATIVIDADE 05: LIDANDO COM STRINGS (MANIPULAÇÃO DE TEXTO NO R)
# Alunos: Luan Leal Serrão Ferraz - 127043
#         Renan Jordão Barcelos - 127048
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES E DADOS
# ------------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(janitor)
library(stringr)
library(naniar)
library(dplyr)
library(readxl)

# Desativa autenticação do Google para carregar a planilha pública
gs4_deauth()

url_planilha <- 'https://docs.google.com/spreadsheets/d/1PZ20j0uVbpDkHk-kifXRAHrLmFqAZWXynihr6xpL4As/edit?gid=57484849#gid=57484849'

# Carregamento das duas únicas tabelas do exercício
artists <- read_sheet(url_planilha, sheet = "artists")
artworks <- read_sheet(url_planilha, sheet = "artworks")


# ------------------------------------------------------------------------------
# 2. AVALIAÇÃO INICIAL E LIMPEZA
# ------------------------------------------------------------------------------

# Respostas das questões de avaliação inicial:

# 1. Existe alguma informação que ajude a relacionar as duas tabelas?
# Resposta: Sim! O identificador do artista está presente nas duas tabelas: na 
# tabela 'artists' a variável se chama 'id', enquanto na 'artworks' é 'artistId'. 
# Além disso, 'artists$name' e 'artworks$artist' contêm os nomes dos artistas.

# 2. Existe variáveis com observações que contém mais de uma informação nas duas tabelas?
# Resposta: Sim! Na tabela 'artists', as variáveis 'placeOfBirth' e 'placeOfDeath'
# juntam cidade e país na mesma string (ex: "London, United Kingdom"). 
# Na tabela 'artworks', a variável 'dimensions' possui altura, largura e profundidade
# juntas, e a variável 'medium' descreve vários materiais e técnicas numa mesma frase.

# Limpeza das tabelas originais (NAs e espaços em branco extras)
artists <- artists |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  mutate(across(where(is.character), ~ na_if(.x, "NULL"))) |>
  mutate(across(where(is.character), ~ str_squish(.x)))

artworks <- artworks |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  mutate(across(where(is.character), ~ na_if(.x, "NULL"))) |>
  mutate(across(where(is.character), ~ str_squish(.x)))


# --- Títulos mais longos e mais curtos (imprimindo direto no console) ---

# Títulos mais longos:
artworks |>
  filter(!is.na(title)) |>
  mutate(tamanho = str_length(title)) |>
  arrange(desc(tamanho)) |>
  select(title, tamanho) |>
  head(5)

# Títulos mais curtos:
artworks |>
  filter(!is.na(title)) |>
  mutate(tamanho = str_length(title)) |>
  arrange(tamanho) |>
  select(title, tamanho) |>
  head(5)


# ------------------------------------------------------------------------------
# 3. VERIFICANDO VARIÁVEIS (LOCAIS E DATAS)
# ------------------------------------------------------------------------------

# Visualizando subconjunto de locais
artists |>
  select(name, placeOfBirth, placeOfDeath) |>
  head(20)

# Separando informações de nascimento na própria tabela 'artists'
artists <- artists |>
  separate_wider_delim(
    cols = placeOfBirth,
    delim = ", ",
    names = c("cidade_nascimento", "pais_nascimento"),
    too_few = "align_start",
    too_many = "drop"
  )

# Investigando padrão de datas (4 dígitos para ano)
str_view(artworks$dateText, "[0-9]{4}")

# Criando a variável 'year' na tabela 'artworks' mantendo a variável anterior
artworks <- artworks |>
  mutate(year = str_extract(dateText, "[0-9]{4}"))

# 1. Qual é a vantagem e qual é a limitação de extrair o ano dessa maneira?
# Resposta: A vantagem é a facilidade e rapidez para capturar o primeiro ano
# numérico em textos comuns (ex: "c. 1920" vira "1920"). A limitação é que em 
# intervalos de anos (ex: "1850-1855") ele pega apenas o primeiro ano ("1850"),
# e em descrições por séculos (ex: "19th century") ele não encontra 4 dígitos e retorna NA.

# 2. Como você lidaria com os casos problemáticos?
# Resposta: Criaria regras com expressões regulares mais avançadas (regex) ou 
# condicionais com case_when(), por exemplo convertendo séculos para anos médios
# (ex: "19th century" -> 1850) ou tirando a média entre os dois anos de um intervalo.


# ------------------------------------------------------------------------------
# 4. DIMENSÕES
# ------------------------------------------------------------------------------

# Verificação das observações de dimensões
artworks |>
  select(dimensions) |>
  head(20)

artworks |>
  select(dimensions) |>
  tail(20)

# Problemas encontrados:
# A variável mistura texto explicativo, dimensões em mm, cm e polegadas.

# Solução: Adicionando variáveis de largura e altura numéricas (em mm)
artworks <- artworks |>
  mutate(
    largura_mm = as.numeric(str_extract(dimensions, "(?<=mm: )\\d+(?= ×)")),
    altura_mm  = as.numeric(str_extract(dimensions, "(?<=× )\\d+(?= mm)")),
    area_mm2   = largura_mm * altura_mm
  )

# 1. Que obras possuem maior tamanho (área)?
artworks |>
  filter(!is.na(area_mm2)) |>
  arrange(desc(area_mm2)) |>
  select(title, artist, dimensions, area_mm2) |>
  head(5)

# 2. Que obras são mais longas (largura)?
artworks |>
  filter(!is.na(largura_mm)) |>
  arrange(desc(largura_mm)) |>
  select(title, artist, dimensions, largura_mm) |>
  head(5)


# ------------------------------------------------------------------------------
# 5. RELACIONANDO AS TABELAS
# ------------------------------------------------------------------------------

# Tabela unificada para consultas relacionais
dados_completos <- artworks |>
  left_join(artists, by = c("artistId" = "id"))

# 1. Quem são os artistas com mais obras?
dados_completos |>
  count(artist, sort = TRUE) |>
  head(10)

# 2. De que países são a maior parte dos artistas?
dados_completos |>
  distinct(artistId, .keep_all = TRUE) |>
  filter(!is.na(pais_nascimento)) |>
  count(pais_nascimento, sort = TRUE) |>
  head(10)

# 3. Em que ano essas pessoas nasceram?
dados_completos |>
  distinct(artistId, .keep_all = TRUE) |>
  mutate(ano_nascimento = str_extract(dates, "[0-9]{4}")) |>
  select(artist, dates, ano_nascimento) |>
  head(10)


# ------------------------------------------------------------------------------
# 6. ANÁLISE DOS MATERIAIS E TÉCNICAS (MEDIUM)
# ------------------------------------------------------------------------------

# 1 e 2. Seleção de 3 materiais/técnicas e cálculo da proporção direto no console
artworks |>
  filter(!is.na(medium)) |>
  summarise(
    prop_papel   = mean(str_detect(medium, regex("paper", ignore_case = TRUE))),
    prop_oleo    = mean(str_detect(medium, regex("oil", ignore_case = TRUE))),
    prop_gravura = mean(str_detect(medium, regex("etching", ignore_case = TRUE)))
  )


# ------------------------------------------------------------------------------
# 7. CONTEÚDO DA OBRA (ANÁLISE TEMÁTICA)
# ------------------------------------------------------------------------------

# Filtrando obras com temática religiosa/espiritual pelos termos "God", "Satan" ou "Heaven"
artworks |>
  filter(!is.na(title)) |>
  mutate(
    religioso = str_detect(title, regex("God|Satan|Heaven", ignore_case = TRUE))
  ) |>
  count(religioso)