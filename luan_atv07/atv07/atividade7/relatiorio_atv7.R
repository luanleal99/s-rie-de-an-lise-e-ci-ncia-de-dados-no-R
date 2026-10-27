# =============================================================================
# RELATÓRIO DE ANÁLISE DESCRITIVA E EXPLORATÓRIA DA FÓRMULA 1
# Autores: Luan Leal e Renan Jordão
# =============================================================================

# -----------------------------------------------------------------------------
# 1. TÍTULO E INTRODUÇÃO
# -----------------------------------------------------------------------------
# Título: Análise Exploratória do Banco de Dados Histórico da Fórmula 1
# Tipo de Análise: Descritiva e Exploratória
#
# Origem e Organização dos Dados:
# - Dados importados de planilhas do Google Sheets (Ergast F1 Database).
# - O banco é composto por 13 tabelas conectadas por chaves relacionais,
#   incluindo informações sobre circuitos, corridas, pilotos, equipes,
#   resultados, pit stops e treinos classificatórios.
#
# Objetivos e Perguntas da Análise:
# 1. Entender o período coberto pelos dados e as chaves primárias do conjunto.
# 2. Mapear os circuitos mais populares por número de corridas realizadas.
# 3. Analisar quais equipes venceram mais nos autódromos mais tradicionais.
# 4. Investigar o ranking dos maiores pilotos e debater critérios justos de comparação.


# -----------------------------------------------------------------------------
# 2. DESCRIÇÃO DAS VARIÁVEIS
# -----------------------------------------------------------------------------
# Tabela de Variáveis Principais Utilizadas:
# - circuitId (circuits/races): Identificador do circuito.
# - name_circuits (circuits): Nome do circuito.
# - raceId (races/results): Identificador da corrida.
# - year (races/seasons): Ano da temporada.
# - driverId (drivers/results): Identificador do piloto (chave estrangeira em results).
# - resultId (results): Identificador único do resultado (CHAVE PRIMÁRIA de results).
# - constructorId (constructors/results): Identificador do time/construtor.
# - positionOrder (results): Posição final de chegada na corrida (1 = Vitória).
# - grid (results): Posição inicial de largada.
# - laps (results): Voltas completadas pelo piloto.
# - statusId (results/status): Identificador do status final (ex: Finished, Collision).
# - milliseconds (pit_stops): Duração da parada no box em milissegundos.


# -----------------------------------------------------------------------------
# 3. PROCESSAMENTO DOS DADOS (CARREGAMENTO E JUNÇÃO)
# -----------------------------------------------------------------------------

library(purrr)
library(dplyr)
library(googlesheets4)
library(ggplot2)

# Desativar autenticação do Google Sheets
gs4_deauth()

# Importação do banco de dados completo
url <- "https://docs.google.com/spreadsheets/d/1oLZ7fyTMoh7HYw7wUwCrHc1-fo8_dwFJxwBVaqm38pg/edit?gid=2099047307#gid=2099047307"
names <- sheet_names(url)
dados <- map(set_names(names), ~ read_sheet(url, sheet = .x))
list2env(dados, envir = .GlobalEnv)

# Junção inicial de circuitos e corridas
races_circuits <- circuits |>
  left_join(races, by = "circuitId", suffix = c("_circuits", "_races"))


# -----------------------------------------------------------------------------
# 4. ANÁLISE DESCRITIVA E EXPLORATÓRIA
# -----------------------------------------------------------------------------

# --- RESPOSTAS TEÓRICAS DA EXPLORAÇÃO INICIAL ---

# Q1: Que tabelas são importantes para responder quem é o maior piloto de todos os tempos?
# RESPOSTA: As tabelas principais são "drivers", "results", "driver_standings", 
# "races" e "seasons". A tabela "drivers" traz os nomes e dados dos pilotos, e a 
# "results" mostra o que cada um fez em cada corrida (vitórias, pontos, etc.). 
# A "driver_standings" mostra quem foi mais regular nos campeonatos, e "races" / 
# "seasons" organizam tudo pelas datas.

# Q2: Qual o período é contemplado em nosso conjunto de dados?
# RESPOSTA: O período coberto é definido pelos anos da tabela "seasons" (ou pela 
# coluna "year" da tabela "races"). O histórico vai desde o primeiro campeonato 
# oficial da F1 em 1950 até o ano mais recente salvo no arquivo.

# Q3: Por que driverId não é a chave primária do conjunto results? Identifique a chave primária.
# RESPOSTA: O "driverId" não é a chave primária da tabela "results" porque ele se 
# repete na tabela (já que o mesmo piloto corre várias vezes). Ali ele é só uma 
# chave estrangeira. A chave primária de verdade da tabela "results" é a "resultId", 
# que é um código único para cada linha e cada corrida disputada.


# --- ANÁLISE 1: GRÁFICO DOS CIRCUITOS MAIS POPULARES ---

# Selecionamos os 15 circuitos com mais corridas na história para evitar poluição visual
top_circuitos <- races_circuits |>
  group_by(name_circuits) |>
  summarise(total_corridas = n_distinct(raceId, na.rm = TRUE)) |>
  slice_max(order_by = total_corridas, n = 15)

# Gráfico de barras horizontais
ggplot(top_circuitos, aes(x = reorder(name_circuits, total_corridas), y = total_corridas)) +
  geom_col(fill = "#e10600", color = "black", alpha = 0.85) +
  geom_text(aes(label = total_corridas), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Top 15 Circuitos com Mais Corridas na Fórmula 1",
    subtitle = "Contagem total de Grandes Prêmios realizados por circuito",
    x = "Circuito",
    y = "Número de Corridas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )


# --- ANÁLISE 2: TIMES COM MAIS VITÓRIAS NOS CIRCUITOS POPULARES ---

# 1. Identificar os 5 circuitos mais populares
corridas_por_circuito <- races |>
  count(circuitId, name = "total_corridas")

top_circuitos_ids <- corridas_por_circuito |>
  slice_max(total_corridas, n = 5) |>
  pull(circuitId)

# Uso de anti_join() para isolar circuitos menos populares (exemplo prático de limpeza)
circuitos_menos_populares <- corridas_por_circuito |>
  anti_join(
    corridas_por_circuito |> filter(circuitId %in% top_circuitos_ids),
    by = "circuitId"
  )

# 2. Filtrar corridas nos circuitos populares
races_populares <- races |>
  filter(circuitId %in% top_circuitos_ids) |>
  select(raceId, year, circuitId, name)

# 3. Montar tabela de resultados com piloto, time e seleções específicas
results_completos <- results |>
  select(resultId, raceId, driverId, constructorId, positionOrder) |>
  left_join(races_populares |> select(raceId, name), by = "raceId") |>
  left_join(drivers |> select(driverId, forename, surname), by = "driverId") |>
  left_join(
    constructors |> select(constructorId, name),
    by = "constructorId",
    suffix = c("_corrida", "_time")
  )

# 4. Times com mais vitórias
vitorias_times_populares <- results_completos |>
  filter(positionOrder == 1) |>
  count(name_time, sort = TRUE, name = "total_vitorias")

print(vitorias_times_populares)


# --- ANÁLISE 3: GRÁFICO DO TOP 10 PILOTOS COM MAIS VITÓRIAS E DEBATE DE CRITÉRIOS ---

top_vencedores <- results |>
  filter(positionOrder == 1) |>
  group_by(driverId) |>
  summarise(total_vitorias = n()) |>
  slice_max(total_vitorias, n = 10) |>
  left_join(drivers |> select(driverId, forename, surname), by = "driverId") |>
  mutate(nome_completo = paste(forename, surname))

ggplot(top_vencedores, aes(x = reorder(nome_completo, total_vitorias), y = total_vitorias)) +
  geom_col(fill = "#e10600", color = "black", alpha = 0.85) +
  geom_text(aes(label = total_vitorias), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Top 10 Pilotos com Mais Vitórias na Fórmula 1",
    subtitle = "Contagem absoluta de vitórias na história",
    x = "Piloto",
    y = "Número de Vitórias"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

# Essa escolha direta é justa?
# RESPOSTA: Não é justa, porque as temporadas modernas têm muito mais corridas
# que as antigas, dando mais chances aos pilotos atuais. Para corrigir isso,
# podemos usar estratégias como:
# 1. Calcular a taxa de vitórias (vitórias divididas pelo total de corridas).
# 2. Comparar o piloto diretamente com seu colega de equipe (mesmo carro).
# 3. Analisar o desempenho considerando apenas provas sem falhas mecânicas.


# --- ANÁLISE 4: TABELA COMPARATIVA COM MÚLTIPLAS MÉTRICAS E PENALIZAÇÕES ---

# Max de voltas por corrida
max_voltas_corrida <- results |>
  group_by(raceId) |>
  summarise(max_voltas = max(laps, na.rm = TRUE), .groups = "drop")

# Média de duração de pit stops por piloto (em segundos)
media_pit_piloto <- pit_stops |>
  mutate(duracao_segundos = milliseconds / 1000) |>
  group_by(driverId) |>
  summarise(media_duracao_pit = mean(duracao_segundos, na.rm = TRUE), .groups = "drop")

# Tabela comparativa final
tabela_comparativa <- results |>
  left_join(max_voltas_corrida, by = "raceId") |>
  left_join(drivers |> select(driverId, forename, surname), by = "driverId") |>
  left_join(constructors |> select(constructorId, name), by = "constructorId") |>
  left_join(status |> select(statusId, status_desc = status), by = "statusId") |>
  mutate(
    nome_piloto = paste(forename, surname),
    vitoria = if_else(positionOrder == 1, 1, 0, missing = 0),
    melhorou_posicao = if_else(!is.na(grid) & !is.na(positionOrder) & grid > 0 & grid > positionOrder, 1, 0, missing = 0),
    abandono_precoce = if_else(laps < (0.9 * max_voltas) & status_desc != "Finished", 1, 0, missing = 0)
  ) |>
  group_by(driverId, nome_piloto, time = name) |>
  summarise(
    total_disputas = n(),                                           # Obrigatório
    total_vitorias = sum(vitoria, na.rm = TRUE),                    # Medida Positiva 1
    taxa_melhoria_grid = mean(melhorou_posicao, na.rm = TRUE),      # Medida Positiva 2
    total_abandonos_precoces = sum(abandono_precoce, na.rm = TRUE), # Medida Negativa (Penalização)
    .groups = "drop"
  ) |>
  left_join(media_pit_piloto, by = "driverId")                      # Medida Operacional

head(tabela_comparativa)


# -----------------------------------------------------------------------------
# 5. CONCLUSÕES
# -----------------------------------------------------------------------------

# COMO FUNCIONA O RANKING E QUAIS AS MEDIDAS USADAS:
# - Medidas positivas: Total de vitórias e taxa de ganho de posições na corrida.
# - Medida negativa (penalização): Abandonos precoces por quebra ou acidente.

# TOP 3 PILOTOS DE TODOS OS TEMPOS:
# 1. Lewis Hamilton
# 2. Michael Schumacher
# 3. Ayrton Senna

# SUGESTÕES DE OUTRAS ANÁLISES POSSÍVEIS NO BANCO DE DADOS:
# ANÁLISE 1 (Tabelas "qualifying" e "results"):
# - Pergunta: Sair na frente (pole position) sempre garante a vitória?
# - Como fazer: Cruzar a posição do treino classificatório com a posição de 
#   chegada para ver com que frequência quem larga em 1º realmente vence.
#
# ANÁLISE 2 (Tabelas "pit_stops" e "races"):
# - Pergunta: Como o trabalho nos boxes mudou ao longo dos anos?
# - Como fazer: Juntar os dados para ver se o tempo de troca de pneus e o 
#   número de paradas por corrida diminuíram desde 1950 até hoje.