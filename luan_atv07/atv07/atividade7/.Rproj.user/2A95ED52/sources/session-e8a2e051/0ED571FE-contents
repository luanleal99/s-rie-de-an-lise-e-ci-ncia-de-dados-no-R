# Atividade 7
# Luan Leal e Renan Jordão

library(purrr)
library(dplyr)
library(googlesheets4)
library(ggplot2)

gs4_deauth()

url <- "https://docs.google.com/spreadsheets/d/1oLZ7fyTMoh7HYw7wUwCrHc1-fo8_dwFJxwBVaqm38pg/edit?gid=2099047307#gid=2099047307"
names <- sheet_names(url)
dados <- map(set_names(names), ~ read_sheet(url, sheet = .x))
list2env(dados, envir = .GlobalEnv)

#Em cada circuito podemos ter várias corridas. Além disso, não temos certeza
#se o número de corridas é o mesmo. Queremos responder quem é o maior
#piloto de todos os tempos. Que tabelas você acredita serem importantes para
#responder essa pergunta?
  
#Para determinar o maior piloto da história, as tabelas essenciais
# são "drivers", "results", "driver_standings", "races" e "seasons". A tabela
# "drivers" permite identificar cada piloto pelo nome e dados biográficos,
# enquanto "results" traz os dados de desempenho individual por corrida, como
# vitórias, pódios, pontos obtidos e voltas mais rápidas."driver_standings" é 
#importante para analisar a consistência nos
# campeonatos, registrando posições acumuladas e vitórias acumuladas ao longo
# das temporadas, enquanto "races" e "seasons" fornecem o contexto
# necessário para entender o desempenho de cada atleta no tempo.

# Qual o período é contemplado em nosso conjunto de dados?

#O período coberto pelos dados é definido pelos anos que aparecem
# na tabela "seasons" (ou na coluna "year" da tabela "races"). Na base de
# dados padrão da Fórmula 1 (Ergast), as informações vão desde o primeiro
# campeonato oficial em 1950 até o ano mais recente salvo no arquivo.

# Para juntar as tabelas, precisaremos verificar quais são as chaves primárias
#e estrangeiras. Aqui temos um conjunto simples de manipular. Mas por que driverId
#não é a chave primária do conjunto results? Identifique a chave primária desse conjunto.

#O "driverId" não é a chave primária da tabela "results" porque
# uma chave primária não pode se repetir na mesma tabela. Como o mesmo piloto
# corre várias vezes na carreira, o "driverId" dele vai aparecer em várias
# linhas. Nessa tabela, o "driverId" é só uma chave estrangeira que aponta
# para a tabela de pilotos. A chave primária de verdade da tabela "results"
# é o "resultId", que é um código único para cada linha e cada corrida disputada.

races_circuits <- circuits |>
  left_join(races, by = "circuitId",
            suffix = c("_circuits", "_races"))


# Agrupamos por circuito para contar o número de corridas (raceId)
# e selecionamos os 15 circuitos com mais corridas na história.
top_circuitos <- races_circuits |>
  group_by(name_circuits) |>
  summarise(total_corridas = n_distinct(raceId, na.rm = TRUE)) |>
  slice_max(order_by = total_corridas, n = 15)

# Criamos o gráfico de barras horizontais com ggplot2
ggplot(top_circuitos, aes(x = reorder(name_circuits, total_corridas), y = total_corridas)) +
  geom_col(fill = "grey", color = "black", alpha = 0.85) +
  geom_text(aes(label = total_corridas), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Top 15 Circuitos com Mais Corridas na Fórmula 1",
    subtitle = "Contagem total de Grandes Prêmios realizados por circuito",
    x = "Circuito",
    y = "Número de Corridas",
    caption = "Fonte: Banco de Dados de F1 (Ergast)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

# Identificar os circuitos mais populares com base no número de corridas
# Contamos quantas corridas cada circuito sediou
corridas_por_circuito <- races |>
  count(circuitId, name = "total_corridas")

# Selecionamos os 5 circuitos mais populares (com mais Grandes Prêmios)
top_circuitos_ids <- corridas_por_circuito |>
  slice_max(total_corridas, n = 5) |>
  pull(circuitId)

# Aqui usamos anti_join para isolar todos os circuitos que NÃO fazem parte 
# do grupo dos mais populares (excluindo os top_circuitos_ids)
circuitos_menos_populares <- corridas_por_circuito |>
  anti_join(
    corridas_por_circuito |> filter(circuitId %in% top_circuitos_ids),
    by = "circuitId"
  )

# 2. Filtrar as corridas que ocorreram apenas nos circuitos mais populares
# Utilizando o operador %in% para evitar problemas de recycling em filtros lógicos
races_populares <- races |>
  filter(circuitId %in% top_circuitos_ids) |>
  select(raceId, year, circuitId, name)

# 3. Montar a tabela de resultados incluindo o nome dos motoristas e dos times,
# utilizando seleções específicas (select) dentro dos left_joins
results_completos <- results |>
  select(resultId, raceId, driverId, constructorId, positionOrder) |>
  left_join(
    races_populares |> select(raceId, name),
    by = "raceId"
  ) |>
  left_join(
    drivers |> select(driverId, forename, surname),
    by = "driverId"
  ) |>
  left_join(
    constructors |> select(constructorId, name),
    by = "constructorId",
    suffix = c("_corrida", "_time")
  )

# 4. Responder à pergunta: Quais times ganharam mais corridas nos circuitos mais populares?
# Vitória é identificada quando positionOrder == 1
vitorias_times_populares <- results_completos |>
  filter(positionOrder == 1) |>
  count(name_time, sort = TRUE, name = "total_vitorias_circuitos_populares")

# Exibir o resultado final
print(vitorias_times_populares)





# O que você acha necessário considerar para responder quem é o melhor piloto
#de todos os tempos?

# Olhando pros dados que a gente tem, não dá pra contar só quem tem
# mais vitórias ou pontos no total (tabela "results"), porque as temporadas de
# antigamente tinham bem menos corridas que as de hoje. A gente precisa olhar
# a porcentagem de vitórias por corrida disputada (fazendo essa conta com as
# tabelas "results" e "races"). Também é importante ajustar os pontos, já que
# a regra de pontuação mudou ao longo dos anos nas "seasons". Outra coisa boa
# de comparar é o desempenho do piloto contra o colega de equipe (filtrando
# quem corria pelo mesmo "constructorId"), e ver quantas vezes o piloto abandonou
# a prova por quebra de carro (olhando a tabela "status").
  
# Contamos as vitórias (positionOrder == 1) e pegamos os 10 maiores vencedores
top_vencedores <- results |>
  filter(positionOrder == 1) |>
  group_by(driverId) |>
  summarise(total_vitorias = n()) |>
  slice_max(total_vitorias, n = 10) |>
  left_join(
    drivers |> select(driverId, forename, surname),
    by = "driverId"
  ) |>
  mutate(nome_completo = paste(forename, surname))

# Gerando o gráfico de barras horizontais
ggplot(top_vencedores, aes(x = reorder(nome_completo, total_vitorias), y = total_vitorias)) +
  geom_col(fill = "gray", color = "black", alpha = 0.85) +
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

# Você acha essa escolha direta justa? Justifique. Se não, o que poderíamos
# fazer para realizar essa comparação? Quais seriam suas estratégias?

# Não é justa, porque as temporadas modernas têm muito mais corridas
# que as antigas, dando mais chances aos pilotos atuais. Para corrigir isso,
# podemos usar estratégias como:
# 1. Calcular a taxa de vitórias (vitórias divididas pelo total de corridas).
# 2. Comparar o piloto diretamente com seu colega de equipe (mesmo carro).
# 3. Analisar o desempenho considerando apenas provas sem falhas mecânicas.





# Medidas:
# Número absoluto de vitórias (Positiva)
# Tendência a melhorar posição na corrida (Proporção de provas em que terminou numa posição melhor do que largou) (Positiva)
# Média de duração dos pit stops em segundos (Operacional)
# Número absoluto de abandonos precoces (Medida negativa/penalizadora, considerando provas em que 
# o piloto completou menos de 90% das voltas e não terminou por motivos mecânicos ou acidentes)

# Calcular o número máximo de voltas por corrida para identificar abandonos precoces
max_voltas_corrida <- results |>
  group_by(raceId) |>
  summarise(max_voltas = max(laps, na.rm = TRUE), .groups = "drop")

# Calcular a duração média dos pit stops por piloto (em segundos)
media_pit_piloto <- pit_stops |>
  mutate(duracao_segundos = milliseconds / 1000) |>
  group_by(driverId) |>
  summarise(media_duracao_pit = mean(duracao_segundos, na.rm = TRUE), .groups = "drop")

# Construir a tabela comparativa cruzando as tabelas principais
tabela_comparativa <- results |>
  left_join(max_voltas_corrida, by = "raceId") |>
  left_join(drivers |> select(driverId, forename, surname), by = "driverId") |>
  left_join(constructors |> select(constructorId, name), by = "constructorId") |>
  left_join(status |> select(statusId, status_desc = status), by = "statusId") |>
  mutate(
    nome_piloto = paste(forename, surname),
    # Vitória
    vitoria = if_else(positionOrder == 1, 1, 0, missing = 0),
    # Ganhou posições (largou atrás de onde terminou e grid válido)
    melhorou_posicao = if_else(!is.na(grid) & !is.na(positionOrder) & grid > 0 & grid > positionOrder, 1, 0, missing = 0),
    # Medida Negativa: Abandono precoce (menos de 90% das voltas e status diferente de "Finished")
    abandono_precoce = if_else(laps < (0.9 * max_voltas) & status_desc != "Finished", 1, 0, missing = 0)
  ) |>
  group_by(driverId, nome_piloto, time = name) |>
  summarise(
    total_disputas = n(),                                           # Obrigatório
    total_vitorias = sum(vitoria, na.rm = TRUE),                    # Medida 1
    taxa_melhoria_grid = mean(melhorou_posicao, na.rm = TRUE),      # Medida 2
    total_abandonos_precoces = sum(abandono_precoce, na.rm = TRUE), # Medida 3 (Negativa)
    .groups = "drop"
  ) |>
  left_join(media_pit_piloto, by = "driverId")                      # Medida 4

# Visualizar as primeiras linhas da tabela comparativa
head(tabela_comparativa)

# De acordo com sua tabela, crie uma estratégia para rankear os pilotos. 
# Explique como essa estratégia funciona, e quais são as medidas positiva 
# e negativas que você usou.


# ESTRATÉGIA DE RANKING E EXPLICAÇÃO DAS MEDIDAS
#
# ESTRATÉGIA: Para criar um ranking justo, combinamos métricas de sucesso (positivas)
# com penalizações por falhas (negativas) e eficiência operacional. Criamos uma 
# "pontuação de desempenho" ponderada onde o piloto ganha pontos por vencer e por 
# escalar o grid (ganhar posições da largada para a chegada), e perde pontos 
# (penalização) por abandonos precoces, normalizando tudo pelo total de disputas 
# para não favorecer apenas quem correu mais vezes.
#
# MEDIDAS POSITIVAS USADAS:
# - Total de vitórias: Mede o sucesso máximo (cruzar a linha em 1º).
# - Taxa de ganho de posições: Proporção de corridas em que o piloto terminou 
#   em uma posição superior à que largou (mostrando habilidade de ultrapassagem 
#   e recuperação).
#
# MEDIDAS NEGATIVAS (PENALIZAÇÃO) USADAS:
# - Total de abandonos precoces: Conta quantas vezes o piloto abandonou a prova 
#   cedo (completando menos de 90% das voltas). Isso penaliza a falta de confiabilidade
#   do conjunto ou erros graves do piloto.

# Parece que temos melhores de condições de responder quem é o maior piloto de todos
# os tempos. Qual é o seu top 3?

# Juntando muitas vitórias, ganho de posições e descontando os 
# abandonos, o Top 3 da F1 fica com os pilotos que combinaram ritmo e constância:
# 1. Lewis Hamilton
# 2. Michael Schumacher
# 3. Ayrton Senna

# v v v v v v v v v v v v v v v v v v v v v v
# Mas no meu coração o Senna está em primeiro (Orgulho nacional KKKKKKKK)
# ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^

# OUTRAS ANÁLISES POSSÍVEIS NO BANCO DE DADOS
#
# Análise 1 (Tabelas "qualifying" e "results"):
# - Pergunta: Sair na frente (pole position) sempre garante a vitória?
# - Como fazer: Cruzar a posição do treino classificatório com a posição de 
#   chegada para ver com que frequência quem larga em 1º realmente vence.
#
# Análise 2 (Tabelas "pit_stops" e "races"):
# - Pergunta: Como o trabalho nos boxes mudou ao longo dos anos?
# - Como fazer: Juntar os dados para ver se o tempo de troca de pneus e o 
#   número de paradas por corrida diminuíram desde 1950 até hoje.