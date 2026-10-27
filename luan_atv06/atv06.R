# Atividade 6

# Nome: Renan Jordão e Luan Leal

library(tidyverse)
library(lubridate)
library(forcats)
library(googlesheets4)

gs4_deauth()

# Importando os dados da aba schedule

schedule <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1diUu1YwFmsEKHL1JS8bZAHnB-ge42aB9uu_-1AowNMg/edit",
  sheet = "schedule.csv"
)

# Mostra as modalidades
schedule |>
  count(discipline_name, sort = TRUE)

# Conta as modalidades
schedule |>
  summarise(
    n_modalidades = n_distinct(discipline_name)
  )

# Período dos dados

schedule |>
  summarise(
    inicio = min(date),
    fim = max(date)
  )

# 1.O que o tipo da variável date representa e como separar essas datas por semana?
# A variável date representa a data em que cada evento acontece, ou seja, informa o dia do evento nos Jogos Olímpicos.

# Criando o número da semana e o fator ordenado

schedule <- schedule |>
  mutate(
    date = as.Date(date)
  )

schedule <- schedule |>
  mutate(
    numero = as.integer(date - min(date)) %/% 7 + 1,
    n_week = fct_inorder(paste("Semana", numero))
  )

#Por que a ordem dos níveis pode ser importante em uma visualização?
# A ordem dos níveis é importante para manter os dias na ordem correta

schedule <- schedule |>
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      ),
      labels = c(
        "Segunda",
        "Terça",
        "Quarta",
        "Quinta",
        "Sexta",
        "Sábado",
        "Domingo"
      )
    )
  )

levels(schedule$day_of_week)

# Ordenando as modalidades pela frequência

schedule <- schedule |>
  mutate(
    discipline_name = fct_infreq(discipline_name),
    discipline_code = fct_infreq(discipline_code)
  )

# Mantendo as oito modalidades mais frequentes

schedule <- schedule |>
  mutate(
    group = fct_lump_n(
      discipline_name,
      n = 8,
      other_level = "Other"
    )
  )

# As modalidades menos frequentes são agrupadas em Other,

#2. Quais as vantagens e desvantages de realizar uma análise a partir dessa nova variável?

# facilita a análise, mas perde o detalhe dessas modalidades

schedule |>
  count(group, sort = TRUE)

# Quantidade de eventos por semana

schedule |>
  count(n_week, sort = TRUE)

# Verificando as classes das variáveis de data e hora

class(schedule$start_datetime_local)
class(schedule$start_datetime_utc)
class(schedule$start_time)

# Convertendo para data-hora caso tenham sido importadas como texto

if (is.character(schedule$start_datetime_local)) {
  schedule$start_datetime_local <- ymd_hms(
    schedule$start_datetime_local,
    tz = "UTC"
  )
}

if (is.character(schedule$start_datetime_utc)) {
  schedule$start_datetime_utc <- ymd_hms(
    schedule$start_datetime_utc,
    tz = "UTC"
  )
}

if (is.character(schedule$end_datetime_utc)) {
  schedule$end_datetime_utc <- ymd_hms(
    schedule$end_datetime_utc,
    tz = "UTC"
  )
}

# Criando os horários locais de Brasília, Amazonas e Acre

schedule <- schedule |>
  mutate(
    start_brasilia = with_tz(
      start_datetime_utc,
      tzone = "America/Sao_Paulo"
    ),
    end_brasilia = with_tz(
      end_datetime_utc,
      tzone = "America/Sao_Paulo"
    ),
    start_amazonas = with_tz(
      start_datetime_utc,
      tzone = "America/Manaus"
    ),
    end_amazonas = with_tz(
      end_datetime_utc,
      tzone = "America/Manaus"
    ),
    start_acre = with_tz(
      start_datetime_utc,
      tzone = "America/Rio_Branco"
    ),
    end_acre = with_tz(
      end_datetime_utc,
      tzone = "America/Rio_Branco"
    )
  )

# Classificando os eventos por horário

# case_when() é usado porque a função recebe várias linhas ao mesmo tempo

classificar_horario <- function(data_hora) {
  
  hora <- hour(data_hora)
  dia <- wday(data_hora, week_start = 1)
  
  case_when(
    hora >= 18 & hora < 23 ~ "Horário nobre",
    hora >= 8 & hora < 23 & dia >= 6 ~ "Fim de semana",
    hora >= 7 & hora < 18 & dia <= 5 ~ "Dia útil",
    TRUE ~ "Madrugada"
  )
}

schedule <- schedule |>
  mutate(
    horario_brasilia = classificar_horario(start_brasilia),
    horario_amazonas = classificar_horario(start_amazonas),
    horario_acre = classificar_horario(start_acre)
  )

# Transformando os horários em fatores ordenados

schedule <- schedule |>
  mutate(
    horario_brasilia = factor(
      horario_brasilia,
      levels = c(
        "Madrugada",
        "Dia útil",
        "Fim de semana",
        "Horário nobre"
      )
    ),
    horario_amazonas = factor(
      horario_amazonas,
      levels = c(
        "Madrugada",
        "Dia útil",
        "Fim de semana",
        "Horário nobre"
      )
    ),
    horario_acre = factor(
      horario_acre,
      levels = c(
        "Madrugada",
        "Dia útil",
        "Fim de semana",
        "Horário nobre"
      )
    )
  )

# Situação escolhida: pessoa em Viçosa entre 7h e 8h em dias úteis

eventos_vicosa <- schedule |>
  filter(
    hour(start_brasilia) >= 7,
    hour(start_brasilia) < 8,
    wday(start_brasilia, week_start = 1) <= 5
  )

# Quantidade de premiações assistidas

eventos_vicosa |>
  filter(is_medal_event == TRUE) |>
  summarise(
    quantidade_premiacoes = n()
  )

# Quantidade de eventos e modalidades nesse horário

eventos_vicosa |>
  summarise(
    quantidade_eventos = n(),
    quantidade_modalidades = n_distinct(discipline_name)
  )

# Proporção entre treinamentos e competições

schedule |>
  mutate(
    tipo_evento = if_else(
      is_training,
      "Treinamento",
      "Competição"
    )
  ) |>
  count(tipo_evento) |>
  mutate(
    proporcao = n / sum(n)
  )

# Eventos no horário nobre em cada região

horario_nobre <- schedule |>
  mutate(
    brasilia = horario_brasilia == "Horário nobre",
    amazonas = horario_amazonas == "Horário nobre",
    acre = horario_acre == "Horário nobre"
  )

# Modalidades mais presentes no horário nobre em Brasília

horario_nobre |>
  filter(brasilia) |>
  count(discipline_name, sort = TRUE)

# Modalidades mais presentes no horário nobre no Amazonas

horario_nobre |>
  filter(amazonas) |>
  count(discipline_name, sort = TRUE)

# Modalidades mais presentes no horário nobre no Acre

horario_nobre |>
  filter(acre) |>
  count(discipline_name, sort = TRUE)

# Treinamentos e competições no horário nobre em Brasília

horario_nobre |>
  filter(brasilia) |>
  mutate(
    tipo_evento = if_else(
      is_training,
      "Treinamento",
      "Competição"
    )
  ) |>
  count(tipo_evento, sort = TRUE)

# Análise final considerando o horário nobre em alguma região do Brasil

horario_nobre <- horario_nobre |>
  mutate(
    horario_nobre_brasil = brasilia | amazonas | acre
  )

# Modalidades mais presentes no horário nobre considerando o Brasil

horario_nobre |>
  filter(horario_nobre_brasil) |>
  count(discipline_name, sort = TRUE)

# Proporção entre treinamentos e competições nesse horário

horario_nobre |>
  filter(horario_nobre_brasil) |>
  mutate(
    tipo_evento = if_else(
      is_training,
      "Treinamento",
      "Competição"
    )
  ) |>
  count(tipo_evento) |>
  mutate(
    proporcao = n / sum(n)
  )

# Calculando a duração dos eventos em minutos

schedule <- schedule |>
  mutate(
    duracao_minutos = as.numeric(
      difftime(
        end_datetime_utc,
        start_datetime_utc,
        units = "mins"
      )
    )
  )

# Duração média e mediana dos eventos

schedule |>
  summarise(
    duracao_media = mean(
      duracao_minutos,
      na.rm = TRUE
    ),
    duracao_mediana = median(
      duracao_minutos,
      na.rm = TRUE
    )
  )

# Duração média e mediana por tipo de evento

schedule |>
  mutate(
    tipo_evento = if_else(
      is_training,
      "Treinamento",
      "Competição"
    )
  ) |>
  group_by(tipo_evento) |>
  summarise(
    duracao_media = mean(
      duracao_minutos,
      na.rm = TRUE
    ),
    duracao_mediana = median(
      duracao_minutos,
      na.rm = TRUE
    )
  )
