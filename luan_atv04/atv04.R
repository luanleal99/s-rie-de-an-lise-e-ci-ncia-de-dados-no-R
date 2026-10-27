# Luan Leal Serrão Ferraz - 127043

# Renan Jordão Barcelos - 127048
  
  
install.packages("googlesheets4")
install.packages("janitor")
install.packages("tidyverse")
install.packages("naniar")
install.packages("dplyr")

library(googlesheets4)
library(janitor)
library(tidyverse)
library(naniar)
library(dplyr)



dados <- riskfactors

?riskfactors

?naniar

vis_miss(dados)

gg_miss_var(dados)

miss_var_summary(dados)

gg_miss_upset(dados)

# Avaliando rapidamente os dados com mais falta, você acha que pode ter algum motivo para que esses dados estejam com uma maior quantidade de NAs?
# Explique o pode causar isso.

# alugmas variáveis podem conter mais dados faltantes por conta de fatores externos, como por exemplo a gravidez, 
# que depende da pessoa ser do sexo feminino e ter uma idade mínima. ou variáveis relacionadas a bebida que dependem que o 
# sujeito ja tenha fumado antes.


# Existe observações sem falta? O que pode fazer com que alguams informações
# não faltem, enquanto outras faltem tanto?

# Variáveis sociodemográficas básicas (como sex, age, state) geralmente são de 
# preenchimento obrigatório pelo entrevistador ou de facílima resposta, garantindo 100% de preenchimento.
#Quanto mais específica, delicada ou condicional for a variável na dinâmica do questionário, maior será a proporção de NA



# 1. Quais combinações de valores faltantes aparecem com maior frequência?

# drink_days, drink_average, smoke_last, smoke_stop e pregnant


# 2. Existem variáveis que parecem frequentemente faltar juntas?

# drink_days + drink_average, smoke_stop + pregnant


#3. Você acredita que algumas dessas faltas poderiam ser consequência do mesmo processo?

# sim, principalmente se formos dividis essas ocorrências conjuntas em blocos, 
# como o bloco das variáveis relacionadas ao alcoolismo ou ao tabagismo.




#4. Que hipóteses podem explicar essas combinações?

# Idade do indivíduo pode explicar isso. Pois a partir do momento que o dado pertence a uma pessoa jovem,
# é bem provável que a pessoa não tenha relação com gravidez, bebidas alcoólicas ou cigarro.



dados |> 
  group_by(sex) |>
  summarise(
    proporcao_faltante =
      mean(is.na(pregnant))
  )


# 1. Qual foi o motivo da escolha das variáveis?

# a variável sex foi escolhida por não conter nenhum dado faltante, em contraste com a variável pregnant,
# que possuia a maior quantidade de dados faltantes. Elas form escolhidas com o intuito de encontrar padrões e informações.

 

# 2. Você conseguiu alguma intuição sobre as faltas a partir dessa análise?

# Sim. A análise demonstra que a ausência do dado não ocorre de forma aleatória ou por falha na coleta, mas sim por uma regra condicional lógica do questionário:
# a pergunta sobre gravidez é ignorada para participantes do sexo masculino.


# 3. Os grupos parecem ter proporções muito diferentes?

# Sim, a diferença é extrema. O grupo masculino apresenta 100% de dados faltantes (proporção de 1,0 ), 
# enquanto o grupo feminino apresenta uma proporção significativamente menor de NAs (0,804), 
# concentrando as respostas válidas da pesquisa.






#Os mecanismos de dados faltantes dsicutidos em sala pode ser resumidos como como:
#• MCAR:a ausência não depende das variáveis observadas nem dos próprios valores
#ausentes;
#• MAR: a ausência pode depender de outras informações observadas;
#• MNAR: a ausência pode depender do próprio valor que não foi observado.
#Com base nisso, responda:


# 1. MCAR é uma suposição plausível para alguma das variáveis?

# Não para a maioria. Esse tipo de falta só acontece por acidentes puros, como uma falha no sistema ao salvar a resposta. 
# Como as ausências nesse estudo seguem uma lógica de perfil ou de regras do formulário, quase nenhuma variável relevante pode ser considerada MCAR.


# 2. Você consegue levantar uma hipótese MAR para alguma variável?

# sim, a variável pregnant . A falta dessa informação é explicada por outra resposta que já conhecemos no formulário: o sex. 
# Como a ausência depende de um dado presente na base (ser do sexo masculino), temos um caso clássico de MAR.


# 3. Você consegue imaginar uma hipótese MNAR para alguma das variáveis?

# Sim, a variável income ou weight . Pessoas com renda muito baixa 
# e pessoas acima do peso costumam se sentir desconfortáveis e preferem omitir a resposta.






# ================== PARTE 2 ====================




gs4_deauth()

url_planilha <- "https://docs.google.com/spreadsheets/d/1TuXNhTqiK8VjWxr3xoiS1-SP9cIWG6Wh6qDHLuHXQhY/edit?gid=0#gid=0"

dados_rs <- read_sheet(url_planilha) |> 
  clean_names()

glimpse(dados_rs)

miss_var_summary(dados_rs)

vis_miss(dados_rs)

colnames(dados_rs)

dados_rs |> 
  filter(is.na(x2003_rank)) |> 
  select(album, release_year, x2003_rank, x2012_rank, x2020_rank)



# 1. Existem variáveis que costumam faltar juntas?

# Rankings passados: x2003_rank e x2012_rank faltam juntos em discos mais novos.
# Dados do artista: artist_gender, artist_birth_year_sum e avg_age_at_top_500_album 
# faltam juntos em bandas com dados biográficos incompletos.


# 2. Existe uma explicação óbvia para a ausência das variáveis? 
# Se sim, que tipo de informação poderia substituir os valores faltantes da tabela?

# Sim. Álbuns lançados após 2003/2012 não existiam na época dos rankings, e discos sem apelo comercial não entraram na Billboard. 
# O que substituir:  Rankings: Usar "Não lançado" (para discos posteriores) ou 501 / "Fora do Top 500" (para discos que existiam, mas não ranquearam).




# 1. Que variáveis podemos supor que são MAR?

# x2003_rank e x2012_rank. A ausência é explicada pelo ano de lançamento do álbum.


# 2. Que variáveis podemos supor que são MNAR?

# spotify_popularity e dados biográficos dos artistas (como artist_birth_year_sum). 
# A falta depende do próprio valor omitido (álbuns obscuros ou impopulares não são catalogados).


# 3. Para as MNAR, por que não podemos ter certeza que são MNAR?

# Porque o dado faltante não está disponível para teste. 
# Não dá para provar se a falta ocorre pela baixa popularidade do álbum ou por uma falha técnica de coleta/API.