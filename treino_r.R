library(dplyr)
library(lubridate)
library(stringr)

setwd("caminho_da_sua_pasta/pasta_do_arquivo")

db <- read.csv("nome_do_arquivo.csv")

db <- db %>% 
  mutate(
    sexo_p = case_when(
      SEXO == 1 ~ "Masculino",
      SEXO == 2 ~ "Feminino",
      SEXO == 0 ~ "Ignorado",
      TRUE ~ NA_character_
    )
  )
# para transformar a col IDADE em idade, usar stringr para extrair os anos
db <- db %>% 
  mutate(
    codigo_idade = str_sub(IDADE, 1, 1),
    anos = str_sub(IDADE, 2),
    idades = case_when(
      codigo_idade <= 3 ~ 0,
      codigo_idade == 4 ~ as.numeric(anos),
      codigo_idade == 5 ~ 100 + as.numeric(anos)
    )
  )

# Classificação das idades em faixa etária
db <- db %>% 
  mutate(
    faixa_etaria = case_when(
      idades >= 0 & idades <= 12 ~ "Criança",
      idades >= 13 & idades <= 17 ~ "Adolescente",
      idades >= 18 & idades <= 59 ~ "Adulto",
      idades >= 60 ~ "Idoso",
      TRUE ~ NA_character_
    )
  )

# quantidade de mortes or faixa etária
db %>%
  group_by(faixa_etaria) %>%
  summarise(mortes_faixa_etaria = n()) 
db %>% count(faixa_etaria, sort = FALSE, name = "mortes_faixa_etaria")

# criação de variável trimestre
db <- db %>% 
  mutate(
    dt_obito = dmy(DTOBITO),
    mes_dt = month(dt_obito, label = TRUE),
    trimestre = case_when(
      mes_dt %in% c("jan", "fev", "mar") ~ "1°trimestre",
      mes_dt %in% c("abr", "mai", "jun") ~ "2°trimestre",
      mes_dt %in% c("jul", "ago", "set") ~ "3°trimestre",
      mes_dt %in% c("out", "nov", "dez") ~ "4°trimestre",
      TRUE ~ NA_character_
    )
  )

# total de mortes e idade média por idade e sexo
db %>% 
  group_by(trimestre, sexo_p) %>% 
  summarise (
    total_obitos = n(),
    idade_media = mean(idades, na.rm = TRUE)
    )

#mês com maior número de óbitos
db %>%
  group_by(mes_dt) |>
  summarize(total_mes = n()) %>%
  arrange(desc(total_mes)) %>% 
  slice(1)


#diferença na proporção de óbitos por sexo
total_linhas = nrow(db)

freq_male = db %>% 
  group_by(sexo_p) %>% 
  filter(sexo_p == "Masculino") %>% 
  summarize(
    freq = n() / total_linhas 
  )

freq_female = db %>% 
  group_by(sexo_p) %>% 
  filter(sexo_p == "Feminino") |>
  summarize(
    freq = n() / total_linhas
  )
dif_prop = freq_male$freq - freq_female$freq
sprintf(
  "A diferenca percentual entre número de óbitos masculinos e femininos é: %f", dif_prop)


#faixa etaria com maior número de mortes
mortes_etaria <- db %>% 
  group_by(faixa_etaria) %>% 
  summarize(
    total_idade = n()) %>% 
  slice_max(total_idade, n = 1)

sprintf("A faixa etária com maior número de mortes é a %s com %d mortes", 
        mortes_etaria$faixa_etaria, mortes_etaria$total_idade)

View(db)


