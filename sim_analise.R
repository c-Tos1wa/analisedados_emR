install.packages("dplyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("readr")
install.packages("ggplot2")
install.packages("tibble")
install.packages("tidyr")
install.packages("readxl")
install.packages("arrow")

setwd("caminhodasuapasta/dados")
getwd()
df_csv <- read.csv("caminhodasuapasta/dados/arquivo.csv")

library(dplyr)
library(lubridate)
glimpse(df_csv)

head(df_csv)
tail(df_csv)
summary(df_csv)

table(df_csv$SEXO, useNA = "always")

df_csv %>%
  count(SEXO, sort = TRUE)

#if_else
df_csv <- df_csv %>% 
  mutate(
    gender = if_else(SEXO == 1, "MASCULINO",
                             if_else(SEXO == 2, "FEMININO",
                                     if_else(SEXO == 0, "IGNORADO", NA_character_)))
  )
df_csv %>% count(SEXO)
df_csv %>% count(gender)

# Case When
df_csv <- df_csv %>% 
  mutate(
    skin_color = case_when(
      RACACOR == 1 ~ "BRANCA",
      RACACOR == 2 ~ "PRETA",
      RACACOR == 3 ~ "AMARELA",
      RACACOR == 4 ~ "PARDA",
      RACACOR == 5 ~ "INDÍGENA",
      is.na(RACACOR) ~ NA_character_
    )
  )
df_csv %>% count(RACACOR)
df_csv %>% count(skin_color)

# remove coluna duplicada
df_csv$gender_pattern <- NULL

#transformar no formato Date
df_csv <- df_csv %>% 
  mutate (
    date_death = dmy(DTOBITO)
  )

# separar o ano
df_csv <- df_csv %>% 
  mutate(
    year_death = year(date_death)
  )

# contagem por ano e sexo
df_csv %>% count(gender, year_death)  

#contagem por cor da pele e sexo
df_csv %>% count(gender, skin_color)

library(stringr)

# determinar idade dos participantes
df_csv <- df_csv %>% 
  mutate(
    type_age = str_sub(IDADE, 1, 1), # separa o primeiro digito dos d+
    age = str_sub(IDADE, 2), 
    years_old = case_when(
      type_age <= 3 ~ 0, # menor que 1 ano
      type_age == 4 ~ as.numeric(age), #própria idade
      type_age == 5 ~ as.numeric(age) + 100 # maior de 100 anos
    )
  )

# total de mortes e média de idade por mês
db_groupby <- df_csv %>% 
  mutate(
    mes_obito = month(date_death, label = TRUE)) %>% 
    group_by(mes_obito) %>% 
    summarise(
      total_obitos = n(),
      idade_media = round(mean(years_old, na.rm = TRUE), digits = 2)
    )

View(db_groupby)


