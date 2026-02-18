
library(tibble)
recem_nascidos <- tibble(
  individuo = 1:20,
  peso_g = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 3031, 2069,
             2581, 2841, 3609, 2838, 3541, 2759, 3248, 3314, 3101, 2834))

View(recem_nascidos)

recem_nascidos <- recem_nascidos %>% 
  mutate(
    faixa_peso = case_when(
      peso_g < 2500 ~ "baixo peso",
      peso_g >= 2500 & peso_g < 3500 ~ "peso ideal",
      peso_g >= 3500 ~ "acima do esperado"
    ),
    faixa_peso = factor(
      faixa_peso,
      levels = c("baixo peso", "peso ideal","acima do esperado"),
      ordered = TRUE
    )
  )
recem_nascidos %>% count(faixa_peso)

media <- recem_nascidos %>% 
  summarize(
    media_peso = mean(peso_g)
  ) %>% 
  pull(media_peso)
media

mediana <- recem_nascidos %>% 
  summarize(
    mediana = median(peso_g) 
  ) %>% 
  pull(mediana)
mediana

assimetria = media - mediana 
# neste caso, cauda para esquerda

quantil = recem_nascidos %>% 
  summarize(
    p10 = quantile(peso_g, 0.1),
    p25 = quantile(peso_g, 0.25),
    p50 = quantile(peso_g, 0.5),
    p75 = quantile(peso_g, 0.75),
    p90 = quantile(peso_g, 0.9)
  )
################################################################################  
analises <- tibble(
  metodo = rep(c(" Autoanalizer", "Microenzymatic"), each = 5),
  valores = c(177, 193, 195, 209, 226, 192, 197, 200, 202, 209)
)

#amplitude
amplitude = analises %>%
  group_by(metodo) %>% 
  summarize(
    min = min(valores),
    max = max(valores),
    amp = max - min
  )
amplitude

#variabilidade (desvio padrão e variância)
desvio <- analises %>% 
  group_by(metodo) %>% 
  summarize(
    dp = sd(valores),
    variancia = var(valores),
    media = mean(valores)
  )
desvio

#coeficiente de variação
cv <- desvio %>% 
  mutate(
    cv_percentual = (dp/media)*100
  ) %>% 
  select(metodo, cv_percentual)
cv

#intervalo interquartil
iq <- analises %>% 
  group_by(metodo) %>% 
  summarize(
    q1 = quantile(valores, 0.25),
    q3 = quantile(valores, 0.75),
    IQ = q3 - q1
  ) 
iq

#função para mostrar principais estatísticas
estatisticas <- function (dados, colunas) {
  dados %>%
    summarize(
      total = n(),
      media = mean({{ colunas }}),
      mediana = median({{ colunas }}),
      variancia = var({{ colunas }}),
      desvio = sd({{ colunas }}),
      cv = (desvio/media) * 100,
      amplitude = max({{ colunas }}) - min({{ colunas }}),
      Q1 = quantile({{ colunas }}, 0.25),
      Q3 = quantile({{ colunas }}, 0.75),
      IQ = Q3 - Q1   
    )
}
estatisticas(recem_nascidos, peso_g)

################################################################################

#gráficos usando ggplot
library(ggplot2)

#grafico de barras em percentual
grafico_bar <- recem_nascidos %>% 
  count(faixa_peso) %>%
  mutate(percent = (n / sum(n)) * 100) %>% 
  ggplot(aes(faixa_peso, percent)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = paste0(percent, "%")), # label sobre a barra
            vjust = -0.3, size = 4) +
  labs(
    title = "Distribuição percentual (%) de recém nascidos por faixa de peso",
    x = "faixa de peso",
    y = ""
  ) +
  #theme_minimal() +
  ylim(0, 100) #limite lateral
grafico_bar

# gráfico boxplot
graphic_boxplot <- analises %>% 
  ggplot(aes(metodo, valores, fill = metodo)) +
  geom_boxplot() +
  labs(
    title = "Comparação entre métodos de medição de colesterol",
    x = "",
    y = "colesterol (mg/dL)"
  ) +
  theme_minimal(paper = "lightgrey", base_line_size = 0.4) +
  scale_fill_manual(values = c("forestgreen", "darkviolet"))
graphic_boxplot

##################################################################################

#histograma
histograma <- recem_nascidos %>% 
  ggplot(aes(peso_g)) +
  geom_histogram(
    bins = 6,
    fill = "forestgreen",
    color = "black"
  ) +
  geom_vline(
    aes(xintercept = mean(peso_g)),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = median(peso_g)),
    color = "yellow",
    linetype = "dotted",
    linewidth = 1,
  ) +
  labs(
    title = "Histograma do peso de recém nascidos",
    subtitle = "linha vermelha = média | linha amarela = mediana",
    x = "peso (g)",
    y = "frequência"
  ) + 
  theme_minimal(accent = "darkgrey")
histograma