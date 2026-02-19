library(dplyr)
library(ggplot2)
library(tidyverse)

# uso correto da escala
ipca <- tibble(
  ano = 2009:2013,
  valor = c(4.31, 5.92, 6.5, 5.84, 5.91)
)
ggplot(ipca, aes(x = factor(ano), y = valor)) +
  geom_col (fill = "gray60") +
  geom_hline(
    yintercept = 4.5,
    linetype = "dashed",
    colour = "magenta"
    ) +
  annotate(
    "text",
    x = 4.7,
    y = 4.7,
    label = "meta 4.5%",
    size = 3.05
  ) +
  labs(
    title = "IPCA no Brasil",
    subtitle = "Eixo y iniciando no 0 possibilita comparações",
    x = "Ano",
    y = "IPCA (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)))

#######################################################################

# uso adequado das cores
# IRR -> Incidence Rate Ratio (valores > 1 aumento de risco)

dengue <- tibble(
  desfecho = rep(c("AVC hemorrágico", "AVC isquêmico", 
                   "infarto agudo", "insuficiência cardíaca"), 2),
  periodo = rep(c("dias 1-7", "dias 8-14"), each = 4),
  IRR = c(10.9, 15.56, 13.53, 27.24,
          4.33, 3.17, 1.16, 2.45)
)

incidence <- dengue %>%
  mutate(desfecho = fct_reorder(desfecho, IRR, .fun = max, .desc = TRUE)) %>%
  ggplot(aes(desfecho, IRR, colour = periodo)) +
  geom_point(position = position_dodge(width = .5), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "gray50") +
  scale_colour_manual(
    values = c("dias 1-7" = "#E66100", "dias 8-14" = "#5D3A9B"),
    name = "Período após infecção"
  ) +
  labs(
    title = "Razão  da taxa de incidência por período após dengue",
    subtitle = "Linha vertical indica região sem aumento de risco",
    x = "",
    y = "razão da taxa de incidência (IRR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
                  legend.position = "bottom"
  )
incidence

###############################################################################

#gradiente de cor para variáveis contínuas
consumo <- mtcars %>% 
  rownames_to_column("modelo") %>% 
  mutate(montadora = word(modelo, 1)) %>% 
  group_by(montadora) %>% 
  summarise(mpg_mean = mean(mpg)) %>% 
  arrange(mpg_mean) %>% 
  head(10)

montadora_mpg <- consumo %>% 
  mutate(montadora = fct_reorder(montadora, mpg_mean)) 

ggplot(
  montadora_mpg, 
  aes(montadora, mpg_mean, fill = mpg_mean)) +
  geom_col() +
  scale_fill_gradient(
    high = "#2C3E50", low = "#3498DB", guide = "none"
  ) +
  coord_flip() +
  labs(
    title = "Consumo médio por montadora",
    x = "",
    y = "milhas por galão (média)"
  ) +
  theme_minimal()
#######################################################################

ds <- tibble(
  set = c("a", "b", "c", "d"),
  n = c(5, 15, 60, 20))

# evitar, mas se for necessário usar, colocar as porcentagens 
graph <- ds %>% 
  ggplot(aes(x = "", y = n, fill = set)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(
    aes(label = paste0(n, "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Gráfico de pizza",
    subtitle = "Sempre incluir porcentagens!") +
  theme_void() +
  theme(legend.position = "bottom")
graph

# melhor com gráfico de barras horizontais
bar_chart <- ds %>% 
  mutate(grupo = fct_reorder(set, n)) %>% 
  ggplot(aes(grupo, n, fill = grupo)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(
    aes(label = paste0(n, "%")),
    hjust = -0.1,
    vjust = 0.15,
    size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = " Alternativa ao grafico de pizza",
    x = "",
    y = "porcentagem em %") +
    theme_minimal() +
    ylim(0, 70)
bar_chart  

#########################################################################

# gráfico de linha (time series)
set.seed(42)
influenza <- tibble(
  week = 1:52,
  year = 2023,
  cases = round(50 + 30*sin((week - 10)*2*pi / 52) + rnorm(52, 0 ,10))
) %>% 
  mutate(cases = pmax(cases, 5)) #modifica para q os valores de casos seja maior ou igual a 5

ggplot(influenza, aes(week, cases)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_point(colour = "blue", size = 1.5) +
  labs(
    title = "Casos de influenza por semana no ano de 2023",
    x = "semana epidemiológica",
    y = "número de casos"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 52, by = 4)) +
  scale_y_continuous(limits = c(0, NA))
