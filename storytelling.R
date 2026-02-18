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
