library(tidyverse)

# t de Student para uma amostra
# Hipótese: A média de peso de recém-nascidos deste hosptial 
# difere de 3200g?

#weight = c(3265, 3260, 3245, 3484, 4146, 3323, 3649,
           # 3200, 3031, 2069, 2581, 2841, 3609, 2838,
           # 3541, 2759, 3248, 3314, 3101, 2834)
weight = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 
             3031, 2069, 2581, 2841, 3609, 2838, 3541, 2759, 
             3248, 3314, 3101, 2834)

length(weight)

# estatistica descritiva
summary(weight)
mean(weight)
sd(weight)


# H0: μ = 3200
# H1: μ ≠ 3200
t.test(weight, mu = 3200)
# resultado: p-value = 0.7432, ou seja, não há evidência 
# que a média difere de 3200g, pois p-value > 0.05


# t de student p/ 2 amostras independentes
# Hipótese: Média de colesterol difere entre dois métodos de medição?

colesterol <- tibble(
  method = rep(c("AutoAnalyzer", "MicroEnzimatic"), each = 5),
  value = c(177, 193, 195, 209, 226,
            192, 197, 200, 202, 209)
)
View(colesterol)

#estatisticas de cada método
colesterol %>% 
  group_by(method) %>%
  summarize(
    total = n(),
    media = mean(value),
    desvio = sd(value)
  )
# H0: μ1 = μ2 
# H1: μ1 ≠ μ2
t.test(value ~ method, data = colesterol)
# o termo 'value ~ method' significa 
# 'compare os valores entre os métodos'
# resultado: p-value = 1, significa que os métodos são
# estatisticamente equivalentes, pois p-value > 0.05

#ANOVA: 3 ou mais grupos
# Hipótese: comparando a pressão arterial de 3 faixas etárias,
# elas serão iguais?

set.seed(123)

bp_data <- tibble(
  age = factor(
    rep(c("jovem", "adulto", "idoso"), each = 20),
    levels = c("jovem","adulto", "idoso")
  ),
  pressure = c(
    rnorm(20, mean = 115, sd = 10),
    rnorm(20, mean = 125, sd = 12),
    rnorm(20, mean = 135, sd = 15)
  )
)
head(bp_data, n=5)

#estatísticas da amostra
bp_data %>%
  group_by(age) %>%
  summarise(
    total = n(),
    media = mean(pressure),
    desvio = sd(pressure)
  )

# gráfico boxplot
bp_data %>% 
  ggplot(aes(age, pressure, fill = age)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.4, show.legend = FALSE) +
  labs(
    title = "Pressão Arterial por Faixa Etária",
    x = "",
    y = "PA(mmHG)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# ANOVA
anova <- aov(pressure ~ age, data = bp_data)
summary(anova)
# resultado ANOVA significativa, pois Pr(<F) < 0.05.


# teste de Tukey
TukeyHSD(anova)
# resultado: mostra que os grupos com media diferentes são
# idoso-jovem e idoso-adulto, por causa do p adj < 0.05