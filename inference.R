library(tidyverse)

setwd("C:/Users/crist/Desktop/r_dados/curso-analise-de-dados-main/dados")

# Teorema Central do Limite
set.seed(42)
average_pop <- 26
sd_pop <- 2
k <- 500 #500 amostras diferentes
n <- 100 # cd amostra contém 100 pessoas

average_sample <- replicate(
  k,
  mean(rnorm(n, average_pop, sd_pop))
)
head(average_sample)

#histograma
hist(average_sample,
     col = "steelblue",
     border = "white",
     main = "Distribuição das médias amostrais (n=100)",
     xlab = "média do IMC (kg/m²)",
     ylab = "frequência",
     breaks = 30
)
abline(v = average_pop, col = "darkred", lwd = 2.5)

mean(average_sample)
sd(average_sample)
std_error = sd_pop / sqrt(n)
std_error


# TCL em população assimétrica
set.seed(123456)
pop <- rexp(10000, rate = 1)
hist(
  pop,
  col = "steelblue",
  border = "white",
  main = "Distribuição assimétrica",
  breaks = 30
)

mean_sample <- function(ssample, n_sample = 1000){
  replicate(
    ssample,
    mean(sample(pop, n_sample))
    )
}

par(mfrow = c(2,2))
hist(
  pop,
  main = "população assimétrica",
  col = "lightblue",
  border = "black",
  breaks = 30
)
hist(
  mean_sample(5),
  main = "médias de tamanho 5",
  col = "steelblue",
  border = "black",
  breaks = 30
)
hist(
  mean_sample(30),
  main = "médias de tamanho 30",
  col = "lightgreen",
  border = "black",
  breaks = 30
)
hist(
  mean_sample(100),
  main = "médias de tamanho 100",
  col = "green",
  border = "black",
  breaks = 30
)
par(mfrow = c(1,1))



# intervalo de confiança
z <- qnorm(0.975) # z-score
std_error
average_sample
average_pop

df_ic <- tibble(
  sample = 1:k,
  mean_ic = average_sample,
  li = mean_ic - z * std_error,
  ls = mean_ic + z * std_error,
  has_mean = li < average_pop & ls > average_pop
)
df_ic

table(df_ic$has_mean) # calcula a qtos intervalos contem a media vdd

df_ic %>% 
  slice(1:50) %>%
  ggplot(aes(x = sample, y = mean_ic, color = has_mean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.4) +
  geom_hline(
    yintercept = average_pop,
    color = "black",
    linewidth = 0.6
      ) +
  scale_color_manual(
    values = c("TRUE" = "darkblue", "FALSE" = "darkred")
  ) +
  labs(
    title = "Intervalo de confiança de 95%",
    subtitle = "Linha horizontal = média populacional (26)",
    x = "amostra",
    y = "IMC(kg/m²)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
