install.packages("datasauRus")
install.packages("tidyverse")
library(dplyr)
library(datasets)
library(ggplot2)
library(datasauRus)
library(tidyverse)


head(datasaurus_dozen)
head(anscombe)

#estatísticas do dataset datasauRus
datasaurus_dozen %>%
  group_by(dataset) %>% 
  summarize(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y),
    corr_x_y = cor(x, y)
  )

#gráfico do dataset datasauRus
ggplot(datasaurus_dozen, aes(x, y)) +
  geom_point(colour = "#dc3220") +
  theme_minimal() +
  facet_wrap(~dataset, ncol = 5) + 
  labs(
    title = "Datasaurus dozen",
    subtitle = "Conjunto de dados com mesmas estatísticas descritivas",
    x = "",
    y = ""
  )

dino <- datasaurus_dozen %>% filter(dataset == "dino")

  ggplot(dino, aes(x, y)) +
  geom_point(colour = "darkgreen", size = 3) +
  labs(
    title = "Dinossauro nos dados",
    subtitle = paste0(
      "média x: ", round(mean(dino$x), 2),
      " | média y: ", round(mean(dino$y),2),
      " | correlação: ", round(cor(dino$x, dino$y),2)),
      x = "",
      y = ""
      ) + 
      theme_minimal(paper = "darkgrey")

#estatísticas de anscombe
ans_pivot <- anscombe %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "conjunto"),
    names_pattern = "(.)(.)") %>%
  mutate(conjunto = paste("conjunto", conjunto))
View(ans_pivot)

ans_pivot_stats <- ans_pivot %>%
  group_by(conjunto) %>% 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    std_x = sd(x),
    std_y = sd(y),
    corr = cor(x,y)
  )
ans_pivot_stats

# grafico de anscombe
graph_ans <- ans_pivot %>% 
  ggplot(aes(x, y)) +
  geom_point(color = "#E66100", size = 2, alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "#5D3A9B",
    linewidth = 0.5) +
  facet_wrap(~conjunto, ncol = 2) +
  labs(
    title = "Quarteto de Anscombe",
    x = "",
    y = "") +
  theme_minimal(paper="#A9A9A9")
graph_ans  
  
