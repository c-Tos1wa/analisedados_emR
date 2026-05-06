library(tidyverse)

# t de Student para uma amostra
# Hipótese: A média de peso de recém-nascidos deste hosptial difere de 3200g?

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
