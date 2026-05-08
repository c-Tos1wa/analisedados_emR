library(tidyverse)

# teste de proporção
# hipótese: a prevalência de hipertensão nesta população difere de 25%?

# dados: na amostra desta população de 200 pessoas, 60 são hipertensos.
p = 60/200

# H0: ρ = 0.25
# H1: ρ ≠ 0.25
prop.test(
  x = 60,
  n = 200,
  p = 0.25
)

# p-value = 0.1208, significa que não há evidência
# de que a proporção seja diferente de 25%