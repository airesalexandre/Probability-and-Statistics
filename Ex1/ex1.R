#Carregar apenas os pacotes permitidos
library(ggplot2)

#Ler os dados (usando R base)
wine_data <- read.csv("winequality-white-q5.csv", header = TRUE, sep = ",")

#Calcular raiz quadrada da densidade
wine_data$sqrt_density <- sqrt(wine_data$density)

#Criar gráfico: boxplot + pontos com jitter para reduzir sobreposição
ggplot(wine_data, aes(x = factor(quality), y = sqrt_density)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "darkred", size = 1) +
  labs(
    title = "Boxplots of sqrt(density) by wine quality",
    x = "Wine Quality (1 = Poor, 5 = Excellent)",
    y = "Square Root of Density"
  ) +
  theme_minimal(base_size = 12)
