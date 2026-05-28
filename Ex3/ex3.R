library(ggplot2)

#Leitura dos dados
dados <- read.csv("C:/Users/Pc Ryzen Pro Gamer/Desktop/projeto-R/clima.csv", stringsAsFactors = FALSE)
dados$Data <- as.POSIXct(dados$Data, format = "%Y-%m-%d %H:%M:%S")
dados <- dados[complete.cases(dados$Data), ]

#Filtrar para agosto de 2010
dados_agosto <- dados[format(dados$Data, "%Y-%m") == "2010-08", ]

#Mediana diária
dados_agosto$Dia <- as.Date(dados_agosto$Data)
medianas <- aggregate(Pressão ~ Dia, dados_agosto, median, na.rm = TRUE)
medianas$Data <- as.POSIXct(medianas$Dia)

#Criação do Gráfico
ggplot(dados_agosto, aes(x = Data, y = Pressão)) +
  geom_line(color = "blue", alpha = 0.6) +
  geom_line(data = medianas, aes(x = Data, y = Pressão), color = "red", size = 1) +
  labs(
    title = "Pressão em agosto de 2010",
    subtitle = "Variação horária (azul) e mediana diária (vermelho)",
    x = "Data", y = "Pressão (hPa)", caption = "Fonte: Kaggle"
  ) +
  theme_minimal()