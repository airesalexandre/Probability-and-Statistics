library(readxl)
library(ggplot2)
#Leitura do ficheiro
wine_data <- read_excel("C:/Users/Pc Ryzen Pro Gamer/Desktop/projeto-R/wine_prod_EU.xlsx")
#Eliminar linhas com Category em falta ou Product Group igual a "Non-Vinified"
wine_data <- subset(wine_data, !is.na(Category) & `Product Group` != "Non-Vinified")
#Filtrar o ano 2000
wine_2000 <- subset(wine_data, Year == 2000)
#Agrupa os países
wine_2000$CountryGroup <- ifelse(
  wine_2000$`Member State` %in% c("France", "Italy", "Spain", "Germany", "Portugal"),
  wine_2000$`Member State`, "Others")
#Criação do gráfico de barras
ggplot(wine_2000, aes(x = Category, y = Production, fill = CountryGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Wine Production by Category and Country (Year 2000)",
    x = "Wine Category",
    y = "Production (10³ hL)",
    fill = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))