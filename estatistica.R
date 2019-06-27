install.packages("scatterplot3d") # Instala o pacote ScatterPlot3D
install.packages("tidyverse")
install.packages("data.table")
install.packages("car")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("ggthemes")
vinstall.packages("scales")
install.packages("dplyr")
install.packages("VIM")
install.packages("data.table")
install.packages("formattable")
install.packages("plotly")
install.packages("corrplot")
install.packages("GGally")
install.packages("caret")
install.packages("leaps")
install.packages("MASS")

library(tidyverse)
library(data.table)
library(car)
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library("scatterplot3d") # Carrega o pacote ScatterPlot3D
library(leaps)
library(MASS)

#ler dados do csv
carros <- read.csv(("~/carros_honda.csv"), header=TRUE, sep=";",encoding="UTF-8")
summary(carros)

#verifica se as variaveis não numericas estao como factor
is.factor(carros$cidade)
is.factor(carros$estado)
is.factor(carros$marca)
is.factor(carros$modelo_carro)

#converte os factor para number para criar grafico de calor
carros$cidade <- as.numeric(carros$cidade)
carros$estado <- as.numeric(carros$estado)
carros$marca <- as.numeric(carros$marca)
carros$modelo_carro <- as.numeric(carros$modelo_carro)

#verifica visualmenete se converteu corretamente
summary(carros)

#cria mapa de calor
ggcorr(carros, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#le novamente os dados pois se tratar factor como numerico o modelo se ajusta de forma diferente 
carros <- read.csv(("~/carros_honda.csv"), header=TRUE, sep=";",encoding="UTF-8")

#regressao simples 1: preco X ano
summary(lm(carros$preco ~ carros$ano ))

plot(carros$preco,carros$ano)

carros %>%
  arrange(desc( ano)) %>%
  ggplot(aes(x=ano, y=preco)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "Ano", y = "Preco", title = "ano X Valor") +
  theme(plot.title = element_text(hjust = 0.5))

#regressao simples 2: preco X kms
summary(lm(carros$preco ~ carros$kms ))

carros %>%
  arrange(desc( ano)) %>%
  ggplot(aes(x=kms, y=preco)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "quilÃ´metro", y = "Preco", title = "Preco X Kms") +
  theme(plot.title = element_text(hjust = 0.5))

plot(carros$preco,carros$kms)

#regressao simples 3: preco X modelo_carro
summary(lm(carros$preco ~ carros$modelo_carro ))

plot(carros$preco,carros$modelo_carro)

carros %>%
  arrange(desc( ano)) %>%
  ggplot(aes(x=modelo_carro, y=preco)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "modelo_carro", y = "Preco", title = "Preco X modelo_carro") +
  theme(plot.title = element_text(hjust = 0.5))


boxplot(carros$preco ~ carros$modelo_carro,
        main="Boxplot de Preco por modelo_carro de carro",
        xlab="modelo_carro do carro", ylab="Preco",
        col=c("yellow","orange","red"))


#regressao multipla 1: preco X ano + kms
summary(lm(carros$preco ~ carros$ano + carros$kms ))

#plot(carros$preco,carros$ano,carros$kms)

scatter3d(carros$preco, carros$kms, carros$ano)

#regressao multipla 2: preco X ano + modelo_carro
summary(lm(carros$preco ~ carros$ano + carros$modelo_carro ))




#regressao multipla 3: Melhor Modelo - preco X ano + Kms + modelo_carro
linear_model = lm(carros$preco ~ carros$ano + carros$kms  + carros$modelo_carro)

summary(linear_model)

#residuos

plot(linear_model$residuals)

plot (linear_model, que = 1: 4)






#tentativa de executar seleção automatica de modelo_carro

carros$marca <- NULL # tem q tirar marca pois e um factor de apenas um nivel

# Fit the full model 
full.model <- lm(preco ~., data = carros)

summary(full.model)

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
