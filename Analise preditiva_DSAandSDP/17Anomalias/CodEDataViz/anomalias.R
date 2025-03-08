# Data Science Academy
# Lab 8 - Detecção de Anomalias em Transações Financeiras com Linguagem R e Power BI

# Verifica a pasta de trabalho
getwd()
setwd ("C:/Users/patri/OneDrive/Documentos/cursoDSA/17Anomalias") 
# Instala os pacotes
install.packages("tidyverse")
install.packages("dplyr")
install.packages("solitude")
install.packages("ggplot2")
install.packages("readr")

# Carrega os pacotes nesta sessão R
library(tidyverse)
library(dplyr)
library(solitude)
library(ggplot2)
library(readr)

# Carrega os dados históricos
dados_historicos_dsa <- read_csv("dados_historicos.csv")
View(dados_historicos_dsa)

# Cria modelo de Machine Learning com isolationForest
#Aprendizado n supervisionado
#significa que o algoritmo vai buscar o padrão dos dados

# chama documentação
?isolationForest 

modelo_ml_dsa = isolationForest$new() 

# Treina o modelo
modelo_ml_dsa$fit(dados_historicos_dsa)

# Faz as previsões com o modelo usando os dados históricos
#concatena
previsoes_historico = dados_historicos_dsa %>%
  modelo_ml_dsa$predict() %>%
  arrange(desc(anomaly_score))

#id,average depth (média de profundidade)
View(previsoes_historico)

#ele percorre os dados, encontra a média coloca o que ta maior com mais anomalia
# Density Plot 
plot(density(previsoes_historico$anomaly_score))
#o que estiver com maior densidade, significa que é o normal, ou seja, a maior parte
# dos dados estão nesse meio, agora é definir a marca de corte.

# Quanto maior o anomaly score maior a chance do registro ser uma anomalia
# Vamos definir como regra que anomaly score acima de 0.62 é uma anomalia
#coloca em um indice que funciona como uma tabela temporaria, coloca os dados em uma caixinha
indices_historico = previsoes_historico[which(previsoes_historico$anomaly_score > 0.62)]

# Faz o filtro
#pega tudo que for maior de 0,62 de anomalia coloca em um indice
anomalias_historico = dados_historicos_dsa[indices_historico$id, ]
#pega tudo menos o que ta em anomalia,os normais abaixo de 0.62
normais_historico = dados_historicos_dsa[-indices_historico$id, ]

# Gráfico
colors()
?ggplot
ggplot() + 
  geom_point(data = normais_historico, 
             mapping = aes(transacao1,transacao2), 
             col = "gold", 
             alpha = 0.8) + 
  geom_point(data = anomalias_historico,
             mapping = aes(transacao1,transacao2), 
             col = "red2", 
             alpha = 0.9)

# Agora carregamos novos dados
getwd()
novos_dados_dsa <- read.csv("novos_dados.csv")
View(novos_dados_dsa)

# Previsões com o modelo treinado
previsoes_novos_dados = modelo_ml_dsa$predict(novos_dados_dsa)

# Se o anomaly score é maior que 0.62 consideramos como anomalia
indices_novos_dados = previsoes_novos_dados[which(previsoes_novos_dados$anomaly_score > 0.62)]

# Filtro
anomalias_novos_dados = novos_dados_dsa[indices_novos_dados$id, ]
normais_novos_dados = novos_dados_dsa[-indices_novos_dados$id, ]

# Gráfico das previsões
ggplot() + 
  geom_point(data = normais_novos_dados, 
             mapping = aes(transacao1,transacao2), 
             col = "turquoise3", 
             alpha = 0.5) + 
  geom_point(data = anomalias_novos_dados, 
             mapping = aes(transacao1,transacao2), 
             col = "tomato3", 
             alpha = 0.8)

View(previsoes_novos_dados)

# Arredondando a coluna 'anomaly_score' para 2 casas decimais
previsoes_novos_dados <- previsoes_novos_dados %>%
  mutate(anomaly_score = round(anomaly_score, 2))

View(previsoes_novos_dados)

# Criando uma nova coluna com base na condição
previsoes_novos_dados <- previsoes_novos_dados %>%
  mutate(status = ifelse(anomaly_score > 0.62, "anomalia", "normal"))

View(previsoes_novos_dados)

# Criando o box plot
ggplot(previsoes_novos_dados, aes(x = status, y = anomaly_score, fill = status)) +
  geom_boxplot() +
  labs(title = "Box Plot de Anomalias e Normais",
       x = "Status",
       y = "Anomaly Score") +
  theme_minimal() +
  scale_fill_manual(values = c("anomalia" = "red", "normal" = "blue")) +
  theme(legend.position = "none")

# Salva em disco
write.csv(previsoes_novos_dados, "previsoes_novos_dados.csv")


