# UNIVERSIDADE FEDERAL DO ABC
# INTRODUÇÃO À INFERÊNCIA ESTATÍSTICA - AVALIAÇÃO A2
# PROF. Ailton Paulo de Oliveira Júnior
# José Juliano Amorim Silva - RA: 11202020074
# Luiza Bistane Spetic - RA: 11201921944

# ESTUDO DA PREDOMINANCIA DE CASOS DE VIOLENCIA INTRAFAMILIAR EM ESTUDANTES DO NONO ANO DO ENSINO FUNDAMENTAL

# Carregar as bibliotecas necessárias
library(ggplot2)

# Importar os dados que serão utilizados
dados <- read.csv2("dataset.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
rownames(dados) <- dados$Capital
dados <- dados[,-1]
View(dados)

# PERGUNTA 1 — Há diferença no percentual de casos de violência intrafamiliar entre a rede pública e a rede privada de ensino?

# Visualizar a distribuição dos dados de Homens
ggplot(dados, aes(x = Homens)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = 'blue', alpha = 0.7) +
  geom_density(alpha = .2, fill = "blue") +
  labs(title = "Homens",
       x = "Valores",
       y = "Densidade") +
  theme(axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))

# Visualizar a distribuição dos dados de Mulheres
ggplot(dados, aes(x = Mulheres)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = 'red', alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Mulheres",
       x = "Valores",
       y = "Densidade") + 
  theme(axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))

# Teste de normalidade de Shapiro-Wilk dos dados de Homens
shapiro.test(dados$Homens)

# Teste de normalidade de Shapiro-Wilk dos dados de Mulheres
shapiro.test(dados$Mulheres)

# Visualizar os dados com boxplot
boxplot(dados$Homens, dados$Mulheres, names = c("Homens", "Mulheres"),
        main = "Boxplot para Homens e Mulheres",
        ylab = "Valores",
        col = c("blue", "red"))

# Teste F de comparação de variâncias
var.test(dados$Homens, dados$Mulheres, alternative = "two.sided", conf.level = 0.95)

# Teste t comparação de médias
t.test(dados$Mulheres, dados$Homens, var.equal = TRUE, alternative = "greater")

# PERGUNTA 2 — Há diferença no percentual de alunos agrediso entre a rede pública e a rede privada de ensino?

# Visualizar a distribuição dos dados da rede Pública
ggplot(dados, aes(x = Publica)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = 'green', alpha = 0.7) +
  geom_density(alpha = .2, fill = "green") +
  labs(title = "Pública",
       x = "Valores",
       y = "Densidade") + 
  theme(axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))

# Visualizar a distribuição dos dados da rede Privada
ggplot(dados, aes(x = Privada)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = 'purple', alpha = 0.7) +
  geom_density(alpha = .2, fill = "purple") +
  labs(title = "Privada",
       x = "Valores",
       y = "Densidade") + 
  theme(axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))

# Teste de normalidade de Shapiro-Wilk dos dados da rede Publica
shapiro.test(dados$Publica)

# Teste de normalidade de Shapiro-Wilk dos dados da rede Privada
shapiro.test(dados$Privada)

# Visualizar os dados com boxplot
boxplot(dados$Privada, dados$Publica, names = c("Privada", "Pública"),
        main = "Boxplot para as redes Privada e Pública",
        ylab = "Valores",
        col = c("green", "purple"))

# Teste F de comparação de variâncias
var.test(dados$Publica, dados$Privada, alternative = "two.sided", conf.level = 0.95)

# Teste t de comparação de médias
t.test(dados$Publica, dados$Privada, var.equal = TRUE, alternative = "greater") 

# PERGUNTA 3 —  Há associação entre porcentagem de casos de violência intrafamiliar e ano?

# Importar os dados que serão utilizados
dados_anos <- read.csv2("dataset-anos.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
View(dados_anos)

#Gráfico de dispersão
plot(dados_anos$ANO, dados_anos$QTDE, 
     main = "Gráfico de Dispersão de QTDE por ANO", 
     xlab = "Ano", 
     ylab = "Quantidade (%)",
     pch = 19,     # Tipo de ponto
     col = "blue")

#Regressão linear
summary(lm(QTDE ~ ANO, data = dados_anos))
