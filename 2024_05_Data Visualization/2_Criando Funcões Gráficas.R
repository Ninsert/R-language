# --------------------------> IMPORTANDO OS DADOS

#https://archive.ics.uci.edu/dataset/14/breast+cancer
BC <- read_excel("BC.xlsx")


# --------------------------> EXPLORANDO OS DADOS
#Visualizando as observações 
str(BC)

#library(dplyr)
glimpse(BC)

#Estatística Univariadas
summary(BC)
class(BC$Class)

# Converter todas as variáveis de character para factor
BC <- data.frame(lapply(BC, as.factor))
summary(BC)
table(BC$Class)

# --------------------------> Gráfico de barra da variável Class

#library(ggplot2)
ggplot(BC, aes(x = Class)) + 
  geom_bar(fill = "green") +
  labs(title = "Frequência da Variável Binária",
       x = "Categoria",
       y = "Frequência") 

ggplot(BC, aes(x = Class, fill = Class)) + 
  geom_bar() +
  labs(title = "Frequência da Variável Binária",
       x = "Categoria",
       y = "Frequência") +
  scale_fill_manual(values = c("blue", "red"))  # Definindo as cores manualmente



# Função para plotar o gráfico de barras com estilo científico
plot_grafico_barras <- function(data, variavel) {
  # Definindo um tema de gráfico científico
  tema_cientifico <- theme_minimal() +
    theme(
      text = element_text(size = 12),  # Tamanho da fonte
      panel.grid.major = element_blank(),  # Remover linhas de grade principais
      panel.grid.minor = element_blank(),  # Remover linhas de grade secundárias
      axis.line = element_line(size = 0.5),  # Espessura da linha do eixo
      axis.text = element_text(color = "black"),  # Cor do texto do eixo
      axis.title = element_text(size = 14),  # Tamanho do texto do título do eixo
      plot.title = element_text(size = 16, face = "bold")  # Tamanho e estilo do título do gráfico
    )
  
  # Criando o gráfico de barras
  grafico <- ggplot(data, aes_string(x = variavel, fill = variavel)) + 
    geom_bar() +
    labs(title = paste("Frequência de", variavel),
         x = "Categoria",
         y = "Frequência") +
    scale_fill_manual(values = c("blue", "red")) +  # Definindo cores manualmente
    tema_cientifico
  
  return(grafico)
}

plot_grafico_barras(BC,"Class")



#----->Outro exemplo de uso da função com uma base de dados fictícia
dados <- data.frame(
  variavel_binaria = c("Sim", "Não", "Sim", "Não", "Sim", "Sim", "Não", "Não")
)

# Chamando a função para plotar o gráfico de barras
plot_grafico_barras(dados, "variavel_binaria")
