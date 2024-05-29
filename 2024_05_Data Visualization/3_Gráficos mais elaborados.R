# --------------------------> IMPORTANDO OS DADOS
#library("readxl")
imov <- read_excel("imoveis_cidade_sao_paulo_2019.xlsx")


# --------------------------> EXPLORANDO OS DADOS
#Visualizando as observações 
str(imov)

#Estatística Univariadas
summary(imov)
class(imov$elevador)

#Mudando a classe das variáveis categóricas de 'character' para 'factor'
imov$elevador <- as.factor(imov$elevador)
imov$novo <- as.factor(imov$novo)
imov$piscina <- as.factor(imov$piscina)
imov$mobilia <- as.factor(imov$mobilia)

summary(imov)


# --------------------------> SELECT & FILTER

#library(dplyr) #pacote para manipulação de dados
# Somente para valores de aluguel
imov_2 <- dplyr::select(imov, preco, cond, area, quarto, banheiro, 
                        bairro, vagas.garagem, novo, elevador, tipo) |> 
  filter(tipo == "venda", quarto < 6, novo==0)

summary(imov_2)

#Temos valores nulos?
is.na(imov_2)
sum(is.na(imov_2)) #quantidade de NA's

imov_2 <- na.omit(imov_2)
summary(imov_2) 

# --------------------------> HISTOGRAMAS

#Histograma da variável preço
#library(ggplot2)
#library(tidyverse)
imov_2 |>
  ggplot(aes(x = preco)) +
  geom_histogram(bins = 15, color = "black", fill = "tomato") +
  labs(x = "Preço de venda", y = "Frequência absoluta")

#Histograma da variável preço
imov_2 |>
  ggplot(aes(x = preco, y = (..count../sum(..count..))*100 )) +
  geom_histogram(bins = ,
                 color = "black",
                 fill = "#00CED1") +
  labs(x = "Preço do aluguel",
       y = "Frequência Relativa (%)")


# Poderia ser utilizada para criar informações mais específicas sobre o dataset
# Para isto, o "summarise" é utilizado em conjunto com a função "group by"
# A seguir, vamos agrupar as informações do dataset pelo critério de "bairro"

#Estatísticas Descritivas
descritivas_grupo_bairro <- group_by(imov_2, bairro) |> summarise(
  n_obs=n(),
  média=mean(preco),
  mediana=median(preco),
  desv_pad=sd(preco),
  mínimo=min(preco),
  máximo=max(preco),
  quartil_1=quantile(preco, type=5, 0.25),
  quartil_3=quantile(preco, type=5, 0.75))
View(descritivas_grupo_bairro)
#valores médios de preço, condomínio, quartos e banheiros agrupados por bairro
medias_grupo_bairro <- group_by(imov_2, bairro) |> 
  summarise(
    preco=mean(preco),
    cond=mean(cond),
    area=mean(area),
    quarto=mean(quarto),
    banheiro=mean(banheiro),
    vagas.garagem=mean(vagas.garagem))


#Gráfico de barra (barplot) dos valores médios de preço agrupados por bairro
ggplot(data=descritivas_grupo_bairro,
       aes(x = bairro, y = média, fill = bairro)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Bairro', y = 'Preço médio dos alugueis em São Paulo') +
  guides(fill = "none") +
  theme_light() 

#Gráfico de barra HORIZONTAL dos valores médios de preço
ggplot(data=descritivas_grupo_bairro,
       aes(x = bairro, y = média, fill = bairro)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Bairro', y = 'Preço médio dos alugueis em São Paulo') +
  guides(fill = FALSE) +
  theme_light() +
  coord_flip()


# --------------------------> FILTER & MUTATE
imov_3 <- filter(imov, 
                 tipo == "venda", quarto < 5, novo==0,
                 bairro == c("Butantã/São Paulo",
                             "Jabaquara/São Paulo",
                             "Itaquera/São Paulo",
                             "Brás/São Paulo",
                             "Capão Redondo/São Paulo")) |> 
  mutate(
    bairro = replace(bairro, bairro=="Butantã/São Paulo", 
                     "Butantã"),
    bairro = replace(bairro, bairro=="Jabaquara/São Paulo", 
                     "Jabaquara"),
    bairro = replace(bairro, bairro=="Itaquera/São Paulo", 
                     "Itaquera"),
    bairro = replace(bairro, bairro=="Brás/São Paulo", 
                     "Brás"),
    bairro = replace(bairro, bairro=="Capão Redondo/São Paulo", 
                     "Capão Redondo"))

#Mudando a classe das variável bairro de 'character' para 'factor'

summary(imov_3)

imov_3$bairro <- as.factor(imov_3$bairro)

class(imov_3$bairro)

imov_3 <- na.omit(imov_3)

summary(imov_3)

#library(ggridges)
#library(viridis)
#library(hrbrthemes)

# Plot da distribuição de densidade de probabilidade
ggplot(imov_3, aes(x = preco, y = bairro)) +
  geom_density_ridges_gradient(aes(fill = ..x..)) +
  scale_fill_gradientn(colours = c("red","pink","green"))

#library(RColorBrewer)
#display.brewer.all()
ggplot(imov_3, aes(x = preco, y = bairro)) +
  geom_density_ridges_gradient(aes(fill = ..x..)) +
  scale_fill_gradientn(colours = brewer.pal(9,'YlOrRd')) +
  labs(x = "Preço de Venda", y = "Densidade de Probabilidade", fill = 'R$', 
       title = 'Preço (venda) dos imoveis em São Paulo -  2019') +
  theme_minimal()

#ggsave(filename = 'hist_preço de venda_imoveis São Paulo.png',
#       height = 12,
#       width = 14,
#       units = 'cm',
#       bg = 'white')


# --------------------------> GRÁFICO BOXPLOT
ggplot(imov_3, aes(y = preco, x = bairro)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(aes(fill = bairro), width = 0.6,
               outlier.shape = 1, outlier.size = 2) +
  labs(x = "bairro", y = "preço do aluguel (R$)") +
  stat_summary(aes(shape = "média"),
               geom = "point",
               fun.y = mean,
               size = 2) +
  theme_classic()

#library(fields)    # Fornece paleta de cores 
ggplot(imov_3, aes(y = preco, x = bairro))+ #Definindo os dados  
  geom_boxplot(outlier.shape = NA)+ # Plotando o boxplot
  geom_jitter(aes(fill=preco), #Adicionando os pontos e colorindo em fun??o da temperatura
              position=position_jitter(width = .2), # Ajustando o espalhamento horizontal dos dados
              shape=21, #Definindo o tipo de pontos
              col='grey30', #Definindo cor de contorno dos pontos
              alpha=.5)+ # Definindo a opacidade dos pontos
  labs(y='Preço do alugel em São Paulo', #Definindo título do eixo y
       x=NULL, #Definindo título do eixo x
       fill='Preço do alugel')+ #Definindo título da legenda 
  scale_fill_gradientn(colours = tim.colors(20))+ # Defindo scala de cores para os pontos
  theme_bw()+ #Definindo tema 
  theme(panel.grid.major.x = element_blank()) #Apagando as grades verticais do gráfico


# --------------------------> GRÁFICO VIOLINO + BOXPLOT

ggplot(imov_3,aes(bairro, preco))+ #informando os dados e as colunas que serão plotadas
  geom_violin(trim = FALSE,fill='aliceblue',col='cornflowerblue')+ #Cria um vioplot para cada bairro
  geom_boxplot(width=.15,fill='blue',outlier.shape = 21, #Cria um boxplot para cada bairro
               outlier.fill = 'skyblue',outlier.size = 2.5)+
  stat_boxplot(geom = 'errorbar',width=.15)+ #Adicionando as barras de erro nos boxplots
  labs(y='Preço do alugel em São Paulo', #Definindo título do eixo y
       x=NULL, #Definindo título do eixo x
       fill='Preço do alugel')+ #Definindo título da legenda 
  theme_minimal() #Definindo um tema para o gráfico



# --------------------------> GRÁFICO DE BOLHAS (BUBBLE PLOT)
ggplot(imov_3, aes(x = area, y = preco, size = cond, color = bairro)) +
  geom_point(alpha = 0.6) + # Adiciona os pontos
  scale_size_continuous(range = c(2, 11)) + # Define o tamanho dos pontos
  labs(title = "Preço vs. Área (bolhas: cond (R$))", x = "Área do Imovel (m^2)",
       y = "Preço de Venda (R$)") + # Adiciona rótulos
  theme_minimal() # Define o tema do gráfico



#-----------GRÁFICO ITERATIVO


# Supondo que 'df' é o seu dataframe com as colunas 'IGEST', 'Comp', 'Massa' e 'IGM'
# Vamos criar uma coluna de cores baseada em 'IGM'
df <- imov_3
df$color <- ifelse(df$elevador == '1', 'red', 'blue')

# Criar o gráfico 3D interativo
p <- plot_ly(data = df, x = ~area, y = ~cond, z = ~preco, 
             type = 'scatter3d', mode = 'markers',
             marker = list(size = 5, color = ~color, opacity = 0.6))

# Adicionar eixos e título
p <- p |> layout(scene = list(
  xaxis = list(title = 'Área (m^2)'),
  yaxis = list(title = 'Cond (R$)'),
  zaxis = list(title = 'Preço (R$)'),
  title = '3D Scatter Plot: Área, Cond e Preço por Elevador'
))

# Salvar o gráfico como HTML
htmlwidgets::saveWidget(p, 'Interactive3DPlot.html')

# Imprime o gráfico no ambiente R (RStudio Viewer ou similar)
p



#-----------GRÁFICO ITERATIVO - MODELO AJUSTADO


# Carregando a função "plot_grafico_barras"
source("Function_plot_3d_2.R")

#MODELO 2
model.preco <- lm(formula = preco ~  cond + area, 
                  data = imov_3)

summary(model.preco)

# Gerar o gráfico 3D
plot_3d_surface_with_points(model.preco, imov_3, 
                            "cond", "area", "preco")








