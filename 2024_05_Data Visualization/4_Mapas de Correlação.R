##########################################################################
#                       CORRELAÇÃO                                       #
##########################################################################


# --------------------------> FUNÇÃO SELECT

#criando um novo dataset a partir do dataset "imov"
cor(select(imov, area, preco))

#recortando as variáveis área e preço do dataset "imov_2"
cor(select(imov_2, area, preco))


# --------------------------> MAPA DE CORRELAÇÃO

#library(corrgram)
corrgram(imov,
         lower.panel = panel.pie, 
         upper.panel = panel.conf, 
         diag.panel = panel.density)

#library(GGally)
ggcorr(imov,label = T)

#library(pheatmap)
MC <- na.omit(imov) |> 
  select(preco, cond, area, quarto, banheiro, 
         suite, vagas.garagem) |> 
  cor()

#library(corrplot)
corrplot(MC, method = "circle",tl.pos = "y")
corrplot.mixed(MC,
               lower = "circle", 
               upper = "number",
               tl.col = "black")


# Somente para valores de venda
imov_4 <- select(imov_2, preco, area, cond, elevador, novo)
summary(imov_4)

corrgram(imov_4,
         lower.panel = panel.pts, 
         upper.panel = panel.conf, 
         diag.panel = panel.density)


ggcorr(imov_4,label = T)

#A função chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
#library(PerformanceAnalytics)
chart.Correlation((imov_4[1:3]), histogram = TRUE)

#library(GGally)
ggpairs(imov_4,columns=1:3,lowe = list(continuous= "smooth"))
ggpairs(imov_4,columns=1:3,aes(colour=elevador))

#library(pheatmap)
pheatmap(cor(select(imov_2, area, preco, cond)))

# --------------------------> FILTER & MUTATE
imov_5 <- select(imov, preco, area, cond, bairro,quarto, novo, 
                 elevador, tipo) |> 
  filter(tipo == "venda", quarto < 5,
         bairro == c("Jabaquara/São Paulo","Itaquera/São Paulo")) |> 
  mutate(bairro = replace(bairro, bairro=="Jabaquara/São Paulo", 
                          "Jabaquara"),
         bairro = replace(bairro, bairro=="Itaquera/São Paulo", 
                          "Itaquera")) |> na.omit()

imov_5$bairro <- as.factor(imov_5$bairro)
summary(imov_5)

ggpairs(imov_5,columns=1:3,aes(colour=bairro))
ggpairs(imov_5,columns=1:4,aes(colour=bairro))


#Carregando Script que possui a função geom_split_violin()
source('scripts/split_violin_ggplot.R')

ggplot(imov_5, aes(x = bairro, y = preco, fill=elevador)) + #Define dados, eixos e cores
  geom_split_violin( trim = FALSE,na.rm = T,lwd=.3) + #Adiciona as densidades de probabilidade (violin plot)
  scale_fill_manual(values =c('#7FBBAE','#D6C25C'),
                    labels=c("0","1"))+ # Define cores e rótulos
  labs(subtitle = "Densidade de probabilidade", # Define título e rótulos
       fill="Elevador",
       y='Preço de Venda',
       x=NULL) +
  theme_bw() # Definindo o Tema


