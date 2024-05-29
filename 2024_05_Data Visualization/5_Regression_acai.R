##########################################################################
#           REGRESSÃO LINEAR MÚLTIPLA - Exemplo 2 - Açaí                 #
##########################################################################

#Remove objetos da memória
#rm(list = ls())

# --------------------------> IMPORTANDO OS DADOS
acai <- read.table("acai.csv", sep = ";", dec = ",", header = T)

str(acai)
summary(acai)
class(acai$pag)

acai$dia <- as.factor(acai$dia)
acai$chuva <- as.factor(acai$chuva)
acai$fds <- as.factor(acai$fds)
acai$pag <- as.factor(acai$pag)
acai$insta <- as.factor(acai$insta)
acai$mes <- as.factor(acai$mes)

summary(acai)


#library(dplyr) #pacote para manipulação de dados
acai2 <- acai |> select(vendas, temp, dia, chuva, fds, pag, 
                      insta, mes) |> filter(mes == c("ago", "set"))

summary(acai2)

#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################
#Dummizando a variável "pag". O código abaixo, automaticamente, fará: a) o
#estabelecimento de dummies que representarão cada uma das regiões da base de 
#dados; b)removerá a variável dummizada original; c) estabelecerá como categoria 
#de referência a dummy mais frequente.
#library(fastDummies)
acai2.dummies <- dummy_columns(.data = acai2,
                                   select_columns = c("pag","dia","chuva","fds"),
                                   remove_selected_columns = T,
                                   remove_first_dummy = T)

summary(acai2.dummies)

# --------------------------> REGRESSÃO LINEAR MÚLTIPLA (RLM)

#Estimando a Regressão Linear Múltipla - MODELO 1
ols.acai <- lm(formula = vendas ~. -mes,
                  data = acai2.dummies)

summary(ols.acai)

#PROCEDIMENTO STEPWISE
ols.acai.step <- step(ols.acai, k = 3.841459)

summary(ols.acai.step)


#Estimando a Regressão Linear Múltipla - MODELO 2
acai.treino <- acai |> select(vendas, temp, chuva, 
                              fds, mes) |> filter(mes == c("ago", "set")) 

acai.treino <- acai.treino[,-5]

summary(acai.treino)

#MODELO 2
ols.acai2 <- lm(formula = vendas ~.  + chuva:fds, data = acai.treino)

summary(ols.acai2)

ggplot(acai.treino,aes(x=temp,y=vendas))+#Definindo os dados de os eixos dos gráficos
  geom_point(size=2.5,pch=21,col='brown4',fill='coral')+#Plotando os pontos e definindo suas características (tamanho e cores)
  geom_smooth(formula = 'y~x',method = 'lm',se=FALSE)+ #Plota a reta de regressão sobre os pontos
  stat_regline_equation()+ #Inserindo a equação de regressão sobre os pontos
  stat_cor(label.y.npc =0.85,
           aes(label= paste(..rr.label..,..p.label..,sep = '~')))+ # Inserindo o R^2 e o p-valor
  facet_wrap(facets = vars(fds),strip.position = 'right')+#Criando os gráficos e função do ano
  theme_light()+#Definindo o tema do gráfico
  theme(strip.background = element_rect(fill = "white",colour = 'grey'), #Definindo a cor de fundo de cada strip como branco
        strip.text = element_text(color = 'black',size = 12))+#Definindo o tamanho do texto no strip
  labs(x='Temperatura Ambiente', #Definindo títulos dos eixos
       y='Venda de Açaí')

#ggsave(filename = 'Vendas de Açai por fds.png',
#       height = 12,
#       width = 20,
#       units = 'cm',
#       bg = 'white')

#PROCEDIMENTO STEPWISE
ols.acai2.step <- step(ols.acai2, k = 3.841459)

summary(ols.acai2.step)


##################################################################################
#                        TESTE DO MODELO NO MÊS DE OUTUBRO                       #
##################################################################################

#CONUNTO DE DADOS DE TESTE

acai.teste <- acai |> select(vendas, temp, chuva, 
                              fds, mes) |> filter(mes == "out") 

acai.teste  <- acai.teste[-5]


# teste do modelo
pred.ols <- predict(ols.acai2, newdata = acai.teste)

# metricas do teste do modelo 1
#library(caret)
teste.ols <- data.frame(acai.teste$vendas, pred.ols)
colnames(teste.ols) <- c("obs","pred")
defaultSummary(teste.ols)

X <- 1:15
teste <- data.frame(X,acai.teste$vendas, pred.ols)
colnames(teste) <- c("X","obs","pred")

ggplot() +
  geom_line(aes(x = teste$X, y = teste$obs,
                  color = "observado"), se = F, size = 1)+
  geom_line(aes(x = teste$X, y = teste$pred,
                  color = "predito"), se = F, size = 1) +
  geom_point(aes(x = teste$X, y = teste$obs,
                 color = "observado"), shape = 0, size = 2, alpha = 4)  +
  geom_point(aes(x = teste$X, y = teste$pred,
                 color = "predito"), shape = 1, size = 3, alpha = 4)  +
  labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí', 
       title='Valores Observados vs. Preditos') +
  theme(legend.title = element_blank(), 
        panel.border = element_rect(NA),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        legend.position = "bottom")


#ggsave(filename = 'Predito vs. Observado.png',
#       height = 12,
#       width = 20,
#       units = 'cm',
#       bg = 'white')

#-------> GERANDO GIF ANIMADO

ggplot(teste[1:2,],aes(x=X,y=pred))+
  geom_point(aes(x = X, y = obs, color = "observado"), shape = 0, size = 2, alpha = 4) +
  geom_point(aes(x = X, y = pred, color = "predito"), shape = 1, size = 3, alpha = 4)  +
  geom_line(col='lightblue', lwd = 2)+
  xlim(range(teste$X))+
  ylim(range(teste$pred))+
  labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí')+
  theme_light()

for (i in seq(1,nrow(teste),1)) {
  
  plot.animado <- ggplot(teste[1:i,],aes(x=X,y=pred)) +
    geom_point(aes(x = X, y = obs, color = "observado"), 
               shape = 0, size = 2, alpha = 4) +
    geom_point(aes(x = X, y = pred, color = "predito"), 
               shape = 1, size = 3, alpha = 4)  +
    geom_line(col='lightblue', lwd = 2) +
    xlim(range(teste$X)) +
    ylim(range(teste$pred)) +
    labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí') +
    theme_light()
  
  print(plot.animado)
  print(i)
  
}


#library(animation)
saveGIF(movie.name = 'plot.animado.gif',{
  
  for (i in seq(1,nrow(teste),1)) {
    
    plot.animado <- ggplot(teste[1:i,],aes(x=X,y=pred)) +
      geom_point(aes(x = X, y = obs, color = "observado"), 
                 shape = 0, size = 2, alpha = 4) +
      geom_point(aes(x = X, y = pred, color = "predito"), 
                 shape = 1, size = 3, alpha = 4)  +
      geom_line(col='lightblue', lwd = 2) +
      xlim(range(teste$X)) +
      ylim(range(teste$pred)) +
      labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí') +
      theme_light()
    
    print(plot.animado)
    print(i)
    
  }
  
})

#Acelerando a animação
saveGIF(movie.name = 'plot.animado.gif',interval=0.2,{
  
  for (i in seq(1,nrow(teste),1)) {
    
    plot.animado <- ggplot(teste[1:i,],aes(x=X,y=pred)) +
      geom_point(aes(x = X, y = obs, color = "observado"), 
                 shape = 0, size = 2, alpha = 4) +
      geom_point(aes(x = X, y = pred, color = "predito"), 
                 shape = 1, size = 3, alpha = 4)  +
      geom_line(col='lightblue', lwd = 2) +
      xlim(range(teste$X)) +
      ylim(range(teste$pred)) +
      labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí') +
      theme_light()
    
    print(plot.animado)
    print(i)
    
  }
  
})

#Aumentando a resolução
saveGIF(movie.name = 'plot.animado.gif',interval=0.2,ani.res=150,
        ani.height=600,ani.width=800,{
          
          for (i in seq(1,nrow(teste),1)) {
            
            plot.animado <- ggplot(teste[1:i,],aes(x=X,y=pred)) +
              geom_point(aes(x = X, y = obs, color = "observado"), 
                         shape = 0, size = 2, alpha = 4) +
              geom_point(aes(x = X, y = pred, color = "predito"), 
                         shape = 1, size = 3, alpha = 4)  +
              geom_line(col='lightblue', lwd = 2) +
              xlim(range(teste$X)) +
              ylim(range(teste$pred)) +
              labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí') +
              theme_light()
            
            print(plot.animado)
            print(i)
            
          }
          
        })

#Aumentando HTML
saveHTML(movie.name = 'plot.animado.gif',interval=0.2,ani.res=150,
         ani.height=600,ani.width=800,{
           
           for (i in seq(1,nrow(teste),1)) {
             
             plot.animado <- ggplot(teste[1:i,],aes(x=X,y=pred)) +
               geom_point(aes(x = X, y = obs, color = "observado"), 
                          shape = 0, size = 2, alpha = 4) +
               geom_point(aes(x = X, y = pred, color = "predito"), 
                          shape = 1, size = 3, alpha = 4)  +
               geom_line(col='lightblue', lwd = 2) +
               xlim(range(teste$X)) +
               ylim(range(teste$pred)) +
               labs(x='Dias (Mês de Outubro)', y='Vendas de Açaí') +
               theme_light()
             
             print(plot.animado)
             print(i)
             
           }
           
         })




