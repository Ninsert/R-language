#----------#   INTRODUÇÃO À GRÁMÁTICA DE GRÁFICOS   #------------#

#library(ggplot2)

#--------------> 1. CAMADA

# 1) DADOS
# 2) MAPEAMENTO
# 3) TRANSFORMAÇÃO ESTATÍSTICA
# 4) OBJETO GEOMÉTRICO
# 5) AJUSTAMENTO DE POSIÇÃO

#DADOS
mpg

#CAMADA
ggplot()+layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
               stat = 'identity', #TRANSFORMAÇÃO ESTATÍSTICA
               geom = 'point', #OBJETO GEOMÉTRICO
               position = 'identity') #AJUSTAMENTO DE POSIÇÃO

#TRANSFORMAÇÃO ESTATÍSTICA
ggplot()+layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
               stat = 'smooth', #TRANSFORMAÇÃO ESTATÍSTICA
               geom = 'point', #OBJETO GEOMÉTRICO
               position = 'identity') #AJUSTAMENTO DE POSIÇÃO

#OBJETO GEOMÉTRICO
ggplot()+layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
               stat = 'identity', #TRANSFORMAÇÃO ESTATÍSTICA
               geom = 'line', #OBJETO GEOMÉTRICO
               position = 'identity') #AJUSTAMENTO DE POSIÇÃO

#OBJETO GEOMÉTRICO
ggplot()+layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
               stat = 'identity', #TRANSFORMAÇÃO ESTATÍSTICA
               geom = 'point', #OBJETO GEOMÉTRICO
               position = 'jitter') #AJUSTAMENTO DE POSIÇÃO

#ESCALA
ggplot()+
  layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
               stat = 'identity', #TRANSFORMAÇÃO ESTATÍSTICA
               geom = 'point', #OBJETO GEOMÉTRICO
               position = "identity")+ #AJUSTAMENTO DE POSIÇÃO
  scale_x_log10()

#SISTEMA DE COORDENADAS
ggplot()+
  layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
        stat = 'identity', #TRANSFORMAÇÃO ESTATÍSTICA
        geom = 'point', #OBJETO GEOMÉTRICO
        position = "identity")+ #AJUSTAMENTO DE POSIÇÃO
  coord_polar()

#TÍTULOS
ggplot()+
  layer(data=mpg, mapping=aes(x=displ, y=hwy), #MAPEAMENTO
        stat = 'identity', #TRANSFORMAÇÃO ESTATÍSTICA
        geom = 'point', #OBJETO GEOMÉTRICO
        position = "identity")+ #AJUSTAMENTO DE POSIÇÃO
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#Podemos simplficar, fazendo o mesmo gráfico sem detalhar a layer,
#fazendo o MAPEAMENTO diretamente na função ggplot de forma 
#mais compacta
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point()
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#COR DOS PONTOS
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(col = 'royalblue')+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(colour = 'red')+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#podemos mudar o elemento geométrico
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_line(colour = 'red')+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#Visualizando os pontos com as linhas
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(colour = 'red')+
  geom_line(colour = 'blue')+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#Ou podemos substituir os dados originais por distribuições
#função "stat"
#Visualizando os pontos com as linhas
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  stat_ecdf()+  #Função que apresenta a distribuição acumulada
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#Adicionando uma "linha de tendência"
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(colour = 'red')+
  stat_smooth(method = 'lm',se=F) +
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(col = 'red')+
  stat_smooth(method = 'auto') +
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#Mudando o tamanho dos pontos
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(col = 'red', size = 2)+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

#mudando a cor de acordo com uma terceira variável
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(col=drv), size = 3)+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) +
  geom_point(size = 3)+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')

ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) +
  geom_point(size = 3)+
  labs(x = 'engine displacement (litres)',
       y = 'highway miles per gallon',
       title = 'hwy vs. displ')+
  facet_wrap(~drv)

#Trabalhando agrupamentos com retas de regressão
iris

ggplot(iris,aes(Sepal.Length,Petal.Length))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)

ggplot(iris,aes(Sepal.Length,Petal.Length,group=Species))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)

ggplot(iris,aes(Sepal.Length,Petal.Length,group=Species,
                col=Species))+
  geom_point() +
  geom_smooth(method='lm',se=FALSE)

#Temas
ggplot(iris,aes(Sepal.Length,Petal.Length,group=Species,
                col=Species))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)+
  theme_bw()

#library(ggthemes)
ggplot(iris,aes(Sepal.Length,Petal.Length,group=Species,
                col=Species))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)+
  theme_excel_new()

#Boxplot
ggplot(iris,aes(y=Sepal.Length,x=Species))+
  geom_boxplot()

