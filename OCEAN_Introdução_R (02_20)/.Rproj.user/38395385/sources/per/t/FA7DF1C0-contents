#---------------#          INTRODUÇÃO A LINGUAGEM R          #---------------#


# PRIMEIRO PASSO EM QUALQUER LINGUAGEM DE PROGRAMAÇÃO:
print("Hello World")


# TIPOS DE VARIÁVEIS:
inteiro <- 12
print(inteiro)
class(inteiro)


numerico_2 <- 12.1
print(numerico_2)
class(numerico_2)

complexo_1 <- 12i + 12
print(complexo_1)
class(complexo_1)

character_1 <-  "Hello World"
print(character_1)
class(character_1)

boolean_1 <- TRUE
print(boolean_1)
class(boolean_1)


# OPERAÇÕES MATEMÁTICAS:
6 + 6
20 - 8
3 * 4
36 / 3
246 %% 13 


# OPERADORES LÓGICOS:
12 == 12
12 != 12
12 > 12
12 < 12
12 >= 12
12 <= 12

12 == 12 & 12==12 # E 
12 == 12 & 12==13 
12 == 12 | 12==13 # OU
12 == 12 | 12==12 
!12 == 12         # NÃO
12 == 12 | !12==13



# CONDICIONAIS:

# se simples:
if (12 == 12){
  print("12 é igual a 12")
}

# se / senão
if (12 != 12){
  print("12 é diferente de 12")
} else{
  print("12 é igual a 12")
}

# se encadeado
if (12 != 12){
  print("12 é diferente de 12")
} else if(12== 12){
  print("12 é igual a 12")
} else{
  print("12 é diferente de 12")
}

# se aninhado
if (12 == 12){
  print("Se Aninhado")
  if (12 == 12){
   print("12 é igual a 12")
  }
} else{
  print("12 é diferente de 12")
}


# LAÇOS DE REPETIÇÃO:
i <- 1
while (i < 13){
  print(i)
  i <- i +  1
}

i <- 1
while (i < 25){
  print(i)
  if (i == 12){
    break
  }
  i <- i +  1
}

i <- 1
while (i < 25){
  i <- i +  1
  if (i == 12){
    next
  }
  print(i)
}

for (x in 1 : 10){
  print(12)
}

flores <- list("setosa","virginica", "versicolor")
for (flor in flores) {
  print(flor)
}

for (flor in flores) {
  if (flor == "virginica"){
    break
  }
  print(flor)
}


# FUNÇÕES: 
roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace=TRUE)
  sum(dice)
}

roll2()



# ESTRUTURAS DE DADOS

# Lista
flores_1 <- list("setosa","virginica", "versicolor")

# Vetor
flores_2 <- c("setosa","virginica", "versicolor")
vetor <- 1:10

# Matriz
matriz <- matrix(
  c(1,0,0,
    0,1,0,
    0,0,1),
  nrow = 3,
  ncol = 3
)

# Data Frame
data_frame <- data.frame (
  flores_2,
  quantidade_em_Manaus = c(100, 150, 120),
  quantidade_em_Rio_Preto = c(60, 30, 45)
)
data_frame



#---------------#                OPERADOR PIPE                 #---------------#

# Utilizando o operador pipe "%>%" e "|>"

# A ideia do operador %>% (pipe) é bem simples: usar o valor resultante 
# da expressão do lado esquerdo como primeiro argumento 
# da função do lado direito. Vamos para um exemplo:

3.14159 |> cos()

x <- c(1, 2, 3, 4)
x |> sum() |> sqrt()



#---------------#       INTRODUÇÃO À GRÁMÁTICA DE GRÁFICOS     #---------------#

library(tidyverse)

mtcars %>%                                #1ª CAMADA - Dados
  
  ggplot(aes(x=disp, y=mpg)) +            #2ª CAMADA - Estética
  
  geom_point(colour = 'royalblue') +      #3ª CAMADA - Geometria
  
  facet_wrap(~am) +                       #4ª CAMADA - Facets
  
  stat_smooth(method = 'auto') +           #5ª CAMADA - Estatística
  coord_cartesian() +                     #6ª CAMADA - Coordenadas
  theme_bw()                              #7ª CAMADA - Temas
  labs(x = 'Cilindradada (in³)',          #8ª CAMADA - Títulos
       y = 'Milha por galão',
       title = 'Consumo médio de veículos')
  
  