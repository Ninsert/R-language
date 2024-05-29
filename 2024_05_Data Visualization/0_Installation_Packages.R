# --------------------------> INSTALAÇÃO DE PACOTES

#library (installr)
#updateR() 

pacotes <- c("ggplot2","ggthemes","plotly","readxl","dplyr",
             "ggridges","viridis","hrbrthemes","fields",
             "RColorBrewer","corrgram","pheatmap",
             "corrplot","PerformanceAnalytics",
             "GGally","fastDummies","ggpubr",
             "animation","lubridate","gganimate",
             "ggperiodic","transformr","caret")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

