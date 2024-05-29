# --------------------------> INSTALAÇÃO DE PACOTES

pacotes <- c("readxl","ggpmisc","ggplot2","jtools","kableExtra",
             "ggpubr","corrgram","corrplot","PerformanceAnalytics",
             "GGally","plotly","lmtest","dplyr","fastDummies",
             "caret","pheatmap","animation", "tidyverse")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#library (installr)
#updateR() 
