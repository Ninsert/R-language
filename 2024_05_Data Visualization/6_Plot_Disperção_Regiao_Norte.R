# Carregar bibliotecas necessárias
library(ggplot2)
library(gganimate)
library(dplyr)
library(lubridate)

# Ler os dados
data <- read.csv("dados//COVID19_20200417.csv", sep=";", stringsAsFactors=FALSE)

# Filtrar dados para a região Norte
estados_norte <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
data_norte <- data |> filter(estado %in% estados_norte)

# Processar dados
data_norte <- data_norte |>
  mutate(data = ymd(data)) |>
  group_by(estado, data) |>
  summarize(casosAcumulados = sum(casosAcumulados), .groups = 'drop')

# Criar plot
p <- ggplot(data_norte, aes(x = data, y = casosAcumulados, color = estado)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = 'Casos Acumulados de COVID-19 na Região Norte do Brasil',
       subtitle = 'Dados até 17/04/2020',
       x = 'Data',
       y = 'Casos Acumulados',
       color = 'Estado') +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  transition_reveal(data)

# Configurar a animação para ser mais lenta
anim <- animate(p, nframes = 200, fps = 10, width = 800, height = 600, renderer = gifski_renderer())

# Salvar animação
anim_save("casos_acumulados_norte.gif", anim)

