library(plotly)
library(dplyr)

plot_3d_surface_with_points <- function(model, data, x_var, y_var, z_var) {
  # Definindo o alcance das variáveis baseado em seus valores mínimos e máximos nos dados
  x_range <- seq(min(data[[x_var]], na.rm = TRUE), max(data[[x_var]], na.rm = TRUE), length.out = 50)
  y_range <- seq(min(data[[y_var]], na.rm = TRUE), max(data[[y_var]], na.rm = TRUE), length.out = 50)
  
  # Criando grid dinâmico com os nomes das variáveis como x_var e y_var
  grid <- expand.grid(
    x_var = x_range, 
    y_var = y_range
  )
  names(grid) <- c(x_var, y_var)  # Renomeando as colunas para corresponder às variáveis dinâmicas
  
  # Calculando previsões para cada combinação de x e y
  predictions <- predict(model, newdata = grid)
  
  # Criando matriz de previsões para plotagem de superfície
  M_matrix <- matrix(predictions, nrow = 50, byrow = TRUE)
  
  # Plotagem do gráfico 3D
  fig <- plot_ly() %>%
    add_surface(x = ~x_range, y = ~y_range, z = ~M_matrix, colors = c("#FFD700", "#FF6347", "#40E0D0"),
                showscale = TRUE, name = "Predicted Surface") %>%
    add_markers(x = ~data[[x_var]], y = ~data[[y_var]], z = ~data[[z_var]], 
                marker = list(color = 'rgb(135, 206, 250)', size = 5),
                name = "Actual Data") %>%
    layout(title = "3D Surface Plot with Actual Data Points",
           scene = list(
             xaxis = list(title = x_var, titlefont = list(color = 'red')),
             yaxis = list(title = y_var, titlefont = list(color = 'green')),
             zaxis = list(title = z_var, titlefont = list(color = 'blue'))),
           legend = list(orientation = "h", x = 0, y = 1, xanchor = "left", yanchor = "bottom"))
  
  return(fig)
}
