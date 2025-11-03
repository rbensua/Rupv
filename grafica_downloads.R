library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(cranlogs)

# Obtener datos de descargas (ajusta las fechas según necesites)
downloads_data <- cran_downloads(
  packages = "deaR", 
  from = "2018-12-23",
  to = Sys.Date()
)

# Agrupar por mes y calcular acumulado
downloads_mensuales <- downloads_data %>%
  mutate(mes = floor_date(date, "month")) %>%
  group_by(mes) %>%
  summarise(descargas_totales = sum(count)) %>%
  arrange(mes) %>%
  mutate(
    descargas_acumuladas = cumsum(descargas_totales),
    mes_num = as.numeric(mes - min(mes)) / 30.44  # Para escalado del eje secundario
  )

# Calcular factor de escala para el eje secundario
scale_factor <- max(downloads_mensuales$descargas_totales) / max(downloads_mensuales$descargas_acumuladas)

# Crear gráfica con dos ejes Y
ggplot(downloads_mensuales) +
  # Barras para descargas mensuales (eje primario izquierdo)
  geom_col(aes(x = mes, y = descargas_totales), 
           fill = "#E69F00", alpha = 0.7, width = 20) +
  # Línea para descargas acumuladas (eje secundario derecho)
  geom_line(aes(x = mes, y = descargas_acumuladas * scale_factor), 
            color = "darkred", size = 1.5, alpha = 0.8) +
  geom_point(aes(x = mes, y = descargas_acumuladas * scale_factor), 
             color = "darkred", size = 2) +
  
  # Escalas y ejes
  scale_y_continuous(
    # Eje primario (descargas mensuales)
    name = "Descargas Mensuales",
    labels = comma,
    expand = expansion(mult = c(0, 0.1)),
    
    # Eje secundario (descargas acumuladas)
    sec.axis = sec_axis(
      ~ . / scale_factor,  # Transformación inversa
      name = "Descargas Acumuladas",
      labels = comma
    )
  ) +
  
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  labs(
    title = "Descargas de ggplot2: Mensuales vs Acumuladas",
    x = "Mes",
    caption = "Barras naranjas: Descargas mensuales | Línea roja: Descargas acumuladas"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y.left = element_text(color = "#E69F00", face = "bold"),
    axis.title.y.right = element_text(color = "darkred", face = "bold"),
    axis.text.y.left = element_text(color = "#E69F00"),
    axis.text.y.right = element_text(color = "darkred"),
    panel.grid.major.x = element_blank()
  )


# Versión con colores en la leyenda
ggplot(downloads_mensuales) +
  geom_col(aes(x = mes, y = descargas_totales, fill = "Mensuales"), 
           alpha = 0.7, width = 20) +
  geom_line(aes(x = mes, y = descargas_acumuladas * scale_factor, color = "Acumuladas"), 
            size = 1.5, alpha = 0.8) +
 # geom_point(aes(x = mes, y = descargas_acumuladas * scale_factor, color = "Acumuladas"), 
  #           size = 2) +
  
  scale_y_continuous(
    name = "Descargas Mensuales",
    labels = comma,
    expand = expansion(mult = c(0, 0.1)),
    sec.axis = sec_axis(~ . / scale_factor, name = "Descargas Acumuladas", labels = comma)
  ) +
  
  scale_fill_manual(
    name = "",
    values = c("Mensuales" = "#E69F00")
  ) +
  
  scale_color_manual(
    name = "",
    values = c("Acumuladas" = "darkred")
  ) +
  
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y"
  ) +
  
  labs(
    title = "Descargas de deaR",
    x = "Mes"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y.left = element_text(color = "#E69F00", face = "bold"),
    axis.title.y.right = element_text(color = "darkred", face = "bold"),
    legend.position = "top",
    legend.box = "horizontal"
  )
