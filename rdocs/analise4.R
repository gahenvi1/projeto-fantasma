if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse")

#cores estat
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )

#tema estat
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


#banco com os dados sem valores NA e referente as colunas da análise 3
banco <- read_csv("banco/banco_final.csv") %>%
  select(imdb, engagement)


#graf de dispersao
ggplot(banco) +
  aes(x = imdb, y = engagement) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.6) +
  labs(
    x = "Nota IMDB",
    y = "Nível de engajamento"
  ) +
  theme_estat()

ggsave("resultados/graf_imdb_x_engajamento.pdf", width = 158, height = 93, units = "mm")


#coeficiente de pearson
round(cor(banco$imdb, banco$engagement, method = "pearson"), 4)


