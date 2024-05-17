if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse", "plotly")

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

#funcao para os dados resumo
print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
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
#valor observado: 0.9243

print_quadro_resumo(banco, var_name = "engagement")












# Criar o gráfico ggplot com geom_jitter
p <- read_csv("banco/banco_final.csv") %>%
  ggplot(aes(x = imdb, y = engagement, text = paste("Título:", title, "<br>Data de Lançamento:", date_aired,"<br>Nota IMDB:", imdb, "<br>Nível de Engajamento:", engagement ))) +
  geom_jitter() +
  labs(x = "Nota do Episódio", y = "Nível de Engajamento")+
  theme_estat()

# Tornar o gráfico interativo com plotly
p_interactive <- ggplotly(p, tooltip = "text")

# Salvar o gráfico interativo em um arquivo HTML
htmlwidgets::saveWidget(p_interactive, "grafico_interativo.html")

