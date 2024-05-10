if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse", "stringr", "ggrepel")

#banco das datas, formato e década
banco <- read_csv("banco/banco_final.csv") %>%
  select(date_aired, format) %>%
  mutate(year = year(date_aired),
         decade = cut(year, breaks = c(seq(1959, 2020, by = 10), Inf), labels = c("1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2029")))

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

### começando a fazer gráfico de barras com frequência
teste_frequencias<- banco %>%
  mutate(decade = case_when(
    decade %>% str_detect("1960-1969") ~ "1960-1969",
    decade %>% str_detect("1970-1979") ~ "1970-1979",
    decade %>% str_detect("1980-1989") ~ "1980-1989",
    decade %>% str_detect("1990-1999") ~ "1990-1999",
    decade %>% str_detect("2000-2009") ~ "2000-2009",
    decade %>% str_detect("2010-2019") ~ "2010-2019",
    decade %>% str_detect("2020-2029") ~ "2020-2029"
  ), format = as.factor(format)) %>%
  group_by(decade, format) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

levels(teste_frequencias$format) <- c("CrossOver", "Filme", "Série")

porcentagens <- str_c(teste_frequencias$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(teste_frequencias$freq, " (", porcentagens, ")")
)

#gráfico padronizado do modelo
ggplot(teste_frequencias) +
  aes(
    x = decade, y = freq,
    fill = format, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 1),
    vjust = -0.5, hjust = .5,
    size = 2.5
  ) +
  labs(x = "Década", y = "Frequência", fill = "Formato") +
  theme_estat()

#anotações:
# primeira data registrada é do ano 1969 e ultima data registrada é do ano 2021  
ggsave("resultados/colunas-multi-freq.pdf", width = 230, height = 100, units = "mm")

#gráfico padronizado do modelo agora de linhas
ggplot(teste_frequencias) +
  aes(x = decade, y = freq, group = format, color = format) +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = legendas),
    position = position_dodge(width = 0.98),
    vjust = -1.1, hjust = 0.45,
    size = 1.8
  ) +
  labs(x = "Década", y = "Frequência", color = "Formato") +
  theme_estat()


#anotações:
# primeira data registrada é do ano 1969 e ultima data registrada é do ano 2021  
ggsave("resultados/linhas-multi-freq.pdf", width = 158, height = 93, units = "mm")

#criando o banco de dados para a análise 1, colunas; 
ana1 <- banco %>%
  group_by(decade, format) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "format", values_from = "n", values_fill = 0) %>%
  ungroup() %>%
  mutate(crossover = as.numeric(CrossOver), 
         movie = as.numeric(Movie), 
         serie = as.numeric(Serie), 
         total = rowSums(select(., -decade))) %>%
  select(., c(decade, crossover, movie, serie, total))


#grafico lollipop da quantidade de total por década
ggplot(ana1, aes(x = decade, y = total)) +
  geom_point(size=3.8, color="#999966", fill=alpha("#008091", 0.3), alpha=0.7, shape=21, stroke=2)+
  geom_segment( aes(x=decade, xend=decade, y=0, yend=total), color = "#041835")+
  geom_text(aes(label = total), vjust = 0.5, hjust = -0.57, size = 4)+
  labs(x = "Década", y = "Frequência") +
  theme_estat()

#gráfico lollipop quantidade de episodios por década
ggsave("resultados/lollipop.pdf", width = 158, height = 93, units = "mm")




# funcao para quadro de medidas resumo da padronizacao
print_quadro_resumo <- function(data, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- data %>%
    summarize(`Média` = round(mean(movie),2),
              `Desvio Padrão` = round(sd(movie),2),
              `Variância` = round(var(movie),2),
              `Mínimo` = round(min(movie),2),
              `1º Quartil` = round(quantile(movie, probs = .25),2),
              `Mediana` = round(quantile(movie, probs = .5),2),
              `3º Quartil` = round(quantile(movie, probs = .75),2),
              `Máximo` = round(max(movie),2)) %>%
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
  latex <- str_c(latex, "| l", "|", sep=" ")
  for (i in seq(2, col_count))
  {
    latex <- str_c(latex, "S", sep=" ")
  }
  
  
  latex <- str_c(latex, "|}\n\t\\toprule\n\t\t", sep="")
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

ana1 %>%
  print_quadro_resumo()


