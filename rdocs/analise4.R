source("~/Documents/projeto-fantasma/rdocs/source.R")


#banco4 com os dados sem valores NA e referente as colunas da análise 3
banco4 <- banco %>%
  select(imdb, engagement)


#graf de dispersao
ggplot(banco4) +
  aes(x = imdb, y = engagement) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.6) +
  labs(
    x = "Nota IMDB",
    y = "Nível de engajamento"
  ) +
  theme_estat()

ggsave("resultados/graf_imdb_x_engajamento.pdf", width = 158, height = 93, units = "mm")


#coeficiente de pearson
round(cor(banco4$imdb, banco4$engagement, method = "pearson"), 4)
#valor observado: 0.9243

print_quadro_resumo(banco4, var_name = "engagement")
print_quadro_resumo(banco4, var_name = "imdb")


# gráfico estático com geom_point
p <- banco %>%
  ggplot(aes(x = imdb, y = engagement, text = paste("Título:", title, "<br>Data de Lançamento:", date_aired,"<br>Nota IMDB:", imdb, "<br>Nível de Engajamento:", engagement ))) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Nota do Episódio", y = "Nível de Engajamento")+
  theme_estat()

# gráfico interativo com plotly
p_interactive <- ggplotly(p, tooltip = "text")

# salvando o arquivo em html
htmlwidgets::saveWidget(p_interactive, "resultados/index.html")

