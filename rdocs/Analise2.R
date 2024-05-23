source("~/Documents/projeto-fantasma/rdocs/source.R")

banco2 <- banco %>%
  select(imdb, season) %>%
  filter(season %in% c(1,2,3,4))


#gráfico box-plot padronizado
ggplot(banco2) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporadas", y = "Nota IMDB") +
  theme_estat()

#salvando o gráfico
ggsave("resultados/box_bi.pdf", width = 158, height = 93, units = "mm")


banco2 %>%
  group_by(season) %>%
  print_quadro_resumo(var_name = "imdb")

#coeficiente de determinacao
modelo <- lm(banco2$imdb ~ banco2$season)

rquadrad <- summary(modelo)$r.squared
print(round(rquadrad, 4))
