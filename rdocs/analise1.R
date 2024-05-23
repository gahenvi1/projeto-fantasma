source("~/Documents/projeto-fantasma/rdocs/source.R")

#banco das datas, formato e década
banco1 <- banco %>%
  select(date_aired, format) %>%
  mutate(year = year(date_aired),
         decade = cut(year, breaks = c(seq(1959, 2020, by = 10), Inf), labels = c("1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2029")))


### começando a fazer gráfico de barras com frequência
teste_frequencias<- banco1 %>%
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

#criando o banco1 de dados para a análise 1, colunas; 
ana1 <- banco1 %>%
  group_by(decade, format) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "format", values_from = "n", values_fill = 0) %>%
  ungroup() %>%
  mutate(crossover = as.numeric(CrossOver), 
         movie = as.numeric(Movie), 
         serie = as.numeric(Serie), 
         total = rowSums(select(., -decade))) %>%
  select(., c(decade, crossover, movie, serie, total))


ana1 %>%
  print_quadro_resumo("movie")

ana1 %>%
  print_quadro_resumo("serie")

ana1 %>%
  print_quadro_resumo("crossover")

ana1 %>%
  print_quadro_resumo("total")


