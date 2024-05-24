source("~/Documents/projeto-fantasma/rdocs/source.R")

banco5 <- banco %>%
  pivot_longer(cols= starts_with("caught_"),
               names_to = "Quem capturou?",
               values_to = "capturou"
  ) %>%
  filter(capturou == T) %>%
  mutate(`Quem capturou?` = str_to_title(str_replace(`Quem capturou?`, "caught_", ""))) %>%
  select(`Quem capturou?`, engagement, title, date_aired) %>%
  mutate(`Quem capturou?` = as.factor(case_when(
    `Quem capturou?` %>% str_detect("Daphnie") ~ "Daphne",
    `Quem capturou?` %>% str_detect("Fred") ~ "Fred",
    `Quem capturou?` %>% str_detect("Scooby") ~ "Scooby",
    `Quem capturou?` %>% str_detect("Shaggy") ~ "Salsicha",
    `Quem capturou?` %>% str_detect("Velma") ~ "Velma",
    `Quem capturou?` %>% str_detect("Not") ~ "MNC",
    `Quem capturou?` %>% str_detect("Other") ~ "Outro"
  )))


#gráfico box-plot padronizado
ggplot(banco5) +
  aes(x = reorder(`Quem capturou?`, engagement, FUN = mean), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Quem capturou?", y = "Nível de Engajamento") +
  theme_estat()

ggsave("resultados/engajamento_x_personagem_caught.pdf", width = 158, height = 93, units = "mm")

banco5 %>%
  group_by(`Quem capturou?`) %>%
  print_quadro_resumo(var_name = "engagement")


#coeficiente de determinacao
modelo <- lm(banco5$engagement ~ banco5$`Quem capturou?`)

rquadrad <- summary(modelo)$r.squared
print(round(rquadrad, 4))



