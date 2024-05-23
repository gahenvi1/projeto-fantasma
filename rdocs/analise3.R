source("~/Documents/projeto-fantasma/rdocs/source.R")


#banco3 com os dados sem valores NA e referente as colunas da análise 3
banco3 <- banco %>%
  select(trap_work_first, setting_terrain) %>%
  filter(!is.na(trap_work_first))

#dados para a tabela dos dados sem o filtro
top3 <- banco %>%
  select(trap_work_first, setting_terrain) %>%
  group_by(setting_terrain) %>%
  summarise(`Frequência` = n()) %>%
  mutate(freq_relativa = round(`Frequência`/sum(`Frequência`), 4) ) %>%
  arrange(desc(`Frequência`))


#banco3 para gráficos? tabela?
terreno <- banco3 %>%
  mutate(setting_terrain = case_when(
    setting_terrain %>% str_detect("Urban") ~ "Urbano",
    setting_terrain %>% str_detect("Rural") ~ "Rural",
    setting_terrain %>% str_detect("Forest") ~ "Floresta"
  )) %>%
  filter(!is.na(setting_terrain)) %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )
terreno$trap_work_first <- factor(terreno$trap_work_first, levels = c(FALSE, TRUE), labels = c("Não", "Sim"))

#legandas
porcentagens <- str_c(terreno$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(terreno$freq, " (", porcentagens, ")"))

#gráfico de barras
ggplot(terreno) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = trap_work_first, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.95),
    vjust = -0.5, hjust = 0.5,
    size = 2.7
  ) +
  labs(x = "Terreno", y = "Frequência", fill = "Armadilha funcionou na primeira tentativa?") +
  theme_estat()

ggsave("resultados/armadilha_x_terreno.pdf", width = 158, height = 93, units = "mm")



#coeficiente de contigencia
# tabela cruzada
tab <- xtabs(~ trap_work_first + setting_terrain, data = banco3)

#funcao que calcula os coeficientes
summary(assocstats(tab))

#frequencia depois dos filtros
outro<- banco3 %>%
  group_by(setting_terrain) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
