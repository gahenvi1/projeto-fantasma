if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse", "stringr", "vcd", "knitr", "kableExtra")

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
  select(trap_work_first, setting_terrain) %>%
  filter(!is.na(trap_work_first))

#dados para a tabela dos dados sem o filtro
top3 <- read_csv("banco/banco_final.csv") %>%
  select(trap_work_first, setting_terrain) %>%
  group_by(setting_terrain) %>%
  summarise(`Frequência` = n()) %>%
  mutate(freq_relativa = round(`Frequência`/sum(`Frequência`), 4) ) %>%
  arrange(desc(`Frequência`))


#banco para gráficos? tabela?
terreno <- banco %>%
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
  labs(x = "Terreno", y = "Frequência", fill = "Armadilha de primeira?") +
  theme_estat()

ggsave("resultados/armadilha_x_terreno.pdf", width = 158, height = 93, units = "mm")



#coeficiente de contigencia
# tabela cruzada
tab <- xtabs(~ trap_work_first + setting_terrain, data = banco)

#funcao que calcula os coeficientes
summary(assocstats(tab))
