pkgs <- c("readxl", "dplyr", "openxlsx", "ggplot2")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(openxlsx); library(ggplot2)
})

arq_entrada <- "data/resultados_tcc_fundeb.xlsx"
arq_saida   <- "data/base_grafico_A1_r.xlsx"
arq_png     <- "data/Grafico_A1_r.png"

lim_x <- 14000
lim_y <- 300
janela <- 0.4

raw <- read_excel(arq_entrada, sheet = "GF_Base_Municipios", .name_repair = "minimal")
d <- tibble(cod_ibge = as.character(raw$cod_ibge),
            receita = as.numeric(raw$rec_educ_por_mat),
            variacao = as.numeric(raw[["variacao_recfundeb_18-25"]])) %>%
  filter(!is.na(receita), !is.na(variacao))

pontos <- d %>%
  mutate(fora_do_grafico = ifelse(receita > lim_x | variacao > lim_y, "sim", "não")) %>%
  arrange(receita) %>%
  transmute(cod_ibge,
            receita_educacional_por_matricula_2018 = round(receita, 2),
            variacao_receita_fundeb_por_matricula_18_25 = round(variacao, 2),
            fora_do_grafico)

lw <- lowess(d$receita, d$variacao, f = janela, iter = 3, delta = 0)
curva <- tibble(receita_educacional_por_matricula_2018 = round(lw$x, 2),
                variacao_suavizada_lowess = round(lw$y, 2)) %>%
  distinct(receita_educacional_por_matricula_2018, .keep_all = TRUE)

d$decil <- cut(d$receita, quantile(d$receita, 0:10 / 10), include.lowest = TRUE, labels = FALSE)
decis <- d %>%
  group_by(decil) %>%
  summarise(n_municipios = n(), receita_media = round(mean(receita), 2),
            variacao_media = round(mean(variacao), 2),
            variacao_mediana = round(median(variacao), 2), .groups = "drop")

rotulos <- c("Municípios", "Curva suavizada (LOWESS)", "Média por decil")
g <- ggplot() +
  geom_point(data = d, aes(receita, variacao, colour = rotulos[1]), size = 0.9) +
  geom_line(data = curva, aes(receita_educacional_por_matricula_2018, variacao_suavizada_lowess,
                              colour = rotulos[2]), linewidth = 0.9) +
  geom_point(data = decis, aes(receita_media, variacao_media, colour = rotulos[3]), size = 2.6) +
  scale_colour_manual(values = c("#B8C4DC", "black", "#404040"), breaks = rotulos) +
  guides(colour = guide_legend(override.aes = list(shape = c(16, NA, 16),
                                                   linetype = c(0, 1, 0),
                                                   linewidth = c(0, 0.9, 0),
                                                   size = c(1.8, 1, 2.6)))) +
  scale_x_continuous(breaks = seq(4000, lim_x, 1000),
                     labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(breaks = seq(50, lim_y, 50)) +
  coord_cartesian(xlim = c(4000, lim_x), ylim = c(40, lim_y), expand = FALSE) +
  labs(x = "Receita educacional por matrícula em 2018 (R$)",
       y = "Variação da receita do Fundeb\npor matrícula, 2018-2025 (%)") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(8, 14, 8, 8))

ggsave(arq_png, g, width = 8, height = 5, dpi = 300, bg = "white")

leia <- data.frame(`Leia-me` = c(
  "Base do Gráfico A.1: variação da receita do Fundeb por matrícula (2018-2025, %) segundo a receita educacional por matrícula de 2018 (R$).",
  "Pontos: um município por linha, ordenados pela receita. fora_do_grafico = sim para receita acima de R$ 14.000 ou variação acima de 300%; entram nos cálculos, mas ficam fora dos eixos.",
  "Curva_LOWESS: curva suavizada (LOWESS, janela de 40% das observações), calculada com todos os municípios.",
  "Decis: média e mediana da variação em cada decil da receita educacional por matrícula.",
  "Fonte: Elaboração própria, a partir de data/resultados_tcc_fundeb.xlsx (aba GF_Base_Municipios)."),
  check.names = FALSE)

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")
for (nm in c("Leia-me", "Pontos", "Curva_LOWESS", "Decis")) addWorksheet(wb, nm)
writeData(wb, "Leia-me", leia, headerStyle = hs)
writeData(wb, "Pontos", as.data.frame(pontos), headerStyle = hs)
writeData(wb, "Curva_LOWESS", as.data.frame(curva), headerStyle = hs)
writeData(wb, "Decis", as.data.frame(decis), headerStyle = hs)
setColWidths(wb, "Leia-me", cols = 1, widths = 140)
tabs <- list(Pontos = pontos, Curva_LOWESS = curva, Decis = decis)
for (nm in names(tabs)) {
  setColWidths(wb, nm, cols = seq_along(tabs[[nm]]), widths = "auto")
  freezePane(wb, nm, firstRow = TRUE)
}
addWorksheet(wb, "Grafico")
insertImage(wb, "Grafico", arq_png, width = 20, height = 12.5, units = "cm", startRow = 2)
worksheetOrder(wb) <- c(5, 1, 2, 3, 4)

saveWorkbook(wb, arq_saida, overwrite = TRUE)
cat("Base gerada em:", arq_saida, "\n")
