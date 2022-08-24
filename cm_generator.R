# Gerador de tabelas para indicadores: câmara de vereadores

if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("scales")) install.packages("scales")
library(scales)

if(!require("kableExtra")) install.packages("kableExtra")
library(kableExtra)

source("funcoes.R")

# Carrega dados
ds_file <- "indicadores cm.xlsx"
df <- readxl::read_excel(ds_file, sheet = "Dados")
data_base <- as.Date(df$data_base[1])
periodo <- format(data_base, format = "%B de %Y")
ano_base <- format(data_base, format = "%Y")
writeLines(paste("\\date{", periodo, "}", sep = ""), "cache/data.tex")

# Limite Constitucional de Gastos Totais
df <- readxl::read_excel(ds_file, sheet = "GastoTotal")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df$data_base <- NULL
df <- select(df, ano, mes, vl_limite, vl_gasto_total_estimado, vl_resultado_estimado, indice_estimado)
df <- df[df$ano==ano_base,]
df$indice_estimado <- percent(df$indice_estimado)
df$ano <- NULL

tbl = app.table.default(df, align=c("l", "r", "r", "r", "r"), caption = "Gasto Total Estimado", col.names = c("Mês", "Limite R$", "Gasto Total Estimado R$", "Resultado Estimado R$", "Índice Estimado %"))
writeLines(tbl, "cache/cm_gasto_total.tex")

df$mes <- as.factor(df$mes)
g <- ggplot(df, aes(x = mes, group = 1)) +
  ggtitle("Gasto Total (CF 1988)") +
  geom_bar(aes(y = vl_gasto_total_estimado, fill = "Gasto Total"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = vl_gasto_total_estimado, label = indice_estimado), vjust = -0.5, color = app.color.primary, na.rm = TRUE) +
  geom_line(aes(y = vl_limite, color = "Limite"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) + 
  scale_color_manual(values = app.color.secondary)
g <- app.plot.theming(g)
ggsave("cache/cm_gasto_total.png", plot = g)

# Limite Constitucional de Gasto com Folha de Pagamentos
df <- readxl::read_excel(ds_file, sheet = "GastoFolha")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df$data_base <- NULL
df <- select(df, ano, mes, vl_limite, vl_gasto_total_estimado, vl_resultado_estimado, indice_estimado)
df <- df[df$ano==ano_base,]
df$indice_estimado <- percent(df$indice_estimado)
df$ano <- NULL

tbl = app.table.default(df, align=c("l", "r", "r", "r", "r"), caption = "Gasto com Folha de Pagamento", col.names = c("Mês", "Limite R$", "Gasto com Folha Estimado R$", "Resultado Estimado R$", "Índice Estimado %"))
writeLines(tbl, "cache/cm_gasto_folha.tex")

df$mes <- as.factor(df$mes)
g <- ggplot(df, aes(x = mes, group = 1)) +
  ggtitle("Gasto com Folha de Pagamento (CF 1988)") +
  geom_bar(aes(y = vl_gasto_total_estimado, fill = "Gasto com Folha"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = vl_gasto_total_estimado, label = indice_estimado), vjust = -0.5, color = app.color.primary, na.rm = TRUE) +
  geom_line(aes(y = vl_limite, color = "Limite"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) + 
  scale_color_manual(values = app.color.secondary)
g <- app.plot.theming(g)
ggsave("cache/cm_gasto_folha.png", plot = g)

# Limite de Despesa Total com Pessoal da LRF
df <- readxl::read_excel(ds_file, sheet = "PessoalLRF")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df$data_base <- NULL
df <- select(df, ano, mes, indice, limite_maximo, limite_alerta, limite_prudencial)
df <- df[df$ano==ano_base,]
df$ano <- NULL
dfg <- df
df$indice <- percent(df$indice, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
df$limite_maximo <- percent(df$limite_maximo, big.mark = ".", decimal.mark = ",")
df$limite_alerta <- percent(df$limite_alerta, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
df$limite_prudencial <- percent(df$limite_prudencial, big.mark = ".", decimal.mark = ",", accuracy = 0.01)

tbl = app.table.default(df, align=c("l", "r", "r", "r", "r"), caption = "Despesa Total com Pessoal (LRF)", col.names = c("Mês", "Índice", "Limite Legal", "Limite de Alerta", "Limite Prudencial"))
writeLines(tbl, "cache/cm_dtp.tex")

dfg$mes <- as.factor(dfg$mes)
g <- ggplot(dfg, aes(x = mes, group = 1)) +
  ggtitle("Despesa Total com Pessoal (LRF)") +
  geom_bar(aes(y = indice, fill = "Índice"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = indice, label = df$indice), vjust = -0.5, color = app.color.primary, na.rm = TRUE) +
  geom_line(aes(y = limite_maximo, linetype = "Limite"), na.rm = TRUE) +
  geom_line(aes(y = limite_alerta, linetype = "Alerta"), na.rm = TRUE) +
  geom_line(aes(y = limite_prudencial, linetype = "Prudencial"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = dfg$mes) +
  scale_fill_manual(values = app.color.primary) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
ggsave("cache/cm_dtp.png", plot = g)

# Limite de Suplementação Autorizado na Lei Orçamentária Anual
df <- readxl::read_excel(ds_file, sheet = "LimiteSuplementacao")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df$data_base <- NULL
df <- select(df, ano, mes, indice, limite, media)
df <- df[df$ano==ano_base,]
df$ano <- NULL
dfg <- df
df$indice <- percent(df$indice, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
df$limite <- percent(df$limite, big.mark = ".", decimal.mark = ",")
df$media <- percent(df$media, big.mark = ".", decimal.mark = ",", accuracy = 0.01)

tbl = app.table.default(df, align=c("l", "r", "r", "r"), caption = "Limite de Suplementação (LOA)", col.names = c("Mês", "Índice", "Limite", "Esperado"))
writeLines(tbl, "cache/cm_suplem.tex")

dfg$mes <- as.factor(dfg$mes)
g <- ggplot(dfg, aes(x = mes, group = 1)) +
  ggtitle("Limite de Suplementação (LOA)") +
  geom_bar(aes(y = indice, fill = "Índice"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = indice, label = df$indice), vjust = -0.5, color = app.color.primary, na.rm = TRUE) +
  geom_line(aes(y = limite, linetype = "Limite"), na.rm = TRUE) +
  geom_line(aes(y = media, linetype = "Esperado"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = dfg$mes) +
  scale_fill_manual(values = app.color.primary) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/cm_suplem.png", plot = g)

# Resultado Orçamentário
df <- readxl::read_excel(ds_file, sheet = "ResultadoOrcamentario")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df$data_base <- NULL
df <- select(df, ano, mes, valor)
pvt <- pivot_wider(df, names_from = ano, values_from = valor)

cnames = append(list("Mês"), levels(unique(df$ano)))
tbl = app.table.default(pvt, align=c("l", "r", "r"), caption = "Resultado Orçamentário", col.names = cnames)
writeLines(tbl, "cache/cm_resultado.tex")

g <- ggplot(df) +
  ggtitle("Resultado Orçamentário") +
  geom_bar(aes(x = mes, y = valor, group = ano, fill = ano), stat = "identity", position = position_dodge(), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary)) +
  scale_x_discrete(limits = unique(df$mes))
g <- app.plot.theming(g)
ggsave("cache/cm_resultado.png", plot = g)

# Saldo Financeiro Bruto e Líquido
df <- readxl::read_excel(ds_file, sheet = "SaldoFinanceiro")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df$data_base <- NULL
df <- select(df, ano, mes, bruto, disponivel)
df <- df[df$ano==ano_base,]
df$ano <- NULL

tbl = app.table.default(df, align=c("l", "r", "r"), caption = "Disponibilidades Financeiras", col.names = c("Mês", "Saldo Bruto", "Saldo Disponível"))
writeLines(tbl, "cache/cm_financeiro.tex")

g <- ggplot(df, aes(x = mes, group = 1)) +
  ggtitle("Disponibilidades Financeiras") +
  geom_bar(aes(y = disponivel, fill = "Disponível"), stat = "identity", position = position_dodge(), na.rm = TRUE) +
  geom_line(aes(y = bruto, color = "Bruto"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) + 
  scale_color_manual(values = app.color.primary)
g <- app.plot.theming(g)
ggsave("cache/cm_financeiro.png", plot = g)
