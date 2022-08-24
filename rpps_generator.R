# Gerador de tabelas para indicadores: RPPS

if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("scales")) install.packages("scales")
library(scales)

if(!require("kableExtra")) install.packages("kableExtra")
library(kableExtra)

source("funcoes.R")

# Carrega dados
ds_file <- "indicadores rpps.xlsx"
df <- readxl::read_excel(ds_file, sheet = "Periodo")
data_base <- as.Date(df$data_base[1])
periodo <- format(data_base, format = "%B de %Y")
ano_base <- format(data_base, format = "%Y")
writeLines(paste("\\date{", periodo, "}", sep = ""), "cache/data.tex")

# Receita Total
df <- readxl::read_excel(ds_file, sheet = "ReceitaTotal")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, arrec_ant,	prev_mes,	arrec_mes),
  align=c("l", "r", "r", "r"),
  caption = "Receita Total (Mensal)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_total_1.tex")

tbl = app.table.default(
  select(df, mes, arrec_ant_acum,	prev_acum,	arrec_acum),
  align=c("l", "r", "r", "r"),
  caption = "Receita Total (Acumulado)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_total_2.tex")

data <- select(df, mes, arrec_ant,	arrec_mes)
ant <- select(data, mes, arrec_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant
ant$arrec_ant <- NULL
atual <- select(data, mes, arrec_mes)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_mes
atual$arrec_mes <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Receita Total: ano anterior x ano atual - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_total_1.png", plot = g)

prev <- select(df, mes, prev_mes)
prev$valor <- prev$prev_mes
prev$prev_mes <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_mes)
arrec$valor <- arrec$arrec_mes
arrec$arrec_mes <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Receita Total: previsto x arrecadado - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.primary, app.color.secondary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_total_2.png", plot = g)

data <- select(df, mes, arrec_ant_acum,	arrec_acum)
ant <- select(data, mes, arrec_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant_acum
ant$arrec_ant_acum <- NULL
atual <- select(data, mes, arrec_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_acum
atual$arrec_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle("Receita Total: ano anterior x ano atual - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_total_3.png", plot = g)

prev <- select(df, mes, prev_acum)
prev$valor <- prev$prev_acum
prev$prev_acum <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_acum)
arrec$valor <- arrec$arrec_acum
arrec$arrec_acum <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, linetype = tipo)) +
  ggtitle("Receita Total: previsto x arrecadado - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_total_4.png", plot = g)

# Contribuição dos Servidores Ativos, Inativos e Pensionistas
df <- readxl::read_excel(ds_file, sheet = "ContribServ")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, arrec_ant,	prev_mes,	arrec_mes),
  align=c("l", "r", "r", "r"),
  caption = "Contribuição dos Servidores (Mensal)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_contrib_serv_1.tex")

tbl = app.table.default(
  select(df, mes, arrec_ant_acum,	prev_acum,	arrec_acum),
  align=c("l", "r", "r", "r"),
  caption = "Contribuição dos Servidores (Acumulado)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_contrib_serv_2.tex")

data <- select(df, mes, arrec_ant,	arrec_mes)
ant <- select(data, mes, arrec_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant
ant$arrec_ant <- NULL
atual <- select(data, mes, arrec_mes)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_mes
atual$arrec_mes <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Contribuição dos Servidores: ano anterior x ano atual - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_serv_1.png", plot = g)

prev <- select(df, mes, prev_mes)
prev$valor <- prev$prev_mes
prev$prev_mes <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_mes)
arrec$valor <- arrec$arrec_mes
arrec$arrec_mes <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Contribuição dos Servidores: previsto x arrecadado - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.primary, app.color.secondary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_serv_2.png", plot = g)

data <- select(df, mes, arrec_ant_acum,	arrec_acum)
ant <- select(data, mes, arrec_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant_acum
ant$arrec_ant_acum <- NULL
atual <- select(data, mes, arrec_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_acum
atual$arrec_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle("Contribuição dos Servidores: ano anterior x ano atual - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_serv_3.png", plot = g)

prev <- select(df, mes, prev_acum)
prev$valor <- prev$prev_acum
prev$prev_acum <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_acum)
arrec$valor <- arrec$arrec_acum
arrec$arrec_acum <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, linetype = tipo)) +
  ggtitle("Contribuição dos Servidores: previsto x arrecadado - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_serv_4.png", plot = g)

# Contribuição Patronal – alíquota normal
df <- readxl::read_excel(ds_file, sheet = "PatronalNormal")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, arrec_ant,	prev_mes,	arrec_mes),
  align=c("l", "r", "r", "r"),
  caption = "Contribuição Patronal Normal (Mensal)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_contrib_patronal_normal_1.tex")

tbl = app.table.default(
  select(df, mes, arrec_ant_acum,	prev_acum,	arrec_acum),
  align=c("l", "r", "r", "r"),
  caption = "Contribuição Patronal Normal (Acumulado)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_contrib_patronal_normal_2.tex")

data <- select(df, mes, arrec_ant,	arrec_mes)
ant <- select(data, mes, arrec_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant
ant$arrec_ant <- NULL
atual <- select(data, mes, arrec_mes)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_mes
atual$arrec_mes <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Contribuição Patronal Normal: ano anterior x ano atual - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_normal_1.png", plot = g)

prev <- select(df, mes, prev_mes)
prev$valor <- prev$prev_mes
prev$prev_mes <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_mes)
arrec$valor <- arrec$arrec_mes
arrec$arrec_mes <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Contribuição Patronal Normal: previsto x arrecadado - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.primary, app.color.secondary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_normal_2.png", plot = g)

data <- select(df, mes, arrec_ant_acum,	arrec_acum)
ant <- select(data, mes, arrec_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant_acum
ant$arrec_ant_acum <- NULL
atual <- select(data, mes, arrec_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_acum
atual$arrec_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle("Contribuição Patronal Normal: ano anterior x ano atual - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_normal_3.png", plot = g)

prev <- select(df, mes, prev_acum)
prev$valor <- prev$prev_acum
prev$prev_acum <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_acum)
arrec$valor <- arrec$arrec_acum
arrec$arrec_acum <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, linetype = tipo)) +
  ggtitle("Contribuição Patronal Normal: previsto x arrecadado - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_normal_4.png", plot = g)

# Contribuição Patronal – alíquota suplementar
df <- readxl::read_excel(ds_file, sheet = "PatronalSuplementar")
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, arrec_ant,	prev_mes,	arrec_mes),
  align=c("l", "r", "r", "r"),
  caption = "Contribuição Patronal Suplementar (Mensal)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_contrib_patronal_suplem_1.tex")

tbl = app.table.default(
  select(df, mes, arrec_ant_acum,	prev_acum,	arrec_acum),
  align=c("l", "r", "r", "r"),
  caption = "Contribuição Patronal Suplementar (Acumulado)",
  col.names = c("Mês", "Ano Anterior", "Previsto", "Arrecadado")
)
writeLines(tbl, "cache/rpps_receita_contrib_patronal_suplem_2.tex")

data <- select(df, mes, arrec_ant,	arrec_mes)
ant <- select(data, mes, arrec_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant
ant$arrec_ant <- NULL
atual <- select(data, mes, arrec_mes)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_mes
atual$arrec_mes <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Contribuição Patronal Suplementar: ano anterior x ano atual - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_suplem_1.png", plot = g)

prev <- select(df, mes, prev_mes)
prev$valor <- prev$prev_mes
prev$prev_mes <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_mes)
arrec$valor <- arrec$arrec_mes
arrec$arrec_mes <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Contribuição Patronal Suplementar: previsto x arrecadado - valor mensal") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.primary, app.color.secondary))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_suplem_2.png", plot = g)

data <- select(df, mes, arrec_ant_acum,	arrec_acum)
ant <- select(data, mes, arrec_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant_acum
ant$arrec_ant_acum <- NULL
atual <- select(data, mes, arrec_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_acum
atual$arrec_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle("Contribuição Patronal Suplementar: ano anterior x ano atual - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_suplem_3.png", plot = g)

prev <- select(df, mes, prev_acum)
prev$valor <- prev$prev_acum
prev$prev_acum <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_acum)
arrec$valor <- arrec$arrec_acum
arrec$arrec_acum <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, linetype = tipo)) +
  ggtitle("Contribuição Patronal Suplementar: previsto x arrecadado - valor acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
ggsave("cache/rpps_receita_contrib_patronal_suplem_4.png", plot = g)

# Disponibilidades de Caixa do RPPS
df <- readxl::read_excel(ds_file, sheet = "Caixa", skip = 1)
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, projetado,	atual),
  align=c("l", "r", "r"),
  caption = "Saldo de Caixa e Equivalentes de Caixa e Investimentos",
  col.names = c("Mês", "Saldo Projetado", "Saldo Efetivo")
)
writeLines(tbl, "cache/rpps_disponibilidades_1.tex")

t1 <- select(df, mes, projetado)
t1$valor <- t1$projetado
t1$projetado <- NULL
t1$tipo <- "Saldo projetado"
t2 <- select(df, mes, atual)
t2$valor <- t2$atual
t2$atual <- NULL
t2$tipo <- "Saldo efetivo"
data <- rbind(t1, t2)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, linetype = tipo)) +
  ggtitle("Saldo de caixa e investimentos: projetado x efetivo") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
ggsave("cache/rpps_disponibilidades_1.png", plot = g)

# Valorização/Desvalorização dos Investimentos do RPPS
df <- readxl::read_excel(ds_file, sheet = "GanhosPerdasRend", range = cellranger::cell_cols("A:G"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, arrec_ant,	prev_mes, arrec_mes),
  align=c("l", "r", "r", "r", "r"),
  caption = "Variação das aplicações financeiras: valores mensais",
  col.names = c("Mês", paste("Desempenho", as.integer(ano_base) - 1), paste("Previsto", ano_base), paste("Desempenho", ano_base))
)
writeLines(tbl, "cache/rpps_disponibilidades_resultado_1.tex")

tbl = app.table.default(
  select(df, mes, arrec_ant_acum,	prev_acum, arrec_acum),
  align=c("l", "r", "r", "r", "r"),
  caption = "Variação das aplicações financeiras: valores acumulados",
  col.names = c("Mês", paste("Desempenho", as.integer(ano_base) - 1), paste("Previsto", ano_base), paste("Desempenho", ano_base))
)
writeLines(tbl, "cache/rpps_disponibilidades_resultado_2.tex")

data <- select(df, mes, arrec_ant,	arrec_mes)
ant <- select(data, mes, arrec_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant
ant$arrec_ant <- NULL
atual <- select(data, mes, arrec_mes)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_mes
atual$arrec_mes <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Desempenho mensal dos investimentos:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_disponibilidades_resultado_1.png", plot = g)

prev <- select(df, mes, prev_mes)
prev$valor <- prev$prev_mes
prev$prev_mes <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_mes)
arrec$valor <- arrec$arrec_mes
arrec$arrec_mes <- NULL
arrec$tipo <- "Realizado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Desempenho mensal dos investimentos: previsto x realizado") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_disponibilidades_resultado_2.png", plot = g)

data <- select(df, mes, arrec_ant_acum,	arrec_acum)
ant <- select(data, mes, arrec_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$arrec_ant_acum
ant$arrec_ant_acum <- NULL
atual <- select(data, mes, arrec_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$arrec_acum
atual$arrec_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Desempenho acumulado dos investimentos:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_disponibilidades_resultado_3.png", plot = g)

prev <- select(df, mes, prev_acum)
prev$valor <- prev$prev_acum
prev$prev_acum <- NULL
prev$tipo <- "Previsto"
arrec <- select(df, mes, arrec_acum)
arrec$valor <- arrec$arrec_acum
arrec$arrec_acum <- NULL
arrec$tipo <- "Arrecadado"
data <- rbind(prev, arrec)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, linetype = tipo)) +
  ggtitle("Desempenho acumulado dos investimentos: previsto x acumulado") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
ggsave("cache/rpps_disponibilidades_resultado_4.png", plot = g)

# Despesa Total
df <- readxl::read_excel(ds_file, sheet = "DespesaTotal", range = cellranger::cell_cols("A:E"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, emp_ant, emp_atual),
  align=c("l", "r", "r"),
  caption = "Despesa Total: empenhado mensal",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_total_1.tex")

tbl = app.table.default(
  select(df, mes, emp_ant_acum, emp_atual_acum),
  align=c("l", "r", "r"),
  caption = "Despesa Total: empenhado acumulado",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_total_2.tex")

data <- select(df, mes, emp_ant,	emp_atual)
ant <- select(data, mes, emp_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant
ant$emp_ant <- NULL
atual <- select(data, mes, emp_atual)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual
atual$emp_atual <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Despesa Total Mensal:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_total_1.png", plot = g)

data <- select(df, mes, emp_ant_acum,	emp_atual_acum)
ant <- select(data, mes, emp_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant_acum
ant$emp_ant_acum <- NULL
atual <- select(data, mes, emp_atual_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual_acum
atual$emp_atual_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Despesa Total Acumulada:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_total_2.png", plot = g)

# Despesa com Inativos e Pensionistas do RPPS
df <- readxl::read_excel(ds_file, sheet = "InativPensRPPS", range = cellranger::cell_cols("A:E"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, emp_ant, emp_atual),
  align=c("l", "r", "r"),
  caption = "Despesa com Inativos e Pensionistas do RPPS: empenhado mensal",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_inativos_rpps_.tex")

tbl = app.table.default(
  select(df, mes, emp_ant_acum, emp_atual_acum),
  align=c("l", "r", "r"),
  caption = "Despesa com Inativos e Pensionistas do RPPS: empenhado acumulado",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_inativos_rpps_2.tex")

data <- select(df, mes, emp_ant,	emp_atual)
ant <- select(data, mes, emp_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant
ant$emp_ant <- NULL
atual <- select(data, mes, emp_atual)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual
atual$emp_atual <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Despesa Mensal com Inativos e Pensionistas do RPPS:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_inativos_rpps_1.png", plot = g)

data <- select(df, mes, emp_ant_acum,	emp_atual_acum)
ant <- select(data, mes, emp_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant_acum
ant$emp_ant_acum <- NULL
atual <- select(data, mes, emp_atual_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual_acum
atual$emp_atual_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Despesa Acumulada com Inativos e Pensionistas do RPPS:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_inativos_rpps_2.png", plot = g)

# Despesa com Inativos e Pensionistas do Tesouro
df <- readxl::read_excel(ds_file, sheet = "InativPensTesouro", range = cellranger::cell_cols("A:E"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, emp_ant, emp_atual),
  align=c("l", "r", "r"),
  caption = "Despesa com Inativos e Pensionistas do Tesouro: empenhado mensal",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_inativos_tesouro_1.tex")

tbl = app.table.default(
  select(df, mes, emp_ant_acum, emp_atual_acum),
  align=c("l", "r", "r"),
  caption = "Despesa com Inativos e Pensionistas do Tesouro: empenhado acumulado",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_inativos_tesouro_2.tex")

data <- select(df, mes, emp_ant,	emp_atual)
ant <- select(data, mes, emp_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant
ant$emp_ant <- NULL
atual <- select(data, mes, emp_atual)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual
atual$emp_atual <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Despesa Mensal com Inativos e Pensionistas do Tesouro:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_inativos_tesouro_1.png", plot = g)

data <- select(df, mes, emp_ant_acum,	emp_atual_acum)
ant <- select(data, mes, emp_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant_acum
ant$emp_ant_acum <- NULL
atual <- select(data, mes, emp_atual_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual_acum
atual$emp_atual_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Despesa Acumulada com Inativos e Pensionistas do Tesouro:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_inativos_tesouro_2.png", plot = g)

# Despesas Administrativas do RPPS
df <- readxl::read_excel(ds_file, sheet = "Manut", range = cellranger::cell_cols("A:E"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, emp_ant, emp_atual),
  align=c("l", "r", "r"),
  caption = "Despesas Administrativas do RPPS: empenhado mensal",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_adm_1.tex")

tbl = app.table.default(
  select(df, mes, emp_ant_acum, emp_atual_acum),
  align=c("l", "r", "r"),
  caption = "Despesas Administrativas do RPPS: empenhado acumulado",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_adm_2.tex")

data <- select(df, mes, emp_ant,	emp_atual)
ant <- select(data, mes, emp_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant
ant$emp_ant <- NULL
atual <- select(data, mes, emp_atual)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual
atual$emp_atual <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Despesas Administrativas Mensais do RPPS:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_adm_1.png", plot = g)

data <- select(df, mes, emp_ant_acum,	emp_atual_acum)
ant <- select(data, mes, emp_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant_acum
ant$emp_ant_acum <- NULL
atual <- select(data, mes, emp_atual_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual_acum
atual$emp_atual_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Despesas Administrativas Acumulada do RPPS:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_adm_2.png", plot = g)

# Despesas com Aposentadorias
df <- readxl::read_excel(ds_file, sheet = "Aposentadorias", range = cellranger::cell_cols("A:E"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, emp_ant, emp_atual),
  align=c("l", "r", "r"),
  caption = "Despesas com Aposentadorias: empenhado mensal",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_aposent_1.tex")

tbl = app.table.default(
  select(df, mes, emp_ant_acum, emp_atual_acum),
  align=c("l", "r", "r"),
  caption = "Despesas com Aposentadorias: empenhado acumulado",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_aposent_2.tex")

data <- select(df, mes, emp_ant,	emp_atual)
ant <- select(data, mes, emp_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant
ant$emp_ant <- NULL
atual <- select(data, mes, emp_atual)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual
atual$emp_atual <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Despesas Mensais com Aposentadorias:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_aposent_1.png", plot = g)

data <- select(df, mes, emp_ant_acum,	emp_atual_acum)
ant <- select(data, mes, emp_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant_acum
ant$emp_ant_acum <- NULL
atual <- select(data, mes, emp_atual_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual_acum
atual$emp_atual_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Despesas Acumuladas com Aposentadorias:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_aposent_2.png", plot = g)

# Despesas com Pensões
df <- readxl::read_excel(ds_file, sheet = "Pensoes", range = cellranger::cell_cols("A:E"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, emp_ant, emp_atual),
  align=c("l", "r", "r"),
  caption = "Despesas com Pensões: empenhado mensal",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_pensoes_1.tex")

tbl = app.table.default(
  select(df, mes, emp_ant_acum, emp_atual_acum),
  align=c("l", "r", "r"),
  caption = "Despesas com Pensões: empenhado acumulado",
  col.names = c("Mês", as.integer(ano_base) - 1, ano_base)
)
writeLines(tbl, "cache/rpps_despesa_pensoes_2.tex")

data <- select(df, mes, emp_ant,	emp_atual)
ant <- select(data, mes, emp_ant)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant
ant$emp_ant <- NULL
atual <- select(data, mes, emp_atual)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual
atual$emp_atual <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle(paste("Despesas Mensais com Pensões:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_pensoes_1.png", plot = g)

data <- select(df, mes, emp_ant_acum,	emp_atual_acum)
ant <- select(data, mes, emp_ant_acum)
ant$ano <- (as.integer(ano_base) - 1)
ant$valor <- ant$emp_ant_acum
ant$emp_ant_acum <- NULL
atual <- select(data, mes, emp_atual_acum)
atual$ano <- as.integer(ano_base)
atual$valor <- atual$emp_atual_acum
atual$emp_atual_acum <- NULL
data <- rbind(ant, atual)
data$ano <- as.factor(data$ano)
data$mes <- as.factor(data$mes)

g <- ggplot(data, aes(x = mes, group = ano, linetype = ano)) +
  ggtitle(paste("Despesas Acumuladas com Pensões:", as.integer(ano_base) - 1, "x", ano_base)) +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
ggsave("cache/rpps_despesa_pensoes_2.png", plot = g)

# Dívida do Executivo em Regime de Parcelamento
df <- readxl::read_excel(ds_file, sheet = "Divida")
df$ano <- as.factor(df$ano)

tbl = app.table.default(
  select(df, ano, saldo_inicial, saldo_projetado, valor_pago, pagamento_projetado),
  align=c("l", "r", "r", "r", "r"),
  caption = "Dívida do Executivo em Regime de Parcelamento",
  col.names = c("Ano", "Saldo Inicial", "Saldo Projetado", "Valor Pago", "Pagamento Projetado")
)
writeLines(tbl, "cache/rpps_parcelamento_1.tex")

data <- select(df, ano, saldo_inicial, saldo_projetado)
inicial <- select(data, ano, saldo_inicial)
inicial$valor <- inicial$saldo_inicial
inicial$saldo_inicial <- NULL
inicial$tipo <- "Saldo inicial"
projetado <- select(data, ano, saldo_projetado)
projetado$valor <- projetado$saldo_projetado
projetado$saldo_projetado <- NULL
projetado$tipo <- "Saldo projetado"
data <- rbind(inicial, projetado)
data$ano <- as.factor(data$ano)

g <- ggplot(data, aes(x = ano, group = tipo, linetype = tipo)) +
  ggtitle("Evolução do saldo devedor") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = levels(data$ano)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
g <- app.plot.theming(g)
ggsave("cache/rpps_parcelamento_1.png", plot = g)

data <- select(df, ano, valor_pago, pagamento_projetado)
inicial <- select(data, ano, valor_pago)
inicial$valor <- inicial$valor_pago
inicial$valor_pago <- NULL
inicial$tipo <- "Valor pago"
projetado <- select(data, ano, pagamento_projetado)
projetado$valor <- projetado$pagamento_projetado
projetado$pagamento_projetado <- NULL
projetado$tipo <- "Pagamento projetado"
data <- rbind(inicial, projetado)
data$ano <- as.factor(data$ano)

g <- ggplot(data, aes(x = ano, group = tipo, fill = tipo)) +
  ggtitle("Evolução dos pagamentos") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = levels(data$ano)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
g <- app.plot.theming(g)
ggsave("cache/rpps_parcelamento_2.png", plot = g)

# Compensação Previdenciária
df <- readxl::read_excel(ds_file, sheet = "COMPREV", range = cellranger::cell_cols("A:F"))
df$ano <- as.factor(format(df$data_base, format = "%Y"))
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]

tbl = app.table.default(
  select(df, mes, receita, despesa),
  align=c("l", "r", "r"),
  caption = "Compensação Previdenciária: receitas x despesas (valores mensais)",
  col.names = c("Mês", "Receita", "Despesa")
)
writeLines(tbl, "cache/rpps_comp_prev_1.tex")

tbl = app.table.default(
  select(df, mes, receita_acum, despesa_acum),
  align=c("l", "r", "r"),
  caption = "Compensação Previdenciária: receitas x despesas (valores acumulados)",
  col.names = c("Mês", "Receita", "Despesa")
)
writeLines(tbl, "cache/rpps_comp_prev_2.tex")

receita <- select(df, mes, receita)
receita$valor <- receita$receita
receita$receita <- NULL
receita$tipo <- "Receita"
despesa <- select(df, mes, despesa)
despesa$valor <- despesa$despesa
despesa$despesa <- NULL
despesa$tipo <- "Despesa"
data <- rbind(receita, despesa)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Compensação Previdenciária: receita x despesa (valores mensais)") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.primary, app.color.secondary))
g <- app.plot.theming(g)
ggsave("cache/rpps_comp_prev_1.png", plot = g)

receita <- select(df, mes, receita_acum)
receita$valor <- receita$receita_acum
receita$receita_acum <- NULL
receita$tipo <- "Receita"
despesa <- select(df, mes, despesa_acum)
despesa$valor <- despesa$despesa_acum
despesa$despesa_acum <- NULL
despesa$tipo <- "Despesa"
data <- rbind(receita, despesa)

data$mes <- as.factor(data$mes)
g <- ggplot(data, aes(x = mes, group = tipo, fill = tipo)) +
  ggtitle("Compensação Previdenciária: receita x despesa (valores acumulados)") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = c(app.color.primary, app.color.secondary))
g <- app.plot.theming(g)
ggsave("cache/rpps_comp_prev_2.png", plot = g)
