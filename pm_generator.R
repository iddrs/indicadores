# Gerador de tabelas para indicadores: prefeitura

if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("scales")) install.packages("scales")
library(scales)

if(!require("kableExtra")) install.packages("kableExtra")
library(kableExtra)

source("funcoes.R")

# Carrega dados
ds_file <- "indicadores pm.xlsx"
df <- readxl::read_excel(ds_file, sheet = "Dados")
data_base <- as.Date(df$data_base[1])
periodo <- format(data_base, format = "%B de %Y")
ano_base <- as.integer(format(data_base, format = "%Y"))
ano_anterior <- ano_base - 1
writeLines(paste("\\date{", periodo, "}", sep = ""), "cache/data.tex")
writeLines(paste("indicadores pm ", format(data_base, format = "%Y-%m"), ".pdf", sep = ""), "cache/arquivo.txt")

# Indicadores Legais
  
## Aplicação de Recursos de Impostos e Transferências de Impostos em Manutenção e Desenvolvimento do Ensino


df <- readxl::read_excel(ds_file, sheet = "MDE")
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano>=ano_anterior,]
df$ano <- as.factor((df$ano))

data <- select(df, ano, mes, indice)
data$indice <- percent(data$indice)
colnames(data) <- c("Ano", "Mês", "Índice")
data <- mutate(data, Mês = factor(Mês, levels = unique(Mês)))
data <- spread(data, Ano, Índice)
tbl = app.table.default(data, align=c("l", "r"), caption = "Evolução do índice de MDE", col.names = colnames(data))
app.table.save(tbl, 'pm_mde_1')


data <- select(df, mes, indice,	minimo, ano)
g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Evolução mensal do índice de MDE") +
  geom_bar(aes(y = indice), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_line(aes(y = minimo), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
app.plot.save('pm_mde_1', g)


## Aplicação dos Recursos Recebidos do FUNDEB na Remuneração dos Profissionais da Educação

df <- readxl::read_excel(ds_file, sheet = "FUNDEB")
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano>=ano_anterior,]
df$ano <- as.factor((df$ano))

data <- select(df, ano, mes, indice)
data$indice <- percent(data$indice)
colnames(data) <- c("Ano", "Mês", "Índice")
data <- mutate(data, Mês = factor(Mês, levels = unique(Mês)))
data <- spread(data, Ano, Índice)
tbl = app.table.default(data, align=c("l", "r"), caption = "Evolução do índice de aplicação do FUNDEB na remuneração da Educação", col.names = colnames(data))
app.table.save(tbl, 'pm_fundeb_1')


data <- select(df, mes, indice,	minimo, ano)
g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Evolução mensal da aplicação do FUNDEB em remuneração da Educação") +
  geom_bar(aes(y = indice), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_line(aes(y = minimo), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
app.plot.save('pm_fundeb_1', g)

## Aplicação de Recursos de Impostos e Transferências de Impostos em Ações e Serviços Públicos de Saúde

df <- readxl::read_excel(ds_file, sheet = "ASPS")
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano>=ano_anterior,]
df$ano <- as.factor((df$ano))

data <- select(df, ano, mes, indice)
data$indice <- percent(data$indice, accuracy = 0.01)
colnames(data) <- c("Ano", "Mês", "Índice")
data <- mutate(data, Mês = factor(Mês, levels = unique(Mês)))
data <- spread(data, Ano, Índice)
tbl = app.table.default(data, align=c("l", "r"), caption = "Evolução do índice de ASPS", col.names = colnames(data))
app.table.save(tbl, 'pm_asps_1')


data <- select(df, mes, indice,	minimo, ano)
g <- ggplot(data, aes(x = mes, group = ano, fill = ano)) +
  ggtitle("Evolução mensal do índice de ASPS") +
  geom_bar(aes(y = indice), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_line(aes(y = minimo), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(df$mes)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
app.plot.save('pm_asps_1', g)


## Limite de Despesa Total com Pessoal da LRF


df <- readxl::read_excel(ds_file, sheet = "PessoalLRF")
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))

data <- select(df, mes, indice)
data$indice <- percent(data$indice, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Índice")
tbl = app.table.default(data, align=c("l", "r"), caption = "Evolução do índice de Despesa Total com Pessoal", col.names = colnames(data))
app.table.save(tbl, 'pm_dtp_1')


g <- ggplot(df, aes(x = mes, group = 1)) +
  ggtitle("Despesa Total com Pessoal (LRF)") +
  geom_bar(aes(y = indice, fill = "Índice"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = indice, label = percent(indice, accuracy = 0.01)), vjust = -0.5, color = app.color.primary, na.rm = TRUE, check_overlap = TRUE) +
  geom_line(aes(y = legal, linetype = "Limite"), na.rm = TRUE) +
  geom_line(aes(y = alerta, linetype = "Alerta"), na.rm = TRUE) +
  geom_line(aes(y = prudencial, linetype = "Prudencial"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_dtp_1', g)


## Limite de Suplementação Autorizado na Lei Orçamentária Anual

df <- readxl::read_excel(ds_file, sheet = "Suplementacao", skip = 11)
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))

data <- select(df, mes, vl_autorizado, vl_utilizado, vl_disponivel, perc_utilizado, perc_esperado)
data$perc_utilizado <- percent(data$perc_utilizado, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_esperado <- percent(data$perc_esperado, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Valor autorizado", "Valor utilizado", "Valor disponível", "Limite utilizado", "Limite esperado")
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r", "r"), caption = "Limite de suplementação autorizado na LOA", col.names = colnames(data))
app.table.save(tbl, 'pm_suplementacao_1')


data <- select(df, mes, perc_esperado, perc_utilizado, perc_limite)
g <- ggplot(data, aes(x = mes, group = 1)) +
  ggtitle("Limite de suplementação autorizado na LOA") +
  geom_bar(aes(y = perc_utilizado, fill = "Utilizado"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = perc_utilizado, label = percent(perc_utilizado, accuracy = 0.01, big.mark = ".", decimal.mark = ",")), vjust = -0.5, color = app.color.primary, na.rm = TRUE, check_overlap = TRUE) +
  geom_line(aes(y = perc_esperado, linetype = "Máximo"), na.rm = TRUE) +
  geom_line(aes(y = perc_limite, linetype = "Esperado"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) +
  scale_linetype_manual(values = c("solid", "dashed"))
g <- app.plot.theming(g)
app.plot.save('pm_suplementacao_1', g)

data <- select(df, mes, vl_autorizado, vl_utilizado, vl_esperado)
g <- ggplot(data, aes(x = mes, group = 1)) +
  ggtitle("Valor da suplementação autorizada na LOA") +
  geom_bar(aes(y = vl_utilizado, fill = "Utilizado"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_line(aes(y = vl_esperado, linetype = "Esperado"), na.rm = TRUE) +
  geom_line(aes(y = vl_autorizado, linetype = "Limite"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) +
  scale_linetype_manual(values = c("dashed", "solid"))
g <- app.plot.theming(g)
app.plot.save('pm_suplementacao_2', g)


## Limite da Despesa Corrente como Proporção da Receita Corrente

df <- readxl::read_excel(ds_file, sheet = "ReceitaDespesaCorrentes", skip = 14)
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))


data <- select(df, mes, indice)
data$indice <- percent(data$indice, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Índice")
tbl = app.table.default(data, align=c("l", "r"), caption = "Relação Receita/Despesa Correntes (CF 1988)", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_despesas_limite_1')

g <- ggplot(df, aes(x = mes, group = 1)) +
  ggtitle("Evolução da relação Receita/Despesa Correntes (CF 1988)") +
  geom_bar(aes(y = indice, fill = "Índice"), stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(aes(y = indice, label = percent(indice, accuracy = 0.01, big.mark = ".", decimal.mark = ",")), vjust = -0.5, color = app.color.primary, na.rm = TRUE, check_overlap = TRUE) +
  geom_line(aes(y = maximo, linetype = "Limite"), na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = df$mes) +
  scale_fill_manual(values = app.color.primary) +
  scale_linetype_manual(values = c("solid"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_despesas_limite_1', g)



# Indicadores Gerenciais

## Resultado da Dotação de Pessoal e Encargos Sociais


df <- readxl::read_excel(ds_file, sheet = "DotacaoFolha", skip = 42)
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))


data <- select(df, mes, com_contratos, sem_contratos)
colnames(data) <- c("Mês", "com Contratos", "sem Contratos")
tbl = app.table.default(data, align=c("l", "r", "r"), caption = "Superávit/Déficit da Dotação de Pessoal e Encargos Sociais", col.names = colnames(data))
app.table.save(tbl, 'pm_dotacao_folha_1')


data <- select(df, mes, com_contratos, sem_contratos)
colnames(data) <- c("Mês", "com Contratos", "sem Contratos")
data <- gather(data, tipo, valor, c(`com Contratos`, `sem Contratos`))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Evolução do Superávit/Déficit da Dotação de Pessoal e Encargos Sociais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
app.plot.save('pm_dotacao_folha_1', g)


## Resultado da Dotação do Auxílio-Alimentação

df <- readxl::read_excel(ds_file, sheet = "DotacaoVale", skip = 13)
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))


data <- select(df, mes, valor)
colnames(data) <- c("Mês", "Valor")
tbl = app.table.default(data, align=c("l", "r"), caption = "Superávit/Déficit da Dotação de Vale-Alimentação", col.names = colnames(data))
app.table.save(tbl, 'pm_dotacao_vale_1')


data <- select(df, mes, valor)

g <- ggplot(data, aes(x = mes)) +
  ggtitle("Evolução do Superávit/Déficit da Dotação de Vale-Alimentação") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE, color = app.color.primary) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = data$mes)
g <- app.plot.theming(g)
app.plot.save('pm_dotacao_vale_1', g)


## Saldo de Caixa Projetado


df <- readxl::read_excel(ds_file, sheet = "CaixaProjetado")
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))


data <- select(df, mes, total, proprio)
colnames(data) <- c("Mês", "Recurso próprio", "Total (exceto RPPS)")
tbl = app.table.default(data, align=c("l", "r", "r"), caption = "Saldo de caixa projetado", col.names = colnames(data))
app.table.save(tbl, 'pm_caixa_projetado_1')


data <- select(df, mes, total, proprio)
colnames(data) <- c("Mês", "Total (exceto RPPS)", "Próprio")
data <- gather(data, tipo, valor, c(`Total (exceto RPPS)`, `Próprio`))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Acompanhamento do saldo de caixa projetado") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary))
g <- app.plot.theming(g)
app.plot.save('pm_caixa_projetado_1', g)


## Superávit/Déficit Financeiro Atual


df <- readxl::read_excel(ds_file, sheet = "Superavit", skip = 4)
df$ano <- format(df$data_base, format = "%Y")
df$mes <- as.factor(format(df$data_base, format = "%b"))
df <- df[df$ano==ano_base,]
df$ano <- as.factor((df$ano))


data <- select(df, mes, valor)
colnames(data) <- c("Mês", "Valor")
tbl = app.table.default(data, align=c("l", "r"), caption = "Superávit/Déficit Financeiro Atual", col.names = colnames(data))
app.table.save(tbl, 'pm_superavit_1')

data <- select(df, mes, valor)

g <- ggplot(data, aes(x = mes)) +
  ggtitle("Evolução dos recursos financeiros disponíveis para empenho") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE, color = app.color.primary) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = data$mes)
g <- app.plot.theming(g)
app.plot.save('pm_superavit_1', g)

## Receita

### Receita Total

df <- readxl::read_excel(ds_file, sheet = "ReceitaTotal", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Receita Total: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_total_1')

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Receita Total: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_total_2')

data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Receita Total: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_total_3')

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Receita Total: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_total_1', g)

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Receita Total: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_total_2', g)

### Receita Corrente


df <- readxl::read_excel(ds_file, sheet = "ReceitaCorrente", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Receita Corrente: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_corrente_1')

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Receita Corrente: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_corrente_2')

data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Receita Corrente: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_corrente_3')

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Receita Corrente: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_corrente_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Receita Corrente: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_corrente_2', g)

### Arrecadação Própria

# A arrecadação própria engloba aquelas receitas orçamentárias arrecadadas diretamente pelo município, ou seja, não são receitas advindas de transferências de outros entes e são decorrentes do esforço direto do órgão arrecadados e outros.


df <- readxl::read_excel(ds_file, sheet = "ArrecadacaoPropria", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Arrecadação Própria: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_propria_1')

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Arrecadação Própria: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_propria_2')

data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Arrecadação Própria: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_propria_3')

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Arrecadação Própria: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_propria_1', g)

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Arrecadação Própria: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_propria_2', g)

### Transferências Correntes


df <- readxl::read_excel(ds_file, sheet = "TransfCorr", skip = 2)
df$mes <- as.factor(df$mes)


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_trans_corr_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_trans_corr_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferências Correntes: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_trans_corr_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferências Correntes: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_trans_corr_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferências Correntes: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_trans_corr_2', g)

### FPM – cota-mensal


df <- readxl::read_excel(ds_file, sheet = "FPM", skip = 2)
df$mes <- as.factor(df$mes)


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "FPM - cota mensal: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_fpm_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "FPM - cota mensal: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_fpm_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "FPM - cota mensal: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_fpm_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("FPM - cota mensal: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_fpm_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("FPM - cota mensal: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_fpm_2', g)

### ICMS


df <- readxl::read_excel(ds_file, sheet = "ICMS", skip = 2)
df$mes <- as.factor(df$mes)


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "ICMS: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_icms_1')

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "ICMS: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_icms_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "ICMS: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_icms_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("ICMS: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_icms_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("ICMS: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_icms_2', g)


### Transferência do FUNDEB


df <- readxl::read_excel(ds_file, sheet = "TransfFUNDEB", skip = 2)
df$mes <- as.factor(df$mes)


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferência do FUNDEB: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_fundeb_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferência do FUNDEB: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_fundeb_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferência do FUNDEB: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_transf_fundeb_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferência do FUNDEB: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_fundeb_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferência do FUNDEB: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_fundeb_2', g)

### Transferências Correntes da Saúde


df <- readxl::read_excel(ds_file, sheet = "TransfSaude", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes da Saúde: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_saude_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes da Saúde: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_saude_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferências Correntes da Saúde: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_transf_corr_saude_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferências Correntes da Saúde: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_saude_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferências Correntes da Saúde: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_saude_2', g)

### Transferências Correntes da Educação


df <- readxl::read_excel(ds_file, sheet = "TransfEducacao", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes da Educação: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_educacao_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes da Educação: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_educacao_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferências Correntes da Educação: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_transf_corr_educacao_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferências Correntes da Educação: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_educacao_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferências Correntes da Educação: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_educacao_2', g)

### Transferências Correntes da Assistência Social

df <- readxl::read_excel(ds_file, sheet = "TransfAssistSocial", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes da Assistência Social: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_assist_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes da Assistência Social: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_assist_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferências Correntes da Assistência Social: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_transf_corr_assist_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferências Correntes da Assistência Social: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_assist_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferências Correntes da Assistência Social: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_assist_2', g)

### Transferências Correntes Federais


df <- readxl::read_excel(ds_file, sheet = "TransfFederal", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes Federais: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_fed_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes Federais: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_fed_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferências Correntes Federais: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_transf_corr_fed_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferências Correntes Federais: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_fed_1', g)


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferências Correntes Federais: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_fed_2', g)

### Transferências Correntes Estaduais


df <- readxl::read_excel(ds_file, sheet = "TransfEstadual", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes Estaduais: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_est_1')


data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Transferências Correntes Estaduais: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_receita_transf_corr_est_2')


data <- select(df, mes, vl_var_ant, vl_var_prev,	var_ant,	var_prev)
data$var_ant <- percent(data$var_ant, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
data$var_prev <- percent(data$var_prev, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
colnames(data) <- c("Mês", paste(ano_base, "(-)", ano_anterior), paste("Arrecadado (-) Previsto", ano_base), paste(ano_base, "/", ano_anterior, "(%)"), paste("Arrecadado / Previsto", ano_base, "(%)"))
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Transferências Correntes Estaduais: Excesso/Frustração", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_receita_transf_corr_est_3')


data <- select(df, mes, arrec_mes_ant, arrec_mes_atual, prev_mes_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Transferências Correntes Estaduais: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_est_1', g)

data <- select(df, mes, arrec_acum_ant, arrec_acum_atual, prev_acum_atual)
colnames(data) <- c("Mês", paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base))
data <- gather(data, tipo, valor, c(paste("Arrecadado em", ano_anterior), paste("Arrecadado em", ano_base), paste("Previsto para", ano_base)))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Transferências Correntes Estaduais: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_receita_transf_corr_est_2', g)

## Despesa

# Nesta seção serão apresentadas informações sobre os principais grupos de despesa, comparando os desempenhos mensal e acumulados com os valores previstos para o ano corrente e empenhados e os liquidados no ano anterior.

### Despesa Total


df <- readxl::read_excel(ds_file, sheet = "DespesaTotal", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, emp_mes, liq_mes, prev_mes)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa Total: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_total_1')

data <- select(df, mes, emp_acum, liq_acum, prev_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa Total: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_total_2')


data <- select(df, mes, vl_emp_prev, vl_liq_prev, perc_emp_prev,	perc_liq_prev)
data$perc_emp_prev <- percent(data$perc_emp_prev, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_liq_prev <- percent(data$perc_liq_prev, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Empenhado (-) Previsto", "Liquidado (-) Previsto", "Empenhado / Previsto", "Liquidado / Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Despesa Total: previsto x despesa", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_despesa_total_3')


data <- select(df, mes, arrec_acum,	perc_emp_arrec,	perc_liq_arrec)
data$perc_emp_arrec <- percent(data$perc_emp_arrec, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_liq_arrec <- percent(data$perc_liq_arrec, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Arrecadado", "Empenhado / Arrecadado", "Liquidado / Arrecadado")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa Total: arrecadação x despesa", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_total_4')


data <- select(df, mes, emp_mes, liq_mes, prev_mes)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Previsto"))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Despesa Total: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_total_1', g)


data <- select(df, mes, emp_acum, liq_acum, prev_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Previsto"))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Despesa Total: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_total_2', g)


data <- select(df, mes, emp_acum, liq_acum, arrec_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Arrecadação")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Arrecadação"))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Despesa Total: executado x arrecadação") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_total_3', g)


### Despesa Corrente
  

df <- readxl::read_excel(ds_file, sheet = "DespesaCorrente", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, emp_mes, liq_mes, prev_mes)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa Corrente: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_corr_1')

data <- select(df, mes, emp_acum, liq_acum, prev_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa Corrente: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_corr_2')


data <- select(df, mes, vl_emp_prev, vl_liq_prev, perc_emp_prev,	perc_liq_prev)
data$perc_emp_prev <- percent(data$perc_emp_prev, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_liq_prev <- percent(data$perc_liq_prev, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Empenhado (-) Previsto", "Liquidado (-) Previsto", "Empenhado / Previsto", "Liquidado / Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Despesa Corrente: previsto x despesa", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_despesa_corr_3')


data <- select(df, mes, arrec_acum,	perc_emp_arrec,	perc_liq_arrec)
data$perc_emp_arrec <- percent(data$perc_emp_arrec, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_liq_arrec <- percent(data$perc_liq_arrec, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Arrecadado", "Empenhado / Arrecadado", "Liquidado / Arrecadado")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa Corrente: arrecadação x despesa", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_corr_4')


data <- select(df, mes, emp_mes, liq_mes, prev_mes)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Previsto"))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Despesa Corrente: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_corr_1', g)


data <- select(df, mes, emp_acum, liq_acum, prev_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Previsto"))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Despesa Corrente: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_corr_2', g)


data <- select(df, mes, emp_acum, liq_acum, arrec_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Arrecadação")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Arrecadação"))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Despesa Corrente: executado x arrecadação") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_corr_3', g)


### Despesa com Pessoal e Encargos Sociais
  
df <- readxl::read_excel(ds_file, sheet = "DespesaPessoal", skip = 2)
df$mes <- as.factor(df$mes)

data <- select(df, mes, emp_mes, liq_mes, prev_mes)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa com Pessoal e Encargos Sociais: valores mensais", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_pessoal_1')


data <- select(df, mes, emp_acum, liq_acum, prev_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa com Pessoal e Encargos Sociais: valores acumulados", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_pessoal_2')


data <- select(df, mes, vl_emp_prev, vl_liq_prev, perc_emp_prev,	perc_liq_prev)
data$perc_emp_prev <- percent(data$perc_emp_prev, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_liq_prev <- percent(data$perc_liq_prev, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Empenhado (-) Previsto", "Liquidado (-) Previsto", "Empenhado / Previsto", "Liquidado / Previsto")
tbl = app.table.default(data, align=c("l", "r", "r", "r", "r"), caption = "Despesa com Pessoal e Encargos Sociais: previsto x despesa", col.names = colnames(data))
tbl <- column_spec(tbl, 2:5, width = "3.2cm")
app.table.save(tbl, 'pm_despesa_pessoal_3')


data <- select(df, mes, arrec_acum,	perc_emp_arrec,	perc_liq_arrec)
data$perc_emp_arrec <- percent(data$perc_emp_arrec, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
data$perc_liq_arrec <- percent(data$perc_liq_arrec, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
colnames(data) <- c("Mês", "Arrecadado", "Empenhado / Arrecadado", "Liquidado / Arrecadado")
tbl = app.table.default(data, align=c("l", "r", "r", "r"), caption = "Despesa com Pessoal e Encargos Sociais: arrecadação x despesa", col.names = colnames(data))
app.table.save(tbl, 'pm_despesa_pessoal_4')


data <- select(df, mes, emp_mes, liq_mes, prev_mes)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Previsto"))
data$tipo <- as.factor(data$tipo)

g <- ggplot(data, aes(x = Mês, group = tipo, fill = tipo)) +
  ggtitle("Despesa com Pessoal e Encargos Sociais: valores mensais") +
  geom_bar(aes(y = valor), stat = "identity", position = "dodge", na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_fill_manual(values = c(app.color.secondary, app.color.primary, app.color.ternary))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_pessoal_1', g)

data <- select(df, mes, emp_acum, liq_acum, prev_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Previsto")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Previsto"))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Despesa com Pessoal e Encargos Sociais: valores acumulados") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_pessoal_2', g)


data <- select(df, mes, emp_acum, liq_acum, arrec_acum)
colnames(data) <- c("Mês", "Empenhado", "Liquidado", "Arrecadação")
data <- gather(data, tipo, valor, c("Empenhado", "Liquidado", "Arrecadação"))
data$tipo <- as.factor(data$tipo)
g <- ggplot(data, aes(x = Mês, group = tipo, linetype = tipo)) +
  ggtitle("Despesa com Pessoal e Encargos Sociais: executado x arrecadação") +
  geom_line(aes(y = valor), na.rm = TRUE) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_discrete(limits = unique(data$Mês)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
g <- app.plot.theming(g)
app.plot.save('pm_despesa_pessoal_3', g)
