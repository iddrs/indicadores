if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)

ds_file <- "indicadores pm.xlsx"
df <- readxl::read_excel(ds_file, sheet = "Dados")
data_base <- as.Date(df$data_base[1])
foutput <- paste("indicadores pm ", format(data_base, format = "%Y-%m"), ".html", sep = "")
print(getwd())
print(foutput)

rmarkdown::render(
  "indicadores_pm.Rmd",
  output_dir = paste(getwd(), "output", sep = "/"),
  output_file = foutput,
  clean = TRUE,
  intermediates_dir = "cache",
  output_format = html_document(
    toc = TRUE,
    toc_depth = 6,
    toc_float = TRUE,
    number_sections = TRUE,
    anchor_sections = TRUE,
    fig_caption = TRUE
  )
)