if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)

ds_file <- "indicadores cm.xlsx"
df <- readxl::read_excel(ds_file, sheet = "Dados")
data_base <- as.Date(df$data_base[1])
foutput <- paste("indicadores cm ", format(data_base, format = "%Y-%m"), ".pdf", sep = "")
print(getwd())
print(foutput)

rmarkdown::render(
  "indicadores_cm.Rmd",
  output_dir = "C:\\Users\\Everton\\OneDrive\\Prefeitura\\2022\\Indicadores\\CM\\",
  # output_dir = getwd(),
  output_file = foutput,
  output_format = "pdf_document",
  pdf_document(
    toc = TRUE,
    number_sections = TRUE,
    fig_caption = TRUE,
    latex_engine = "pdflatex"
  )
)
