# Funções gerais

options(knitr.kable.NA = '')

app.table.default <- function(data, align = NULL, caption = NULL, format.args = list(big.mark = ".", decimal.mark =",", nsmall = 2), col.names = NULL) {
  tbl <- knitr::kable(data, align=align, caption = caption, format.args = format.args, col.names = col.names, format = "latex")
  tbl <- kable_classic(tbl)
  tbl <- kable_styling(tbl, position = "center", latex_options = c("HOLD_position", "repeat_header"), latex_table_env = "tabular", table.envir = "table*")
  return(tbl)
}

app.plot.theming <- function(g) {
  return(
    g +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent")
      ) +
      xlab(NULL) +
      ylab(NULL)
  )
}

app.color.primary <- "#000000"
app.color.secondary <- "#696969"
app.color.ternary <- "#C0C0C0"

app.table.save <- function(tbl, name) {
  writeLines(tbl, paste("cache/", name, ".tex", sep = ""))
}

app.plot.save <- function(name, plot) {
  ggsave(paste("cache/", name, ".png", sep = ""), plot = plot, height = 10, scale = 0.75, units = "cm", dpi = "print")
}