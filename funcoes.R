# Funções gerais

options(knitr.kable.NA = '')

app.table.default <- function(data, align = NULL, caption = NULL, format.args = list(big.mark = ".", decimal.mark =",", nsmall = 2), col.names = NULL) {
  tbl <- knitr::kable(data, align=align, caption = caption, format.args = format.args, col.names = col.names, format = "latex")
  tbl <- kable_classic(tbl)
  tbl <- kable_styling(tbl, position = "center", latex_options = c("HOLD_position", "repeat_header"))
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
        plot.background = element_rect(fill = "transparent"),
        aspect.ratio = 9/16
      ) +
      xlab(NULL) +
      ylab(NULL)
  )
}

app.color.primary <- "black"
app.color.secondary <- "grey"