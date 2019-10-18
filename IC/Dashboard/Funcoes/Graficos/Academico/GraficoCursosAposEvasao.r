GraficoCursosAposEvasao <- function(cursosProsseguidosdf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS
  # ==============================================================================
  plot_ly(cursosProsseguidosdf, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Curso, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text", text = ~ paste(Quantidade, "evadido(s)"),
      textfont = list(color = "#FFFFFF", size = 14),
      marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2)),
      width = 0.5
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de evadidos", autotick = F, range = c(0, 12))
    )
}
