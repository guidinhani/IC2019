GraficoAreasCursosAposEvasao <- function(areasCursosProsseguidosdf) {
  # ==============================================================================
  # GRÁFICO DE COLUNAS
  # ==============================================================================
  plot_ly(areasCursosProsseguidosdf, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Area, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text", text = ~ paste(Quantidade, "curso(s)"),
      textfont = list(color = "#FFFFFF", size = 14),
      marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de cursos por área", autotick = F),
      margin = list(l = 170)
    )
}
