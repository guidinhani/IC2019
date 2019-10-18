GraficoTipoEmpresa <- function(tipoEmpresadf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS
  # ==============================================================================
  plot_ly(tipoEmpresadf, x = ~Tipo, source = "sourceTipoEmpresa") %>%
    add_trace(
      y = ~Quantidade, type = "bar",
      textposition = "inside",
      hoverinfo = "text",
      textfont = list(color = "#FFFFFF", size = 14),
      text = ~ paste(Quantidade, "evadido(s)"),
      marker = list(
        color = c("rgb(163, 21, 16)", "rgb(61, 124, 54)"),
        line = list(color = c("rgb(99, 9, 9)", "rgb(32, 71, 28)"), width = 2)
      ),
      width = .5
    ) %>%
    layout(yaxis = list(title = "Quantidade de evadidos"), xaxis = list(title = ""))
}
