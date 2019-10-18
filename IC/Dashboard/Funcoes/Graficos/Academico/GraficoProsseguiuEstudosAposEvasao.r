GraficoProsseguiuEstudosAposEvasao <- function(quantidadeCursosAposEvasaodf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS EMPILHADO
  # ==============================================================================
  plot_ly(quantidadeCursosAposEvasaodf, x = ~TITULO, source = "source") %>%
    add_trace(
      type = "bar",
      y = ~NAO, name = "NÃO",
      textposition = "inside",
      hoverinfo = "text",
      textfont = list(color = "#FFFFFF", size = 14),
      text = ~ paste(NAO, "evadido(s)"),
      marker = list(color = c("rgb(163, 21, 16)"), line = list(color = c("rgb(99, 9, 9)"), width = 2)),
      width = .4
    ) %>%
    add_trace(
      type = "bar",
      y = ~SIM, name = "SIM",
      textposition = "inside",
      hoverinfo = "text",
      textfont = list(color = "#FFFFFF", size = 14),
      text = ~ paste(SIM, "evadido(s)"),
      marker = list(color = c("rgb(35, 101, 131)"), line = list(color = c("rgb(13, 60, 81)"), width = 2)),
      width = .4
    ) %>%
    layout(
      yaxis = list(title = "Quantidade de evadidos"), xaxis = list(title = ""), showlegend = T, barmode = "stack"
    )
}
