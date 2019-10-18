GraficoAnoDeEvasao <- function(anosEvadidosdf) {
  # ==============================================================================
  # GRÁFICO DE COLUNAS - QUANTIDADE POR ANO
  # ==============================================================================
  plot_ly(anosEvadidosdf,
    x = ~AnoEvasao, y = ~Quantidade, type = "bar",
    hoverinfo = "text",
    text = ~ paste(Quantidade, "evadido(s)"),
    marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2))
  ) %>%
    layout(
      yaxis = list(title = "Quantidade de evadidos"),
      xaxis = list(title = "Anos", autotick = F, tickmode = "linear")
    )
}
