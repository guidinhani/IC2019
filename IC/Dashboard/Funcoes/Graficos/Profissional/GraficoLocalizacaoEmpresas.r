GraficoLocalizacaoEmpresas <- function(localizacaoEmpresasdf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS
  # ==============================================================================
  plot_ly(localizacaoEmpresasdf, x = ~Quantidade) %>%
    add_trace(
      y = ~reorder(Cidade, -Quantidade), type = "bar",
      hoverinfo = "text", text = ~paste(Quantidade, "evadido(s)"),
      marker = list( color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de evadidos", autotick = F), margin = list(l = 120)
    )
}
