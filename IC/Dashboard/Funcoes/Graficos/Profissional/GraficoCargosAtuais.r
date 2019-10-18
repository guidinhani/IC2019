GraficoCargosAtuais <- function(cargosAtuaisdf) {
  # ==============================================================================
  # GRÁFICO DE COLUNAS
  # ==============================================================================
  plot_ly(cargosAtuaisdf, x = ~Quantidade) %>%
    add_trace(
      y = ~reorder(Cargo, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text",
      text = ~paste(Quantidade, "evadido(s)"),
      marker = list(
        color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2),
        width = .5
      )
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de evadidos", autotick = F), margin = list(l = 200)
    )
}
