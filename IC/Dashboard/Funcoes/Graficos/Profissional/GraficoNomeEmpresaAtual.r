GraficoNomeEmpresaAtual <- function(nomeEmpresaAtual) {
  # ==============================================================================
  # EVENTO DE CLICK
  # ==============================================================================
  eventdata <- event_data("plotly_click", source = "sourceTipoEmpresa")
  # ==============================================================================
  # VALIDAÇAO DO EVENTO DE CLICK
  # ==============================================================================
  validate(need(!is.null(eventdata), "Clique no gráfico de colunas acima para saber mais informações"))
  # ==============================================================================
  # DEFINIÇÃO DE CORES
  # ==============================================================================
  if (eventdata$x == "PRIVADA") {
    colors <- "rgb(163, 21, 16)"
    fillColors <- "rgb(99, 9, 9)"
  }
  if (eventdata$x == "PUBLICA") {
    colors <- "rgb(61, 124, 54)"
    fillColors <- "rgb(32, 71, 28)"
  }
  # ==============================================================================
  # CONSULTA - TIPO DA EMPRESA ATUAL DE TRABALHO DOS EGRESSOS DE ACORDO COM A BARRA SELECIONADA
  # ==============================================================================
  if (identical(eventdata$x, "PRIVADA")) {
    empresaAtual <- nomeEmpresaAtual %>%
      group_by(Empresa) %>%
      filter(Tipo %in% eventdata$x) %>%
      summarise(Quantidade = n()) %>%
      filter(Quantidade >= 2)

    margin <- 100
  }
  if (identical(eventdata$x, "PUBLICA")) {
    empresaAtual <- nomeEmpresaAtual %>%
      group_by(Empresa) %>%
      filter(Tipo %in% eventdata$x) %>%
      summarise(Quantidade = n())
    margin <- 250
  }
  # ==============================================================================
  # VALIDAÇAO DO EVENTO DE CLICK
  # ==============================================================================
  validate(need(!nrow(empresaAtual) == 0, "Clique no gráfico de colunas acima para saber mais informações"))
  # ==============================================================================
  # GRÁFICO DE COLUNAS
  # ==============================================================================
  plot_ly(empresaAtual, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Empresa, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text", text = ~ paste(Quantidade, "evadido(s)"),
      marker = list(color = colors, line = list(color = fillColors, width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de evadidos"), margin = list(l = margin)
    )
}
