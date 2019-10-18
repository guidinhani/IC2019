GraficoTrabalhaAreaEvasao <- function(trabalhaAreaEvasaodf) {
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EVADIDOS QUE TRABALHAM NA ÁREA DE EVASÃO
  # ==============================================================================
  quantidadeTrabalhaNaArea <- trabalhaAreaEvasaodf %>%
    select(Trabalha) %>%
    group_by(Trabalha) %>%
    summarise(Quantidade = n())

  X <- c("TRABALHA NA ÁREA DO CURSO DE EVADIDO?")
  SIM <- quantidadeTrabalhaNaArea[, 2][2, ]
  NAO <- quantidadeTrabalhaNaArea[, 2][1, ]
  quantidadeTrabalhaNaArea <- data.frame(X, SIM, NAO)
  names(quantidadeTrabalhaNaArea) <- c("X", "SIM", "NAO")

  # ==============================================================================
  # GRÁFICO DE BARRAS EMPILHADO
  # ==============================================================================
  plot_ly(quantidadeTrabalhaNaArea,
    x = ~X, source = "sourceTrabalhaNaArea"
  ) %>%
    add_trace(
      type = "bar",
      y = ~NAO, name = "NAO", hoverinfo = "text", text = ~paste(NAO, "evadido(s)"),
      textfont = list(color = '#FFFFFF', size = 14),
      textposition = "inside",
      marker = list(
        color = c("rgb(163, 21, 16)"),
        line = list(color = c("rgb(99, 9, 9)"), width = 2)
      ),
      width = .5
    ) %>%
    add_trace(
      type = "bar",
      y = ~SIM, name = "SIM", hoverinfo = "text", text = ~paste(SIM, "evadido(s)"),
      textfont = list(color = '#FFFFFF', size = 14),
      textposition = "inside",
      marker = list(
        color = c("rgb(35, 101, 131)"), line = list(color = c("rgb(13, 60, 81)"), width = 2)
      ),
      width = .5
    ) %>%
    layout(yaxis = list(title = "Quantidade de evadidos"), xaxis = list(title = ""), showlegend = T, barmode = "stack")
}
