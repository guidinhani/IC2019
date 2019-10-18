GraficoCursosConcluidos <- function(cursosConcluidos) {
  # ==============================================================================
  # GRÁFICO DE PIZZA
  # ==============================================================================
  plot_ly(cursosConcluidos,
    labels = ~Curso, values = ~Quantidade, type = "pie", sort = FALSE,
    textposition = "inside",
    textinfo = "label+value",
    textfont = list(color = "#FFFFFF", size = 14),
    hoverinfo = "text",
    text = ~ paste(Curso, "-", Quantidade, "evadido(s)"),
    marker = list(
      colors = c(
        "rgb(42, 135, 246)", "rgb(52, 135, 237)", "rgb(61, 136, 227)", "rgb(70, 137, 218)",
        "rgb(79, 138, 209)", "rgb(89, 139, 200)", "rgb(128, 128, 128)"
      ),
      line = list(color = "#FFFFFF", width = 2)
    )
  ) %>%
    layout(showlegend = FALSE)
}
