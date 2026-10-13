# available_draw.R
# ::rtemis.dplot3::
# 2026- EDG rtemis.org

# %% draw_fns ----
draw_fns <- c(
  draw_3Dscatter = "3D Scatter Plot",
  draw_bar = "Bar Plot",
  draw_box = "Box Plot",
  draw_calibration = "Calibration Plot",
  draw_confusion = "Confusion Matrix",
  draw_dist = "Density and Histogram Plots",
  draw_fit = "Scatter Plot with Fit Line",
  draw_graphD3 = "Network Graph using networkD3",
  draw_graphjs = "Network Graph using threejs",
  draw_heatmap = "Heatmap using heatmaply",
  draw_leaflet = "Choropleth Map using leaflet",
  draw_pie = "Pie Chart",
  draw_protein = "Amino Acid Annotation Plot",
  draw_pvals = "P-value Bar Plot",
  draw_roc = "ROC Curve",
  draw_scatter = "Scatter Plot",
  draw_spectrogram = "Spectrogram",
  draw_survfit = "Survival Curves",
  draw_table = "Table using plotly",
  draw_ts = "Time Series Plot",
  draw_varimp = "Barplot for Variable Importance",
  draw_volcano = "Volcano Plot",
  draw_xt = "Time Series Line Plot"
)


# %% available_draw ----
#' Available Draw Functions
#'
#' Print available draw functions for visualization.
#'
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list of draw function descriptions, invisibly.
#'
#' @author EDG
#' @export
#' @examples
#' available_draw()
available_draw <- function(verbosity = 1L) {
  fns <- as.list(draw_fns)
  if (verbosity > 0L) {
    cat("Available draw functions:\n")
    printls(fns, print_class = FALSE, limit = -1L)
  }
  invisible(fns)
} # /rtemis.dplot3::available_draw
