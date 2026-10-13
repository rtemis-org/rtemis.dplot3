# rtemis.dplot3-package.R
# ::rtemis.dplot3::
# 2026- EDG rtemis.org

#' \pkg{rtemis.dplot3}: Interactive 'plotly' Graphics for the 'rtemis' Ecosystem
#'
#' @description
#' The `draw_*()` functions draw interactive 'plotly', 'networkD3', 'threejs' and 'leaflet'
#' graphics. `available_draw()` lists them. 'rtemis.draw' provides their successors.
#'
#' @section Options:
#' Options set in `.Rprofile` define defaults for every `draw_*()` call:
#' \describe{
#'     \item{rtemis_theme}{Theme name, e.g. "whiteigrid" or "darkgraygrid". See
#'     `available_themes()`.}
#'     \item{rtemis_font}{Font family.}
#'     \item{rtemis_palette}{Palette name. See `get_palette()`.}
#' }
#'
#' @section Themes:
#' A `Theme` object, created by the `theme_*()` functions or `choose_theme()`, sets the colors,
#' fonts and grid of a plot.
#'
#' @keywords internal
#' @import S7 data.table graphics rtemis.core stats
#' @rawNamespace import(grDevices, except = gray)
#' @importFrom rtemis train se cluster mgetnames
#' @importFrom utils head tail
"_PACKAGE"
