# plotops.R
# ::rtemis.dplot3::
# 2020- EDG rtemis.org

#' @keywords internal
#' @noRd
getlim <- function(x, axs = c("r", "i"), axs.r.pct = .04) {
  axs <- match.arg(axs)

  .x <- na.exclude(x)
  .min <- min(.x)
  .max <- max(.x)

  if (axs == "r") {
    .diff <- .max - .min
    c(.min - axs.r.pct * .diff, .max + axs.r.pct * .diff)
  } else {
    c(.min, .max)
  }
} # /rtemis.dplot3::getlim
