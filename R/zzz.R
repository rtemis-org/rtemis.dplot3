# zzz.R
# ::rtemis.dplot3::
# 2026- EDG rtemis.org

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  if (is.null(getOption("rtemis_theme"))) {
    options(rtemis_theme = "whitegrid")
  }
  if (is.null(getOption("rtemis_palette"))) {
    options(rtemis_palette = "rtms")
  }
  if (is.null(getOption("rtemis_font"))) {
    options(rtemis_font = "Helvetica")
  }
}
