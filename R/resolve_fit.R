# resolve_fit.R
# ::rtemis.dplot3::
# 2026- EDG rtemis.org

# %% resolve_fit_hyperparameters ----
#' Resolve a `fit` name and optional hyperparameters
#'
#' `draw_scatter()` and `draw_3Dscatter()` name a supervised learning algorithm
#' as a string. The name matches one of `rtemis::available_supervised()`,
#' ignoring case, and the algorithm's `rtemis` setup function supplies its
#' default hyperparameters.
#'
#' @param fit Character: Algorithm name.
#' @param fit_params Optional `Hyperparameters` object: Hyperparameters for
#' `fit`.
#'
#' @return `Hyperparameters` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_fit_hyperparameters <- function(fit, fit_params) {
  algorithms <- names(rtemis::available_supervised(verbosity = 0L))
  algorithm <- algorithms[tolower(algorithms) == tolower(fit)]
  if (length(algorithm) != 1L) {
    rtemis.core::abort(
      "`fit` must name a supervised learning algorithm; '",
      fit,
      "' is not one. See rtemis::available_supervised().",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.null(fit_params)) {
    return(getExportedValue("rtemis", paste0("setup_", algorithm))())
  }
  if (!inherits(fit_params, "rtemis::Hyperparameters")) {
    rtemis.core::abort(
      "`fit_params` must be a `Hyperparameters` object, created by the ",
      "algorithm's setup function, e.g. rtemis::setup_",
      algorithm,
      "().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (tolower(fit) != tolower(fit_params@algorithm)) {
    rtemis.core::abort(
      "`fit` is '",
      fit,
      "', but `fit_params` defines hyperparameters for ",
      fit_params@algorithm,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  fit_params
} # /rtemis.dplot3::resolve_fit_hyperparameters
