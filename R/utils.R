# utils.R
# ::rtemis.dplot3::
# 2016- EDG rtemis.org

# %% recycle ----
#' Recycle values to the length of a target
#'
#' @param x Vector: Values to recycle.
#' @param target Vector: Target whose length the output takes.
#'
#' @return Vector of length `length(target)`.
#'
#' @keywords internal
#' @noRd
recycle <- function(x, target) {
  lenx <- length(x)
  lent <- length(target)
  rep(x, ceiling(lent / lenx))[seq_len(lent)]
} # /rtemis.dplot3::recycle


# %% drange ----
#' Scale values linearly to a range
#'
#' @param x Numeric vector or matrix.
#' @param lo Numeric: Lowest output value.
#' @param hi Numeric: Highest output value.
#' @param byCol Logical: If TRUE, scale each column of a matrix separately.
#'
#' @return Numeric vector or matrix.
#'
#' @keywords internal
#' @noRd
drange <- function(x, lo = 0, hi = 1, byCol = TRUE) {
  dr <- function(x, lo, hi) {
    .min <- min(x, na.rm = TRUE)
    (x - .min) / max(x - .min, na.rm = TRUE) * (hi - lo) + lo
  }

  if (NCOL(x) > 1) {
    if (byCol) {
      apply(x, 2, function(x) dr(x, lo, hi))
    } else {
      dr(x, lo, hi)
    }
  } else {
    dr(x, lo, hi)
  }
} # /rtemis.dplot3::drange


# %% pval_stars ----
#' Significance stars for p-values
#'
#' @param x Numeric: p-values.
#'
#' @return Factor: "***", "**", "*" or "".
#'
#' @keywords internal
#' @noRd
pval_stars <- function(x) {
  cut(x, breaks = c(0, .001, .01, .05, 1), labels = c("***", "**", "*", ""))
} # /rtemis.dplot3::pval_stars


# %% iflengthy ----
#' Return a value with nonzero length, or NULL
#'
#' @param x Any object.
#'
#' @return `x`, or NULL when `x` has length 0.
#'
#' @keywords internal
#' @noRd
iflengthy <- function(x) {
  if (length(x) > 0) x else NULL
} # /rtemis.dplot3::iflengthy


# %% date2factor ----
#' Bin dates into a factor
#'
#' @param x Date vector.
#' @param time_bin Character \{"year", "quarter", "month", "day"\}: Bin width.
#' @param make_bins Character \{"range", "present"\}: "range" makes a level for
#' every bin in `bin_range`; "present" makes a level for every bin in `x`.
#' @param bin_range Date vector of length 2: First and last date to bin.
#' @param ordered Logical: If TRUE, return an ordered factor.
#'
#' @return Factor.
#'
#' @keywords internal
#' @noRd
date2factor <- function(
  x,
  time_bin = c("year", "quarter", "month", "day"),
  make_bins = c("range", "present"),
  bin_range = range(x, na.rm = TRUE),
  ordered = FALSE
) {
  time_bin <- match.arg(time_bin)
  make_bins <- match.arg(make_bins)

  if (time_bin == "year") {
    if (make_bins == "present") {
      factor(data.table::year(x), ordered = ordered)
    } else {
      out <- as.character(data.table::year(x))
      factor(
        out,
        levels = as.character(seq(
          data.table::year(bin_range[1]),
          data.table::year(bin_range[2])
        )),
        ordered = ordered
      )
    }
  } else if (time_bin == "quarter") {
    if (make_bins == "present") {
      factor(
        paste0(data.table::year(x), " Q", data.table::quarter(x)),
        ordered = ordered
      )
    } else {
      factor(
        paste0(data.table::year(x), " Q", data.table::quarter(x)),
        levels = levels(date2yq(seq(
          bin_range[1],
          bin_range[2],
          by = "quarter"
        ))),
        ordered = ordered
      )
    }
  } else if (time_bin == "month") {
    ym <- paste(substr(months(x), 1, 3), data.table::year(x))
    if (make_bins == "present") {
      .levels <- unique(ym[order(x)])
      factor(ym, levels = .levels, ordered = ordered)
    } else {
      factor(
        ym,
        levels = levels(date2ym(seq(bin_range[1], bin_range[2], by = "month"))),
        ordered = ordered
      )
    }
  } else if (time_bin == "day") {
    if (make_bins == "present") {
      factor(x, levels = as.character(unique(x[order(x)])), ordered = ordered)
    } else {
      factor(
        x,
        levels = as.character(seq(bin_range[1], bin_range[2], by = 1)),
        ordered = ordered
      )
    }
  }
} # /rtemis.dplot3::date2factor


# %% date2yq ----
#' Year and quarter of dates
#'
#' @param x Date vector.
#' @param ordered Logical: If TRUE, return an ordered factor.
#'
#' @return Factor.
#'
#' @keywords internal
#' @noRd
date2yq <- function(x, ordered = FALSE) {
  factor(
    paste0(data.table::year(x), " Q", data.table::quarter(x)),
    ordered = ordered
  )
} # /rtemis.dplot3::date2yq


# %% date2ym ----
#' Year and month of dates
#'
#' @param x Date vector.
#' @param ordered Logical: If TRUE, return an ordered factor.
#'
#' @return Factor.
#'
#' @keywords internal
#' @noRd
date2ym <- function(x, ordered = FALSE) {
  ym <- paste(substr(months(x), 1, 3), data.table::year(x))
  .levels <- unique(ym[order(x)])
  factor(ym, levels = .levels, ordered = ordered)
} # /rtemis.dplot3::date2ym


# %% list2html ----
#' Format a named list as HTML lines
#'
#' @param x Named list.
#' @param sep Character: Separator between name and value.
#' @param col Character: Value color.
#' @param key_weight Numeric: Font weight of names.
#' @param value_weight Numeric: Font weight of values.
#' @param line Character: Line terminator.
#'
#' @return Character: HTML.
#'
#' @keywords internal
#' @noRd
list2html <- function(
  x,
  sep = ": ",
  col = "#16A0AC",
  key_weight = 100,
  value_weight = 300,
  line = "<br>"
) {
  .names <- names(x)
  sapply(seq_along(x), \(i) {
    paste0(
      html_span(.names[i], style = paste0("font-weight:", key_weight, ";")),
      sep,
      html_span(
        x[[i]],
        style = paste0("color:", col, "; font-weight:", value_weight, ";")
      ),
      line
    )
  }) |>
    paste0(collapse = "") |>
    html_raw()
} # /rtemis.dplot3::list2html


# %% do_call ----
#' Call a function with suggestions for common errors and warnings
#'
#' @param fn Function or Character: Function to call.
#' @param args List: Arguments to `fn`.
#' @param error_pattern_suggestion Optional Named list: Error message patterns
#' and the suggestion to print for each.
#' @param warning_pattern_suggestion Optional Named list: Warning message
#' patterns and the suggestion to print for each.
#' @param verbosity Integer: Verbosity level.
#'
#' @return The value of `fn`.
#'
#' @keywords internal
#' @noRd
do_call <- function(
  fn,
  args,
  error_pattern_suggestion = NULL,
  warning_pattern_suggestion = NULL,
  verbosity = 1L
) {
  # Callers pass the function itself (`do_call(Rtsne::Rtsne, args)`), which has
  # no character form, so the name for the error message comes from the
  # unevaluated argument. A caller may pass a string instead; both label.
  fn_label <- if (is.character(fn)) fn else deparse1(substitute(fn))
  common_errors <- list(
    "object '(.*)' not found" = "Check that the object exists and is spelled correctly.",
    "object of type 'closure' is not subsettable" = "Check that the object is a list or data.frame."
  )
  common_warnings <- list(
    "NAs introduced by coercion" = "Check that the input is of the correct type.",
    "glm.fit: fitted probabilities numerically 0 or 1 occurred" = paste(
      bold("Reasons for this warning include:"),
      "1) Perfect Separation of classes.",
      "2) Highly Imbalanced data.",
      "3) Extreme values in predictors.",
      "4) Too many predictors for the number of observations.",
      "5) Multicollinearity.",
      bold("Suggestion:"),
      "Try using GLMNET or tree-based algorithms",
      sep = "\n  "
    )
  )
  err_pat_sug <- c(common_errors, error_pattern_suggestion)
  warn_pat_sug <- c(common_warnings, warning_pattern_suggestion)
  # A backend called over a loop repeats one warning per iteration. The
  # condition repeats with it, which is the caller's to handle; the advice is
  # the same every time, so it is given once per `do_call()`.
  advised <- character()
  tryCatch(
    {
      withCallingHandlers(
        {
          do.call(fn, args)
        },
        warning = function(w) {
          # No `invokeRestart("muffleWarning")`: the warning continues to the
          # caller as the condition the backend raised. Removing this line
          # would silently make every backend warning uncatchable again.
          if (verbosity < 1L) {
            return(invisible(NULL))
          }
          fnwarn <- conditionMessage(w)
          idi <- which(vapply(
            names(warn_pat_sug),
            function(pattern) grepl(pattern, fnwarn),
            logical(1L)
          ))
          if (length(idi) == 0L || fnwarn %in% advised) {
            return(invisible(NULL))
          }
          advised <<- c(advised, fnwarn)
          # The warning itself is deferred by R until the top-level call
          # returns, so the advice quotes the warning it refers to.
          info(
            highlight(fn_label),
            " warned: ",
            fnwarn,
            "\n  ",
            paste0(
              vapply(idi, function(i) warn_pat_sug[[i]], character(1L)),
              collapse = "\n  "
            ),
            verbosity = verbosity
          )
          invisible(NULL)
        } # /warning
      ) # /withCallingHandlers
    },
    error = function(e) {
      fnerr <- e[["message"]]
      errmsg <- paste0(
        highlight(fn_label),
        " failed with error:\n",
        fnerr,
        "\n"
      )
      idi <- which(vapply(
        names(err_pat_sug),
        function(pattern) grepl(pattern, fnerr),
        logical(1L)
      ))
      if (length(idi) > 0L) {
        suggestions <- vapply(idi, function(i) err_pat_sug[[i]], character(1L))
        errmsg <- paste0(
          red(errmsg),
          fmt(
            paste0(
              bold("\nSuggestion:\n  "),
              paste0(suggestions, collapse = "\n  ")
            ),
            col = rtemis_colors[["orange"]]
          )
        )
      }
      if (verbosity > 0L) {
        cat("\n")
      }
      rtemis.core::abort(
        errmsg,
        class = "rtemis_runtime_error",
        parent = e,
        verbosity = verbosity
      )
    } # /error
  ) # /tryCatch
} # /rtemis.dplot3::do_call


# %% brier_score ----
#' Brier score
#'
#' @param true_int Integer: True labels, 1 for the positive class, 0 otherwise.
#' @param predicted_prob Numeric \[0, 1\]: Predicted probabilities of the
#' positive class.
#'
#' @return Numeric: Brier score.
#'
#' @keywords internal
#' @noRd
brier_score <- function(true_int, predicted_prob) {
  true_int <- clean_int(true_int)
  check_float01inc(predicted_prob)
  mean((true_int - predicted_prob)^2)
} # /rtemis.dplot3::brier_score


# %% labels2int ----
#' Binary factor to integer
#'
#' @param x Factor: Binary outcome.
#' @param binclasspos Integer \{1, 2\}: Position of the positive class among
#' the levels.
#'
#' @return Integer: 1 for the positive class, 0 otherwise.
#'
#' @keywords internal
#' @noRd
labels2int <- function(x, binclasspos = 2L) {
  stopifnot(is.factor(x))
  # Convert factor to 0, 1 where 1 is the positive class as defined by binclasspos
  if (binclasspos == 1L) {
    xi <- 2L - as.integer(x)
  } else {
    xi <- as.integer(x) - 1L
  }
  xi
} # /rtemis.dplot3::labels2int


# %% check_is_S7 ----
#' Check that a value is an instance of a given S7 class
#'
#' @param x Object to check.
#' @param cls S7 class object to check against.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if the check fails.
#'
#' @keywords internal
#' @noRd
check_is_S7 <- function(
  x,
  cls,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }
  if (is.null(x)) {
    rtemis.core::abort(
      "`",
      arg_name,
      "` cannot be NULL.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (!S7_inherits(x, cls)) {
    rtemis.core::abort(
      "`",
      arg_name,
      "` must be a `",
      cls@name,
      "` object.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  invisible()
} # /rtemis.dplot3::check_is_S7
