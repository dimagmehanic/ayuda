#' Statistical analysis class
#'
#' @description Compute descriptive statistics, observation counts,
#'   missing values, and frequency tables for a vector.
#'
#' @field x Input vector.
#'
#' @examples
#' # Numeric vector
#' s <- GetStat$new(c(1, 2, 3, NA, 5, 5, 6))
#' s$obs()
#'
#'
#' @export
GetStat <- R6::R6Class("GetStat", # nolint
  public = list(
    x = NULL,

    #' @description Initialize a new GetStat object
    #' @param x A vector.
    initialize = function(x) {
      self$x <- x
      if (is.vector(self$x)) {
        message("📊 a vector of length ", sum(!is.na(self$x)))
      } else {
        message("📦 a data frame with ",
                ncol(self$x), " columns and ", nrow(self$x), " rows")
      }
    },

    #' @description Observation and missing value statistics
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    obs = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_obs(self$x)
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Total = dplyr::n(),
            Missing = sum(is.na(Value)),
            `Non-missing` = sum(!is.na(Value)),
            `Missing (%)` = round(mean(is.na(Value)) * 100, 2),
            `Non-missing (%)` = round(mean(!is.na(Value)) * 100, 2)
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Range statistics
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    range = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_range(self$x, ...)
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Min = min(Value, ...),
            Max = max(Value, ...),
            Range = max(Value, ...) - min(Value, ...)
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Central tendency statistics
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    central = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_central(self$x, ...)
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Mean = mean(Value, ...),
            Median = stats::median(Value, ...),
            Mode = get_mode(Value, ...)
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Variance and standard deviation statistics
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    var_sd = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_var_sd(self$x, ...)
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Variance = var(Value, ...),
            `Standard deviation` = sd(Value, ...)
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description IQR and outlier fence statistics
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    iqr = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_iqr(self$x, ...)
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            `Lower outer fence` = stats::quantile(Value, 0.25, ...) - 3 * stats::IQR(Value, ...),
            `Lower inner fence` = stats::quantile(Value, 0.25, ...) - 1.5 * stats::IQR(Value, ...),
            Q1 = stats::quantile(Value, 0.25, ...),
            Median = stats::quantile(Value, 0.5, ...),
            Q3 = stats::quantile(Value, 0.75, ...),
            IQR = stats::IQR(Value, ...),
            `Upper inner fence` = Q3 + 1.5 * IQR,
            `Upper outer fence` = Q3 + 3 * IQR
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Skewness statistic
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    skewness = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        data.frame(
          Statistic = "Skewness",
          Value = get_skewness(self$x, ...)
        )
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Skewness = {
              n <- sum(!is.na(Value))
              m <- mean(Value, ...)
              s <- sd(Value, ...)
              sum(((Value - m)^3) / (s^3)) / n
            }
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Kurtosis statistic
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    kurtosis = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        data.frame(
          Statistic = "Kurtosis",
          Value = get_kurtosis(self$x, ...)
        )
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Kurtosis = {
              n <- sum(!is.na(Value))
              m <- mean(Value, ...)
              s <- sd(Value, ...)
              sum(((Value - m)^4) / (s^4)) / n
            }
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Coefficient of variation statistic
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    cv = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        data.frame(
          Statistic = "CV (%)",
          Value = get_cv(self$x, ...)
        )
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Value"
          ) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            `CV (%)` = 100 * sd(Value, ...) / mean(Value, ...)
          ) %>%
          tidyr::pivot_longer(
            cols = -Variable,
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          tidyr::pivot_wider(
            names_from = Variable,
            values_from = Value
          )
      }
    },

    #' @description Z-score statistic
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    zscore = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        data.frame(
          Value = self$x,
          `Z-score` = get_zscore(self$x, ...)
        )
      } else {
        if (is.null(col)) {
          if (inherits(self$x, "tbl_lazy")) {
            col <- self$x %>% dplyr::select(dplyr::where(is.numeric)) %>% names()
          } else {
            col <- colnames(self$x)[sapply(self$x, is.numeric)]
          }
        }

        if (inherits(self$x, "tbl_lazy")) {
          df <- self$x %>%
            dplyr::select(dplyr::all_of(col)) %>%
            dplyr::collect()
        } else {
          df <- self$x %>% dplyr::select(dplyr::all_of(col))
        }

        df %>%
          dplyr::summarise(dplyr::across(
            dplyr::everything(),
            ~ scale(.x, ...),
            .names = "{.col}_zscore"
          ))
      }
    },

    #' @description List tables or files
    #' @param ... Additional arguments passed to read functions
    #' @return character vector
    list = function(...) {
      names(self$x)
    }

  )
)
