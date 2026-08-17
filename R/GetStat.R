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
#' s$get_obs()
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
        message("📊 a vector of length ", length(self$x))
      } else {
        message("📦 a data frame with ",
                ncol(self$x), " columns and ", nrow(self$x), " rows")
      }
    },

    #' @description Observation and missing value statistics
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    obs = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_obs(self$x)
      } else {
        if (is.null(col)) col <- colnames(self$x)

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
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    range = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_range(self$x, ...)
      } else {
        if (is.null(col)) col <- colnames(self$x)

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
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    central = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_central(self$x, ...)
      } else {
        if (is.null(col)) col <- colnames(self$x)

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
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    var_sd = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_var_sd(self$x, ...)
      } else {
        if (is.null(col)) col <- colnames(self$x)

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
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    iqr = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        get_iqr(self$x, ...)
      } else {
        if (is.null(col)) col <- colnames(self$x)

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
            Q1 = stats::quantile(Value, 0.25, ...),
            Median = stats::quantile(Value, 0.5, ...),
            Q3 = stats::quantile(Value, 0.75, ...),
            IQR = stats::IQR(Value, ...),
            `Lower outer fence` = Q1 - 3 * IQR,
            `Lower inner fence` = Q1 - 1.5 * IQR,
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
    #' @return A data frame with Metric and Value columns.
    #' @param col column names
    #' @param ... Additional arguments passed to read functions
    skewness = function(col = NULL, ...) {
      if (is.vector(self$x)) {
        data.frame(
          Statistic = "Skewness",
          Value = get_skewness(self$x, ...)
        )
      } else {
        if (is.null(col)) col <- colnames(self$x)

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
              n <- dplyr::n()
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

    #' @description List tables or files
    #' @param ... Additional arguments passed to read functions
    #' @return character vector
    list = function(...) {
      names(self$x)
    }

  )
)
