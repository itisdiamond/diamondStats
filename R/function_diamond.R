#' Compute Mean and Standard Deviation
#'
#' Computes the mean and standard deviation of a numeric vector, ignoring NA values.
#'
#' @param x Numeric vector.
#'
#' @return A named numeric vector with elements "Mean" and "SD".
#'
#' @examples
#' sdmean_diamond(c(1, 2, 3, 4, NA))
#'
#' @export
sdmean_diamond <- function(x) {
  if (!is.numeric(x)) stop("Input vector must be numeric.")
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  return(c(Mean = round(mean_val, 2), SD = round(sd_val, 2)))
}


#' Barplot with customizable error bars
#'
#' @param formula A formula like outcome ~ group
#' @param data Data frame
#' @param error.bars Error bar type: "sd", "se", "conf.int"
#' @param conf.level Confidence level for "conf.int"
#' @param ylim y-axis limits
#' @param col Bar color
#' @param ... Further arguments to barplot
#' @return Invisibly returns bar midpoints
#' @export
barplot_error <- function(formula, data = NULL, error.bars = "sd", conf.level = 0.95, ylim = NULL, col = "lightblue", ...) {
  if (missing(data)) data <- environment(formula)
  mf <- model.frame(formula, data)
  response <- mf[[1]]
  group <- mf[[2]]
  groups <- split(response, group)
  mean_vals <- sapply(groups, mean, na.rm = TRUE)
  sd_vals <- sapply(groups, sd, na.rm = TRUE)
  n <- sapply(groups, function(x) sum(!is.na(x)))
  labels <- names(mean_vals)
  error_bar <- switch(
    error.bars,
    "sd" = sd_vals,
    "se" = sd_vals / sqrt(n),
    "conf.int" = {
      se <- sd_vals / sqrt(n)
      tval <- qt(1 - (1 - conf.level) / 2, df = n - 1)
      se * tval
    },
    stop('Unknown error bar type: use "sd", "se", "conf.int"')
  )
  upper <- mean_vals + error_bar
  lower <- mean_vals - error_bar
  if (is.null(ylim))
    ylim <- c(0, max(upper, na.rm = TRUE) * 1.1)
  bp <- barplot(mean_vals, names.arg = labels, ylim = ylim, col = col, border = "black", ...)
  segments(bp, lower, bp, upper, lwd = 2)
  segments(bp - 0.1, upper, bp + 0.1, upper, lwd = 2)
  segments(bp - 0.1, lower, bp + 0.1, lower, lwd = 2)
  invisible(bp)
}

#' Extended regression diagnostics
#'
#' @param input Formula or lm object
#' @param data Data frame
#' @param best_res Number of best (smallest abs z-residuals) to show
#' @param worst_res Number of worst (largest abs z-residuals) to show
#' @param plot Logical, show regression plot
#' @param order "desc" or "asc"
#' @return Data frame with regression diagnostics
#' @export
lm_summary <- function(input, data = NULL, best_res = NULL, worst_res = NULL, plot = FALSE, order = c("desc", "asc")) {
  order <- match.arg(order)
  if (inherits(input, "formula")) {
    model <- lm(input, data = data)
  } else if (inherits(input, "lm")) {
    model <- input
  } else {
    stop("Input must be a formula or an lm object")
  }
  mf <- model.frame(model)
  y <- mf[[1]]
  x <- mf[[2]]
  y_name <- names(mf)[1]
  x_name <- names(mf)[2]
  fitted_vals <- fitted(model)
  residuals_vals <- residuals(model)
  hat_vals <- hatvalues(model)
  cook_d <- cooks.distance(model)
  resid_z <- rstandard(model)
  n <- length(y)
  b0_leave1 <- numeric(n)
  b1_leave1 <- numeric(n)
  for (i in 1:n) {
    temp_model <- lm(y[-i] ~ x[-i])
    coefs <- coef(temp_model)
    b0_leave1[i] <- coefs[1]
    b1_leave1[i] <- coefs[2]
  }
  hat_thresh <- 2 * mean(hat_vals)
  cook_thresh <- 4 / n
  flags <- (abs(resid_z) > 2) + (hat_vals > hat_thresh) + (cook_d > cook_thresh)
  influential <- flags >= 2
  df <- data.frame(
    y = y,
    x = x,
    fitted = fitted_vals,
    residuals = residuals_vals,
    resid_z = resid_z,
    hat_value = hat_vals,
    cook_d = cook_d,
    b0_lo = round(b0_leave1, 5),
    b1_lo = round(b1_leave1, 5),
    influential = influential
  )
  names(df)[1:2] <- c(y_name, x_name)
  ord_z <- if (order == "desc") {
    order(abs(df$resid_z), decreasing = TRUE)
  } else {
    order(abs(df$resid_z), decreasing = FALSE)
  }
  if (!is.null(best_res) || !is.null(worst_res)) {
    best_rows <- if (!is.null(best_res)) tail(ord_z, best_res) else integer(0)
    worst_rows <- if (!is.null(worst_res)) head(ord_z, worst_res) else integer(0)
    selected <- sort(unique(c(best_rows, worst_rows)))
    df <- df[selected, ]
    df <- df[order(abs(df$resid_z), decreasing = (order == "desc")), ]
  } else {
    df <- df[ord_z, ]
  }
  if (plot) {
    col_vector <- rep("black", n)
    labels <- rep(NA, n)
    best_rows_all <- if (!is.null(best_res)) tail(ord_z, best_res) else integer(0)
    worst_rows_all <- if (!is.null(worst_res)) head(ord_z, worst_res) else integer(0)
    col_vector[best_rows_all] <- "blue"
    col_vector[worst_rows_all] <- "red"
    labels[best_rows_all] <- as.character(best_rows_all)
    labels[worst_rows_all] <- as.character(worst_rows_all)
    infl_idx <- which(influential)
    labels[infl_idx] <- paste0(labels[infl_idx], "*")
    plot(x, y, col = col_vector, pch = 19,
         main = paste("Regression:", y_name, "~", x_name),
         xlab = x_name, ylab = y_name)
    abline(model, col = "gray", lwd = 1.5)
    text(x, y, labels = labels, pos = 3, cex = 0.7, col = "gray40")
    legend("bottom", inset = -0.3, xpd = TRUE, legend = c("Worst residuals",
                                                          "Best residuals", "Other", "Influential (*)"), col = c("red",
                                                                                                                 "blue", "black", NA), pch = c(19, 19, 19, NA), horiz = TRUE,
           cex = 0.8, bty = "n", text.col = c("red", "blue", "black", "black"))
  }
  return(df)
}

#' Glass's delta for single sample (bootstrap CI)
#'
#' @param x Numeric vector
#' @param mu Reference mean
#' @param sd Population SD
#' @param R Bootstrap reps
#' @param conf.level Confidence level
#' @param seed Random seed
#' @return An object of class glassdelta
#' @export
glass.delta_1s <- function(x, mu, sd, R = 10000, conf.level = 0.95, seed = 123) {
  set.seed(seed)
  x <- na.omit(x)
  delta <- (mean(x) - mu) / sd
  boot <- replicate(R, {
    xb <- sample(x, replace = TRUE)
    (mean(xb) - mu) / sd
  })
  alpha <- 1 - conf.level
  ci <- quantile(boot, probs = c(alpha / 2, 1 - alpha / 2))
  result <- list(
    estimate = delta,
    conf.int = ci,
    conf.level = conf.level,
    mu = mu,
    sd = sd,
    method = "bootstrap percentile CI"
  )
  class(result) <- "glassdelta"
  return(result)
}

#' @export
print.glassdelta <- function(x, digits = 6, ...) {
  cat("Glass's delta (single sample)\n\n")
  cat("d estimate:", format(round(x$estimate, digits)), "\n")
  cat("Reference mu:", x$mu, "\n")
  cat("Population SD:", x$sd, "\n")
  cat(paste0(round(x$conf.level * 100), "% confidence interval (", x$method, "):\n"))
  cat("    lower     upper \n")
  cat(format(round(x$conf.int[1], digits)), " ", format(round(x$conf.int[2], digits)), "\n")
}



#' Draw Mean and Standard Deviation on a Plot
#'
#' Adds an arrow representing the mean and standard deviation on an existing plot.
#'
#' @param x Numeric vector.
#' @param at X-axis location to draw the line.
#' @param col Line color.
#' @param lwd Line width.
#' @param length Arrowhead length.
#'
#' @return No return value. Adds graphical elements to the current plot.
#'
#' @export
fit_mean_diamond <- function(x, at = 1, col = "black", lwd = 2, length = 0.1) {
  if (!is.numeric(x)) stop("Input must be numeric.")
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)

  arrows(x0 = at, y0 = m - s, x1 = at, y1 = m + s,
         angle = 90, code = 3, length = length, lwd = lwd, col = col)
}


#' Compare Classical, Trimmed and Winsorized Means
#'
#' Computes the mean and standard deviation using classical, trimmed, and winsorized methods.
#'
#' @param x Numeric vector.
#' @param trim Trimming proportion for trimmed mean (default = 0.2).
#' @param probs Quantile cutoffs for winsorization (default = c(0.05, 0.95)).
#' @param verbose Logical. If \code{TRUE}, prints the resulting data frame.
#'
#' @return A data frame with mean, standard deviation, sample size, and method type.
#'
#' @export
mean_diamond <- function(x, trim = 0.2, probs = c(0.05, 0.95), verbose = FALSE) {
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  if (trim < 0 || trim >= 0.5) stop("trim must be between 0 and 0.5.")
  if (length(probs) != 2 || any(probs < 0 | probs > 1) || probs[1] >= probs[2]) {
    stop("probs must be a valid numeric pair between 0 and 1.")
  }

  n_base <- length(x)
  mean_base <- mean(x)
  sd_base <- sd(x)

  x_trimmed <- sort(x)
  k_trim <- floor(trim * n_base)
  if (k_trim > 0) x_trimmed <- x_trimmed[(k_trim + 1):(n_base - k_trim)]
  n_trimmed <- length(x_trimmed)
  mean_trimmed <- mean(x_trimmed)
  sd_trimmed <- sd(x_trimmed)

  limits <- quantile(x, probs = probs, na.rm = TRUE)
  x_wins <- x
  x_wins[x < limits[1]] <- limits[1]
  x_wins[x > limits[2]] <- limits[2]
  n_wins <- length(x_wins)
  mean_wins <- mean(x_wins)
  sd_wins <- sd(x_wins)

  result <- data.frame(
    type = c("base", "trimmed", "winsorized"),
    n = c(n_base, n_trimmed, n_wins),
    mean = round(c(mean_base, mean_trimmed, mean_wins), 5),
    sd = round(c(sd_base, sd_trimmed, sd_wins), 5),
    trim = c(NA, trim, NA),
    probs = c(NA, NA, paste0("c(", probs[1], ", ", probs[2], ")"))
  )

  if (verbose) {
    print(result)
    return(invisible(result))
  } else {
    return(result)
  }
}


#' Skewness and Kurtosis with Classification
#'
#' Computes skewness and kurtosis (bias-corrected) using the \pkg{DescTools} package and classifies the results.
#'
#' @param x Numeric vector.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed before computation.
#'
#' @return A list with:
#' \describe{
#'   \item{skewness}{Skewness value}
#'   \item{skew_class}{Interpretation of skewness}
#'   \item{kurtosis}{Kurtosis value}
#'   \item{kurt_class}{Interpretation of kurtosis}
#' }
#'
#' @importFrom DescTools Skew Kurt
#' @export
SkewKurt_diamond <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) < 8) stop("Sample size too small to evaluate distribution shape.")

  skew <- DescTools::Skew(x)
  kurt <- DescTools::Kurt(x)

  classify <- function(value) {
    if (value < -2) "Severely negative"
    else if (value < -1) "Moderately negative"
    else if (value < -0.5) "Slightly negative"
    else if (value <= 0.5) "Approximately normal"
    else if (value <= 1) "Slightly positive"
    else if (value <= 2) "Moderately positive"
    else "Severely positive"
  }

  list(
    skewness = round(skew, 2),
    skew_class = classify(skew),
    kurtosis = round(kurt, 2),
    kurt_class = classify(kurt)
  )
}

#' Correlation Analysis with Confidence Intervals and Interpretation
#'
#' Computes Pearson, Spearman, and/or Kendall correlation between two numeric vectors,
#' optionally grouped by a subset factor. Outputs coefficient, shared variance, p-value, confidence intervals,
#' and a qualitative interpretation of strength.
#'
#' @param x A numeric vector or a formula of the form \code{y ~ x}.
#' @param y A numeric vector. Not required if \code{x} is a formula.
#' @param data Optional data frame in which variables in \code{x}, \code{y}, and \code{subset} are evaluated.
#' @param subset Optional vector (logical, factor, character, or numeric) used to define subgroups.
#' @param method Character vector specifying one or more correlation methods to use:
#'   \code{"pearson"}, \code{"spearman"}, \code{"kendall"}. Defaults to all three.
#' @param method.subset Optional vector of correlation methods to apply for each subgroup (recycled if needed).
#' @param use String indicating how missing values are handled. Default is \code{"complete.obs"}.
#' @param digits Integer. Number of digits for rounding results (default is 4).
#' @param conf.level Confidence level for the interval (default is 0.95).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{method}{The correlation method used.}
#'   \item{coefficient}{The correlation coefficient.}
#'   \item{shared_variance}{Squared coefficient (R²).}
#'   \item{p_value}{P-value of the test.}
#'   \item{ci_lower}{Lower bound of the confidence interval.}
#'   \item{ci_upper}{Upper bound of the confidence interval.}
#'   \item{interpretation}{Qualitative interpretation (e.g., "Moderate", "Strong").}
#'   \item{n}{Sample size.}
#'   \item{group}{Group name (if subset by group).}
#' }
#'
#' @examples
#' correlation_diamond(mtcars$mpg, mtcars$hp)
#' correlation_diamond(mpg ~ hp, data = mtcars)
#' correlation_diamond(mpg ~ hp, data = mtcars, subset = mtcars$cyl)
#'
#' @export
correlation_diamond <- function(x, y = NULL,
                                data = NULL,
                                subset = NULL,
                                method = c("pearson", "spearman", "kendall"),
                                method.subset = NULL,
                                use = "complete.obs",
                                digits = 4,
                                conf.level = 0.95) {
  if (!is.null(data)) data <- as.data.frame(data)

  if (inherits(x, "formula")) {
    vars <- all.vars(x)
    if (length(vars) != 2) stop("Formula must be of the form y ~ x")
    yvar <- vars[1]
    xvar <- vars[2]
    if (is.null(data)) stop("You must supply a 'data' argument when using a formula.")
    y <- data[[yvar]]
    x <- data[[xvar]]
  }

  if (is.null(x) || is.null(y)) stop("Both 'x' and 'y' must be provided.")

  if (!is.null(subset)) {
    subset_eval <- eval(substitute(subset), envir = if (!is.null(data)) data else parent.frame())

    if (is.logical(subset_eval)) {
      if (length(subset_eval) != length(x)) {
        stop("'subset' must be a logical vector of the same length as 'x' and 'y'")
      }
      x <- x[subset_eval]
      y <- y[subset_eval]
    } else if (is.factor(subset_eval) || is.character(subset_eval) || is.numeric(subset_eval)) {
      subset_levels <- unique(subset_eval)
      n_levels <- length(subset_levels)
      all_results <- list()

      if (!is.null(method.subset)) {
        method.subset <- tolower(method.subset)
        if (length(method.subset) < n_levels) {
          method.subset <- rep(method.subset, length.out = n_levels)
          warning("`method.subset` recycled to match number of levels.")
        }
      }

      for (i in seq_along(subset_levels)) {
        lev <- subset_levels[i]
        lev_filter <- subset_eval == lev
        x_sub <- x[lev_filter]
        y_sub <- y[lev_filter]

        if (length(x_sub) >= 3 && length(y_sub) >= 3) {
          temp_method <- if (!is.null(method.subset)) method.subset[i] else method
          temp <- correlation_diamond(x_sub, y_sub,
                                      method = temp_method, use = use,
                                      digits = digits, conf.level = conf.level)
          temp$group <- as.character(lev)
          all_results[[as.character(lev)]] <- temp
        }
      }

      final <- do.call(rbind, all_results)
      rownames(final) <- NULL
      return(final)
    } else {
      stop("'subset' must be a logical vector or a categorical/numeric variable")
    }
  }

  if (length(x) != length(y)) stop("x and y must have the same length")
  if (use == "complete.obs") {
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
  }

  n <- length(x)
  if (n < 3) stop("At least 3 complete observations required")

  method <- tolower(method)

  get_strength <- function(r, type) {
    cutoffs <- switch(type,
                      pearson = c(0.00, 0.10, 0.40, 0.70, 0.90),
                      spearman = c(0.00, 0.10, 0.38, 0.68, 0.89),
                      kendall = c(0.00, 0.06, 0.26, 0.49, 0.71))
    labels <- c("Negligible", "Weak", "Moderate", "Strong", "Very Strong")
    idx <- max(which(abs(r) >= cutoffs))
    labels[idx]
  }

  format_p <- function(p, digits) {
    threshold <- 10^(-digits)
    if (is.na(p)) return(NA)
    if (p < threshold) paste0("< ", format(threshold, scientific = FALSE))
    else round(p, digits)
  }

  results <- list()

  if ("pearson" %in% method) {
    p <- suppressWarnings(cor.test(x, y, method = "pearson", conf.level = conf.level))
    results[["pearson"]] <- data.frame(
      method = "Pearson",
      coefficient = round(unname(p$estimate), digits),
      shared_variance = round(unname(p$estimate)^2, digits),
      p_value = format_p(p$p.value, digits),
      ci_lower = round(if (!is.null(p$conf.int)) p$conf.int[1] else NA, digits),
      ci_upper = round(if (!is.null(p$conf.int)) p$conf.int[2] else NA, digits),
      interpretation = get_strength(p$estimate, "pearson"),
      n = n,
      stringsAsFactors = FALSE
    )
  }

  if ("spearman" %in% method) {
    s <- suppressWarnings(cor.test(x, y, method = "spearman", conf.level = conf.level, exact = FALSE))
    results[["spearman"]] <- data.frame(
      method = "Spearman",
      coefficient = round(unname(s$estimate), digits),
      shared_variance = round(unname(s$estimate)^2, digits),
      p_value = format_p(s$p.value, digits),
      ci_lower = round(if (!is.null(s$conf.int)) s$conf.int[1] else NA, digits),
      ci_upper = round(if (!is.null(s$conf.int)) s$conf.int[2] else NA, digits),
      interpretation = get_strength(s$estimate, "spearman"),
      n = n,
      stringsAsFactors = FALSE
    )
  }

  if ("kendall" %in% method) {
    k <- suppressWarnings(cor.test(x, y, method = "kendall", exact = FALSE))
    results[["kendall"]] <- data.frame(
      method = "Kendall",
      coefficient = round(unname(k$estimate), digits),
      shared_variance = round(unname(k$estimate)^2, digits),
      p_value = format_p(k$p.value, digits),
      ci_lower = NA,
      ci_upper = NA,
      interpretation = get_strength(k$estimate, "kendall"),
      n = n,
      stringsAsFactors = FALSE
    )
  }

  final <- do.call(rbind, results)
  rownames(final) <- NULL
  return(final)
}

#' Influence Analysis on Correlation Coefficient
#'
#' Evaluates how each observation affects the overall correlation coefficient (Pearson, Spearman, or Kendall)
#' using a leave-one-out approach. Optionally highlights points that most increase or decrease the correlation.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as \code{x}.
#' @param method The correlation method to use: \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' @param digits Number of decimal places to round the output.
#' @param improve Optional integer. Number of points to show that most increase the correlation.
#' @param worsen Optional integer. Number of points to show that most decrease the correlation.
#' @param plot Logical. If \code{TRUE}, generates a scatterplot highlighting influential points.
#' @param threshold Numeric. Minimum correlation change to consider as influential (default = 0.01).
#' @param subset Optional logical vector for subsetting \code{x} and \code{y} before analysis.
#'
#' @return A data frame with the leave-one-out correlation, its change from the full sample,
#' and whether each point increases, decreases, or does not affect correlation meaningfully.
#' The return includes an attribute \code{"global_correlation"} with the original correlation value.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30)
#' y <- x + rnorm(30, sd = 0.3)
#' cor_influence_diamond(x, y, method = "pearson", improve = 2, worsen = 2, plot = TRUE)
#'
#' @export
cor_influence_diamond <- function(x, y, method = "pearson", digits = 4,
                                  improve = NULL, worsen = NULL,
                                  plot = FALSE, threshold = 0.01,
                                  subset = NULL) {
  if (!is.null(subset)) {
    if (!is.logical(subset) || length(subset) != length(x)) {
      stop("'subset' must be a logical vector of the same length as 'x' and 'y'")
    }
    x <- x[subset]
    y <- y[subset]
  }

  if (length(x) != length(y)) stop("x and y must be the same length.")
  n <- length(x)
  global_cor <- suppressWarnings(cor(x, y, method = method, use = "complete.obs"))
  influence <- numeric(n)

  for (i in 1:n) {
    xi <- x[-i]
    yi <- y[-i]
    influence[i] <- suppressWarnings(cor(xi, yi, method = method, use = "complete.obs"))
  }

  delta <- influence - global_cor

  df_all <- data.frame(
    id = 1:n,
    x = x,
    y = y,
    leave_one_out_cor = round(influence, digits),
    change = round(delta, digits),
    effect = ifelse(delta > threshold, "increase",
                    ifelse(delta < -threshold, "decrease", "neutral"))
  )

  selected <- NULL

  if (!is.null(improve)) {
    df_inc <- df_all[df_all$effect == "increase", ]
    df_inc <- df_inc[order(-df_inc$change), ][1:min(improve, nrow(df_inc)), ]
    selected <- df_inc
  }

  if (!is.null(worsen)) {
    df_dec <- df_all[df_all$effect == "decrease", ]
    df_dec <- df_dec[order(df_dec$change), ][1:min(worsen, nrow(df_dec)), ]
    selected <- if (is.null(selected)) df_dec else rbind(selected, df_dec)
  }

  if (is.null(improve) && is.null(worsen)) {
    selected <- df_all[order(-abs(df_all$change)), ]
  }

  if (plot) {
    colors <- rep("gray80", n)
    colors[selected$id] <- ifelse(selected$effect == "increase", "red",
                                  ifelse(selected$effect == "decrease", "blue", "gray80"))

    op <- par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
    plot(x, y, pch = 19, col = colors,
         xlab = "x", ylab = "y", main = "Selected influential points on correlation")
    abline(lm(y ~ x), col = "black", lwd = 2, lty = 2)

    legend("bottom", inset = -0.2,
           legend = c("Increase", "Decrease"),
           col = c("red", "blue"), pch = 19, horiz = TRUE, cex = 0.8, bty = "n")

    text(selected$x, selected$y, labels = selected$id,
         pos = 3, col = ifelse(selected$effect == "increase", "red", "blue"),
         cex = 0.8)

    par(op)
  }

  rownames(selected) <- NULL
  attr(selected, "global_correlation") <- round(global_cor, digits)
  return(selected)
}
#' Partial Correlation with Optional Grouping
#'
#' Computes the partial correlation between \code{x} and \code{y}, controlling for \code{z},
#' optionally grouped by a subset variable. Supports Pearson, Spearman, and Kendall methods.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param z Numeric vector or data frame of control variables.
#' @param data Optional data frame.
#' @param subset Optional logical or grouping variable (factor, character, numeric).
#' @param method Correlation method(s): \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' @param subset.method Optional vector of methods to use for each subgroup.
#' @param digits Number of digits to round the output.
#'
#' @return A data frame with method, coefficient, shared variance, p-value, interpretation, and sample size.
#'
#' @importFrom ppcor pcor.test
#' @export
pcorrelation_diamond <- function(x, y, z, data = NULL, subset = NULL,
                                 method = c("pearson", "spearman", "kendall"),
                                 subset.method = NULL,
                                 digits = 4) {
  # helper function included inside
  get_strength <- function(r, type) {
    cutoffs <- switch(type,
                      pearson = c(0.00, 0.10, 0.40, 0.70, 0.90),
                      spearman = c(0.00, 0.10, 0.38, 0.68, 0.89),
                      kendall = c(0.00, 0.06, 0.26, 0.49, 0.71))
    labels <- c("Negligible", "Weak", "Moderate", "Strong", "Very Strong")
    idx <- max(which(abs(r) >= cutoffs))
    labels[idx]
  }

  if (!requireNamespace("ppcor", quietly = TRUE)) {
    stop("Package 'ppcor' is required. Please install it with install.packages('ppcor').")
  }

  method <- match.arg(method, choices = c("pearson", "spearman", "kendall"), several.ok = TRUE)
  if (!is.null(data)) data <- as.data.frame(data)

  if (!is.null(subset)) {
    subset_eval <- eval(substitute(subset), envir = if (!is.null(data)) data else parent.frame())

    if (is.logical(subset_eval)) {
      x <- x[subset_eval]
      y <- y[subset_eval]
      z <- if (is.data.frame(z)) z[subset_eval, , drop = FALSE] else z[subset_eval]
    } else if (is.factor(subset_eval) || is.character(subset_eval) || is.numeric(subset_eval)) {
      levels <- unique(subset_eval)
      n_levels <- length(levels)
      results_all <- list()

      if (!is.null(subset.method)) {
        subset.method <- tolower(subset.method)
        if (length(subset.method) < n_levels) {
          subset.method <- rep(subset.method, length.out = n_levels)
          warning("`subset.method` recycled to match number of groups.")
        }
      }

      for (i in seq_along(levels)) {
        lev <- levels[i]
        idx <- subset_eval == lev
        x_sub <- x[idx]
        y_sub <- y[idx]
        z_sub <- if (is.data.frame(z)) z[idx, , drop = FALSE] else z[idx]

        complete_cases <- complete.cases(x_sub, y_sub, z_sub)
        x_sub <- x_sub[complete_cases]
        y_sub <- y_sub[complete_cases]
        z_sub <- if (is.data.frame(z_sub)) z_sub[complete_cases, , drop = FALSE] else z_sub[complete_cases]

        n <- length(x_sub)
        if (n < 4) next

        selected_methods <- if (!is.null(subset.method)) subset.method[i] else method

        for (m in selected_methods) {
          pc <- suppressWarnings(ppcor::pcor.test(x_sub, y_sub, z_sub, method = m))
          r <- unname(pc$estimate)
          results_all[[paste(lev, m, sep = "_")]] <- data.frame(
            method = switch(m, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall"),
            coefficient = round(r, digits),
            shared_variance = round(r^2, digits),
            p_value = if (is.na(pc$p.value)) NA else format(pc$p.value, scientific = TRUE, digits = digits),
            ci_lower = NA,
            ci_upper = NA,
            interpretation = get_strength(r, m),
            n = n,
            group = as.character(lev),
            stringsAsFactors = FALSE
          )
        }
      }

      final <- do.call(rbind, results_all)
      rownames(final) <- NULL
      return(final)
    }
  }

  if (!is.null(data)) {
    x <- eval(substitute(x), envir = data)
    y <- eval(substitute(y), envir = data)
    z <- eval(substitute(z), envir = data)
  }

  complete_cases <- complete.cases(x, y, z)
  x <- x[complete_cases]
  y <- y[complete_cases]
  z <- if (is.data.frame(z)) z[complete_cases, , drop = FALSE] else z[complete_cases]

  n <- length(x)
  if (n < 4) stop("At least 4 complete observations are required.")

  results <- list()
  for (m in method) {
    pc <- suppressWarnings(ppcor::pcor.test(x, y, z, method = m))
    r <- unname(pc$estimate)
    results[[m]] <- data.frame(
      method = switch(m, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall"),
      coefficient = round(r, digits),
      shared_variance = round(r^2, digits),
      p_value = if (is.na(pc$p.value)) NA else format(pc$p.value, scientific = TRUE, digits = digits),
      ci_lower = NA,
      ci_upper = NA,
      interpretation = get_strength(r, m),
      n = n,
      group = NA,
      stringsAsFactors = FALSE
    )
  }

  final <- do.call(rbind, results)
  rownames(final) <- NULL
  return(final)
}


#' Multivariate Normality Test Using Energy Test
#'
#' Performs the multivariate normality test (E-test) from the \pkg{energy} package
#' on two or more numeric vectors. Supports optional subsetting or group-wise analysis.
#'
#' @param ... Two or more numeric vectors (same length) to test for multivariate normality.
#' @param subset Optional logical or grouping vector (factor, character, or numeric)
#'   to filter or split the dataset.
#' @param R Integer. Number of replicates used in the energy test (default is 1000).
#' @param na.omit Logical. If \code{TRUE}, removes rows with missing data before testing.
#'
#' @return Invisibly returns the test result(s), either a single \code{htest} object or a list
#'   of such objects by group. Also prints formatted results in the console.
#'
#' @details
#' This function wraps \code{\link[energy]{mvnorm.etest}} and provides summary output.
#' If \code{subset} is a factor or character vector, the test is applied group-wise and
#' results are printed per group. It supports 2 or more numeric vectors.
#'
#' @importFrom energy mvnorm.etest
#'
#' @examples
#' if (requireNamespace("energy", quietly = TRUE)) {
#'   set.seed(123)
#'   x <- rnorm(100)
#'   y <- rnorm(100)
#'   mvnorm(x, y)
#' }
#'
#' @export
mvnorm <- function(..., subset = NULL, R = 1000, na.omit = TRUE) {
  if (!requireNamespace("energy", quietly = TRUE)) install.packages("energy", dependencies = TRUE)
  library(energy)

  vars <- list(...)
  if (length(vars) < 2) stop("At least two numeric vectors must be provided.")

  df <- as.data.frame(vars)
  if (!all(sapply(df, is.numeric))) stop("All inputs must be numeric vectors.")

  if (!is.null(subset)) {
    subset_eval <- eval(substitute(subset), envir = parent.frame())

    if (is.logical(subset_eval)) {
      if (length(subset_eval) != nrow(df)) stop("Logical subset must be same length as data.")
      df <- df[subset_eval, ]
    } else if (is.factor(subset_eval) || is.character(subset_eval) || is.numeric(subset_eval)) {
      levels <- unique(subset_eval)
      results <- list()

      for (lev in levels) {
        idx <- which(subset_eval == lev)
        df_sub <- df[idx, ]
        if (na.omit) df_sub <- na.omit(df_sub)
        if (any(!complete.cases(df_sub))) {
          cat("Group:", lev, "\n")
          cat("Missing values present. Set na.omit = TRUE to ignore them.\n\n")
          next
        }

        cat("Group:", lev, "\n")
        cat("Valid observations:", nrow(df_sub), "\n")

        if (nrow(df_sub) >= 3 && all(sapply(df_sub, is.numeric))) {
          test_result <- tryCatch(
            mvnorm.etest(as.matrix(df_sub), R = R),
            error = function(e) return(NULL)
          )
          if (!is.null(test_result)) {
            cat("\n$multivariate_normality\n")
            cat("Energy e-test: Statistic =", round(test_result$statistic, 3),
                ", p =", format(test_result$p.value, scientific = TRUE, digits = 5),
                "→", ifelse(test_result$p.value > 0.05, "✓ Normal", "✗ Not normal"), "\n\n")
            results[[as.character(lev)]] <- test_result
          } else {
            cat("Test failed due to internal error.\n\n")
          }
        } else {
          cat("Not enough valid data for test or non-numeric values.\n\n")
        }
      }

      return(invisible(results))
    } else {
      stop("'subset' must be a logical, factor, character, or numeric variable.")
    }
  }

  if (na.omit) df <- na.omit(df)
  if (any(!complete.cases(df))) stop("Missing values present. Set na.omit = TRUE to ignore them.")
  if (nrow(df) < 3) {
    cat("Not enough valid data.\n")
    return(invisible(NULL))
  }

  cat("Valid observations:", nrow(df), "\n")
  test_result <- mvnorm.etest(as.matrix(df), R = R)
  cat("\n$multivariate_normality\n")
  cat("Energy e-test: Statistic =", round(test_result$statistic, 3),
      ", p =", format(test_result$p.value, scientific = TRUE, digits = 5),
      "→", ifelse(test_result$p.value > 0.05, "✓ Normal", "✗ Not normal"), "\n")
  return(invisible(test_result))
}

#' Linear Model Summary with Residual Diagnostics and Influence Metrics
#'
#' Provides a summary of a linear model including fitted values, residuals, standardized residuals,
#' leverage (hat values), and Cook's distance. Identifies influential and extreme residuals, with
#' optional plotting and filtering of most/least extreme points.
#'
#' @param input A formula (e.g. \code{y ~ x}) or an object of class \code{lm}.
#' @param data Optional data frame if a formula is supplied.
#' @param best_res Integer. Number of observations with smallest residuals to include.
#' @param worst_res Integer. Number of observations with largest residuals to include.
#' @param plot Logical. If \code{TRUE}, plots the residuals with color-coded influence.
#' @param order Sorting order of residuals: \code{"desc"} (default) or \code{"asc"}.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{y}{Observed values of the dependent variable.}
#'   \item{x}{Values of the independent variable.}
#'   \item{fitted}{Fitted values from the model.}
#'   \item{residuals}{Raw residuals.}
#'   \item{resid_z}{Standardized residuals.}
#'   \item{hat_value}{Leverage (hat) values.}
#'   \item{cook_d}{Cook's distance.}
#'   \item{critical}{Logical flag for influential observations (based on residuals, leverage, or Cook's D).}
#' }
#'
#' @details
#' Observations with \code{|standardized residual| > 2}, high leverage
#' (\code{hat > 2 * mean(hat)}), or high Cook's distance (\code{> 4/n}) are marked as \code{critical}.
#'
#' If \code{plot = TRUE}, a scatterplot is shown with:
#' \itemize{
#'   \item Red points: worst residuals
#'   \item Blue points: best residuals
#'   \item Asterisk on label: critical observation
#' }
#'
#' @examples
#' model <- lm(mpg ~ hp, data = mtcars)
#' lm_summary(model, worst_res = 3, plot = TRUE)
#'
#' @export
lm_summary <- function(input, data = NULL, best_res = NULL, worst_res = NULL,
                       plot = FALSE, order = c("desc", "asc")) {
  order <- match.arg(order)

  if (inherits(input, "formula")) {
    model <- lm(input, data = data)
  } else if (inherits(input, "lm")) {
    model <- input
  } else {
    stop("Input must be a formula or an lm object")
  }

  mf <- model.frame(model)
  y <- mf[[1]]
  x <- mf[[2]]
  y_name <- names(mf)[1]
  x_name <- names(mf)[2]

  fitted_vals <- fitted(model)
  residuals_vals <- residuals(model)
  hat_values <- hatvalues(model)
  cook_d <- cooks.distance(model)
  resid_z <- rstandard(model)

  df <- data.frame(
    y = y,
    x = x,
    fitted = fitted_vals,
    residuals = residuals_vals,
    resid_z = resid_z,
    hat_value = hat_values,
    cook_d = cook_d
  )
  names(df)[1:2] <- c(y_name, x_name)

  n <- nrow(df)
  hat_threshold <- 2 * mean(hat_values)
  cook_threshold <- 4 / n
  df$critical <- abs(resid_z) > 2 | hat_values > hat_threshold | cook_d > cook_threshold

  ord_z <- if (order == "desc") {
    order(abs(df$resid_z), decreasing = TRUE)
  } else {
    order(abs(df$resid_z), decreasing = FALSE)
  }

  if (!is.null(best_res) || !is.null(worst_res)) {
    best_rows <- if (!is.null(best_res)) tail(ord_z, best_res) else integer(0)
    worst_rows <- if (!is.null(worst_res)) head(ord_z, worst_res) else integer(0)
    selected <- sort(unique(c(best_rows, worst_rows)))
    df <- df[selected, ]
    df <- df[order(abs(df$resid_z), decreasing = (order == "desc")), ]
  } else {
    df <- df[ord_z, ]
  }

  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(df[num_cols], function(col) round(col, 5))

  if (plot) {
    full_n <- length(y)
    col_vector <- rep("black", full_n)
    labels <- rep(NA, full_n)

    best_rows_all <- if (!is.null(best_res)) tail(ord_z, best_res) else integer(0)
    worst_rows_all <- if (!is.null(worst_res)) head(ord_z, worst_res) else integer(0)

    col_vector[best_rows_all] <- "blue"
    col_vector[worst_rows_all] <- "red"

    labels[best_rows_all] <- as.character(best_rows_all)
    labels[worst_rows_all] <- as.character(worst_rows_all)

    crit_idx <- which(df$critical)
    labels[crit_idx] <- paste0(labels[crit_idx], "*")

    plot(x, y, col = col_vector, pch = 19,
         main = paste("Regression:", y_name, "~", x_name),
         xlab = x_name, ylab = y_name)
    abline(model, col = "gray", lwd = 1.5)
    text(x, y, labels = labels, pos = 3, cex = 0.7, col = "gray40")

    legend("bottom", inset = -0.3, xpd = TRUE,
           legend = c("Worst residuals", "Best residuals", "Other", "Critical (*)"),
           col = c("red", "blue", "black", NA), pch = c(19, 19, 19, NA),
           horiz = TRUE, cex = 0.8, bty = "n", text.col = c("red", "blue", "black", "black"))
  }

  return(df)
}

#' Plot binomial probability mass function
#'
#' @param x Vector of integer values (number of successes)
#' @param size Number of trials
#' @param prob Probability of success
#' @param show_table Logical, whether to print probability table
#' @export
dbinom_plot <- function(x, size, prob, show_table = FALSE) {
  if (missing(x) || missing(size) || missing(prob)) {
    stop("Please specify 'x', 'size', and 'prob'.")
  }

  probs <- dbinom(x, size = size, prob = prob)
  mode_index <- which.max(probs)
  mode_x <- x[mode_index]
  mode_y <- probs[mode_index]

  if (show_table) {
    table <- data.frame(
      `Successes (x)` = x,
      `P(X = x)` = round(probs, 4)
    )
    print(format(table, justify = "right"), row.names = FALSE)
  }

  colors <- rep("skyblue", length(x))
  colors[mode_index] <- "orange"

  barplot(probs, names.arg = x,
          col = colors, border = "black",
          main = paste("Binomial Distribution (n =", size, ", p =", prob, ")"),
          xlab = "Number of Successes", ylab = "Probability")

  text(x = mode_index,
       y = mode_y + max(probs) * 0.05,
       labels = paste("Mode =", mode_x),
       col = "red", font = 2)
}






