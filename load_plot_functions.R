
### LOAD PLOT FUNCTIONS SCRIPT

library(tidyverse)
library(ggrepel)
library(here)

### Create plot theme `theme_classic_grid`

theme_classic_grid <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "grey85"),
      #panel.grid.minor = element_line(color = "grey92"),
      axis.title.x = element_text(size = base_size + 2),
      axis.title.y = element_text(size = base_size + 2)
    )
}

### Create function `hist_plot`

hist_plot <- function(
    data,
    x_var,
    binwidth = NULL,
    fill = "orange",
    color = "white",
    alpha = 0.6,
    mean_line = FALSE,
    median_line = FALSE,
    skew_value = FALSE,
    kurtosis_value = FALSE,
    break_min = NULL,
    break_max = NULL,
    break_by  = NULL,
    x_label   = NULL,
    title     = NULL
) {
  # pull vector, drop non-finite
  x <- dplyr::pull(data, {{ x_var }})
  x <- as.numeric(x[is.finite(x)])
  
  # summary stats
  m   <- mean(x); md <- median(x); sdv <- stats::sd(x)
  z   <- if (sdv > 0) (x - m) / sdv else x*0
  sk  <- if (sdv > 0) mean(z^3) else 0
  kt  <- if (sdv > 0) mean(z^4) - 3 else 0
  
  # smart binwidth
  if (is.null(binwidth)) {
    iqr <- stats::IQR(x, na.rm = TRUE); n <- length(x)
    bw_fd <- if (iqr > 0) 2 * iqr / (n^(1/3)) else NA_real_
    bw_sc <- if (sdv > 0) 3.5 * sdv / (n^(1/3)) else NA_real_
    binwidth <- bw_fd
    if (!is.finite(binwidth) || binwidth <= 0) binwidth <- bw_sc
    if (!is.finite(binwidth) || binwidth <= 0) binwidth <- diff(range(x))/30
  }
  
  # breaks for counts / ymax
  xmin <- floor(min(x) / binwidth) * binwidth
  xmax <- ceiling(max(x) / binwidth) * binwidth
  brks <- seq(xmin, xmax + binwidth, by = binwidth)
  h    <- hist(x, breaks = brks, plot = FALSE)
  ymax <- max(h$counts)
  
  # y-axis (integers only)
  by_int   <- max(1L, floor(ymax / 6))
  y_breaks <- seq(0, ymax, by = by_int)
  
  # x-axis breaks: user-specified or auto
  if (!is.null(break_min) && !is.null(break_max) && !is.null(break_by)) {
    x_breaks <- seq(break_min, break_max, by = break_by)
    x_limits <- c(break_min, break_max)
  } else {
    x_min_lab <- floor(min(x))
    x_max_lab <- ceiling(max(x))
    x_step <- max(1, round((x_max_lab - x_min_lab) / 8))
    x_breaks <- seq(x_min_lab, x_max_lab, by = x_step)
    x_limits <- c(x_min_lab, x_max_lab)
  }
  
  # ----- labels text & placement (top-right) -----
  skew_lab <- paste0("Skew = ", round(sk,1))
  kurt_lab <- paste0("Kurtosis = ", round(kt,1))
  mean_lab <- paste0("Mean = ", round(m,1))
  medn_lab <- paste0("Median = ", round(md,1))
  
  # your requested placement rule:
  lab_x    <- max(x_limits) * 0.85
  lab_y_mn <- ymax * 0.95
  lab_y_md <- ymax * 0.85
  lab_y_sk <- ymax * 0.75   # mean below kurtosis
  lab_y_kt <- ymax * 0.65   # median below mean
  
  p <- ggplot(data, aes(x = {{ x_var }})) +
    geom_histogram(binwidth = binwidth, fill = fill, color = color, alpha = alpha, boundary = xmin) +
    scale_y_continuous(breaks = y_breaks, expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, expand = expansion(mult = c(0.01, 0.01))) +
    labs(x = x_label, y = "Frequency", title = title) +
    theme_classic() +
    theme(
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.grid.major.x = element_line(colour = "grey95"),
      panel.grid.minor = element_blank()
    )
  
  if (mean_line)   p <- p + geom_vline(xintercept = m,  color = "darkblue", linewidth = 0.8)
  if (median_line) p <- p + geom_vline(xintercept = md, color = "darkblue", linetype = "dashed", linewidth = 0.8)
  
  # top-right labels (no leader lines)
  if (skew_value) {
    p <- p + geom_label(
      data = tibble(x = lab_x, y = lab_y_sk, lab = skew_lab),
      aes(x = x, y = y, label = lab),
      inherit.aes = FALSE, fill = "white", color = "darkblue", size = 5, label.size = 0.25
    )
  }
  if (kurtosis_value) {
    p <- p + geom_label(
      data = tibble(x = lab_x, y = lab_y_kt, lab = kurt_lab),
      aes(x = x, y = y, label = lab),
      inherit.aes = FALSE, fill = "white", color = "darkblue", size = 5, label.size = 0.25
    )
  }
  if (mean_line) {
    p <- p + geom_label(
      data = tibble(x = lab_x, y = lab_y_mn, lab = mean_lab),
      aes(x = x, y = y, label = lab),
      inherit.aes = FALSE, fill = "white", color = "darkblue", size = 5, label.size = 0.25
    )
  }
  if (median_line) {
    p <- p + geom_label(
      data = tibble(x = lab_x, y = lab_y_md, lab = medn_lab),
      aes(x = x, y = y, label = lab),
      inherit.aes = FALSE, fill = "white", color = "darkblue", size = 5, label.size = 0.25
    )
  }
  
  p
}

### Create function `scatter_plot`

scatter_plot <- function(
    data,
    x_var,
    y_var,
    point_color = "steelblue",
    size = 3,
    alpha = 0.7,
    jitter_width = 0.4,
    jitter_height = 0.4,
    corr_line = TRUE,
    title = NULL,
    x_label = NULL,
    y_label = NULL
) {
  # pull vectors & drop non-finite pairs
  x <- dplyr::pull(data, {{ x_var }})
  y <- dplyr::pull(data, {{ y_var }})
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  df <- tibble(x = x, y = y)
  
  # correlation
  r_val <- suppressWarnings(cor(x, y, use = "complete.obs"))
  r_lab <- paste0("Correlation = ", sprintf("%.2f", r_val))
  
  # label position (top-right corner at 0.9*max)
  x_rng <- range(x); y_rng <- range(y)
  x0 <- x_rng[1] + 0.9 * diff(x_rng)
  y0 <- y_rng[1] + 0.9 * diff(y_rng)
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_jitter(
      color = point_color,
      size = size,
      alpha = alpha,
      width = jitter_width,
      height = jitter_height
    )
  
  if (isTRUE(corr_line)) {
    p <- p + geom_smooth(method = "lm", se = FALSE, color = "darkblue")
  }
  
  p +
    geom_label(
      data = tibble(x = x0, y = y0, lab = r_lab),
      aes(x = x, y = y, label = lab),
      inherit.aes = FALSE,
      color = "darkblue",
      fill = "white",
      size = 5,
      label.size = 0.25
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_classic() +
    theme(
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.grid.major.x = element_line(colour = "grey95"),
      panel.grid.minor = element_blank()
    )
}


# Visualize t-crit region and observed t-stat

plot_tdist <- function(t_obs, df, alpha = 0.05) {
  
  # critical values & p-value
  t_crit <- qt(1 - alpha/2, df)
  p_val  <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)
  
  # data for t pdf
  t_grid <- tibble(
    x = seq(-5, 5, length.out = 2000),
    d = dt(x, df),
    in_tail = abs(x) >= t_crit
  )
  
  # base plot
ggplot(t_grid, aes(x, d)) +
    # shaded tail regions
    geom_area(
      data = dplyr::filter(t_grid, x <= -t_crit),
      aes(y = d),
      fill = "grey50", alpha = 0.4
    ) +
    geom_area(
      data = dplyr::filter(t_grid, x >=  t_crit),
      aes(y = d),
      fill = "grey50", alpha = 0.4
    ) +
    # t-distribution curve
    geom_line(color = "black", linewidth = 1) +
    # critical value lines
    geom_vline(xintercept = c(-t_crit, t_crit), linetype = "dashed") +
    # observed t-statistic (red line)
    geom_vline(xintercept = t_obs, color = "red", linewidth = 1.2) +
    # alpha region labels (moved upward)
    annotate("text",
             x = -t_crit - 0.3,
             y = dt(t_crit, df) + 0.03,
             label = sprintf("α = %.4f", alpha/2),
             hjust = 1, size = 4) +
    annotate("text",
             x =  t_crit + 0.3,
             y = dt(t_crit, df) + 0.03,
             label = sprintf("α = %.4f", alpha/2),
             hjust = 0, size = 4) +
    # labels and theme
    labs(
      subtitle = sprintf("df = %d, t = %.2f, α = %.3f", df, t_obs, alpha),
      x = "X",
      y = "Probability Density"
    ) +
    coord_cartesian(xlim = c(-5, 5)) +
    theme_classic(base_size = 13)
}

### Plot grouped distributions (3 ways)

library(ggridges)
library(patchwork)
library(forcats)
library(tools)

theme_fancy <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank())
}

plot_group_dist <- function(data, outcome, group_var, title,
                            subtitle = "Sample of 400 movies from IMDB",
                            colors = c("#0288b7", "#a90010")) {
  
  # keep labels as in original (title-case "rating" -> "Rating")
  x_lab <- toTitleCase(outcome)
  
  # make a helper factor column for consistent labels/facets
  d <- data %>% mutate(.grp = factor(.data[[group_var]]))
  
  # BOX PLOT
  p_box <- ggplot(d, aes(x = .grp, y = .data[[outcome]], fill = .grp)) +
    geom_boxplot() +
    scale_fill_manual(values = colors, guide = FALSE) +
    scale_y_continuous(breaks = seq(1, 10, 1)) +
    labs(x = NULL, y = x_lab) +
    theme_fancy()
  
  # HISTOGRAM (faceted by group)
  p_hist <- ggplot(d, aes(x = .data[[outcome]], fill = .grp)) +
    geom_histogram(binwidth = 1, color = "white") +
    scale_fill_manual(values = colors, guide = FALSE) +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    labs(y = "Count", x = x_lab) +
    facet_wrap(~ .grp, nrow = 2) +
    theme_fancy() +
    theme(panel.grid.major.x = element_blank())
  
  # RIDGES (median line; group labels from factor, reversed as original)
  p_ridges <- ggplot(d, aes(x = .data[[outcome]], y = fct_rev(.grp), fill = .grp)) +
    stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "white") +
    scale_fill_manual(values = colors, guide = FALSE) +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    labs(x = x_lab, y = NULL, subtitle = "White line shows median rating") +
    theme_fancy()
  
  # assemble like original
  (p_box | p_hist) / p_ridges +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme(
        #text = element_text(family = "Asap Condensed"),
        #plot.title = element_text(face = "bold", size = rel(1))
      )
    )
}

#############################

# plot_group_dist(
#   data = ac_movies,
#   x = "rating",
#   y = "action_comedy",
#   title = "Do comedies get higher ratings than action movies?",
#   subtitle = "Sample of 400 movies from IMDB")

#############################

mean_diff_barplot <- function(data, group_var, title, outcome = "rating",
                              colors = c("#0288b7", "#a90010")) {
  
  # group means
  mns <- data %>%
    mutate(.grp = factor(.data[[group_var]])) %>%
    group_by(.grp) %>%
    summarize(mean_value = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  if (nrow(mns) != 2) warning("Function expects exactly 2 groups in `x`.")
  
  n_groups <- nrow(mns)
  colors_use <- rep_len(colors, n_groups)
  
  # mean difference: first level minus second level (shown in subtitle)
  diff_val <- mns$mean_value[1] - mns$mean_value[2]
  subtitle_txt <- sprintf("Mean difference (%s − %s) = %.3f",
                          as.character(mns$.grp[1]), as.character(mns$.grp[2]), diff_val)
  
  ggplot(mns, aes(x = .grp, y = mean_value, fill = .grp)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", mean_value)),
              vjust = -0.35, size = 4) +
    scale_fill_manual(values = colors_use) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = title,
      subtitle = subtitle_txt,
      x = NULL,
      y = "Mean Rating"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}

# mean_diff_barplot(
#   data = ac_movies,
#   outcome = "rating",
#   group_var = "action_comedy",
#   title = "Average Movie Rating",
#   colors = c("#0288b7", "#a90010"))
