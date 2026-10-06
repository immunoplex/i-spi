# =============================================================================
# precision_weight_panel.R
#
# Ported from std-curver/R/precision_weight_panel_m16.R +
# plate_diagnostic_plots.R (plate_palette/natural_sort_plates/.diag_theme),
# for the "Precision Weights" > Summary tab figure. The original panel was
# built against compute_model16_batch()'s in-memory output ($weights /
# $phi_table) and a fixed single feature_val, with antigen x source as the
# grid dimensions (one feature picked by re-running the call by hand).
#
# This port reads the PERSISTED curveRweights tables instead
# (calib_weights/calib_weights_fit via fetch_weights_panel_data()/
# fetch_weights_panel_fit() in calib_data_access.R) and flips the control
# scheme: antigen is the user-multi-selected panel dimension (one subplot per
# antigen, combining all of that antigen's sources within the one panel --
# colour = plate as before, one phi/beta1 annotation line per contributing
# source), and method (bayesian/frequentist) is an explicit single-select,
# since calib_weights/calib_weights_fit are keyed by method.
#
# Helpers below are small, deliberate copies (prefixed .pwp_) of
# plate_palette()/natural_sort_plates()/.diag_theme() from
# plate_diagnostic_plots.R rather than sourcing that 1573-line file for three
# ~10-line functions -- same light-duplication convention used throughout
# this app (see e.g. std_curve_weights_module.R's own file header).
# =============================================================================

.pwp_plate_palette <- function(plate_levels) {
  base_cols <- c(
    "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
    "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
    "#cab2d6", "#d9d900", "#8dd3c7", "#bebada", "#fb8072"
  )
  n <- length(plate_levels)
  if (n > length(base_cols))
    stop(".pwp_plate_palette: too many plates (", n, "); max supported = ",
         length(base_cols))
  stats::setNames(base_cols[seq_len(n)], plate_levels)
}

.pwp_natural_sort_plates <- function(x) {
  nums <- as.integer(sub("^[^0-9]*(\\d+).*$", "\\1", x))
  x[order(nums)]
}

.pwp_theme <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) %+replace%
    ggplot2::theme(
      panel.grid.major  = ggplot2::element_line(colour = "grey88", linewidth = 0.35),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(size = 12),
      axis.title        = ggplot2::element_text(size = 12),
      legend.position   = "none",
      plot.title        = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5,
                                                 margin = ggplot2::margin(b = 2)),
      plot.caption      = ggplot2::element_text(size = 9, hjust = 0, lineheight = 1.4,
                                                 colour = "grey25",
                                                 margin = ggplot2::margin(t = 10)),
      plot.margin       = ggplot2::margin(t = 4, r = 4, b = 2, l = 4)
    )
}

# ---- Internal: one antigen's panel (all its sources combined) -------------
.build_antigen_weight_panel <- function(d_antigen,
                                        fit_rows,
                                        plate_cols,
                                        panel_title  = "",
                                        is_first_col = TRUE,
                                        x_log        = TRUE,
                                        x_limits     = NULL,
                                        y_limits     = NULL,
                                        hist_bins    = 25,
                                        point_size   = 2.0) {

  d <- d_antigen[
    is.finite(d_antigen$predicted_concentration) & d_antigen$predicted_concentration > 0 &
    is.finite(d_antigen$w_norm)                  & d_antigen$w_norm                  > 0,
    , drop = FALSE
  ]
  if (nrow(d) < 5L)
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = .5, y = .5,
                          label = "insufficient weights data", colour = "grey50") +
        ggplot2::theme_void() + ggplot2::labs(title = panel_title)
    )

  # pcov_pass (TRUE/FALSE) is the analog of the original's binary weight_gated
  # (1/0): TRUE -> gate includes (anchor at mean-weight y = 1); FALSE -> gate
  # excludes (anchor at the axis floor y = 0).
  has_gate <- "pcov_pass" %in% names(d) && any(!is.na(d$pcov_pass))
  if (has_gate) d$gate_val <- as.integer(d$pcov_pass)

  # ---- phi/beta1 annotation: one line per contributing source --------------
  phi_ann <- if (!is.null(fit_rows) && nrow(fit_rows) > 0L) {
    lines <- vapply(seq_len(nrow(fit_rows)), function(i) {
      r <- fit_rows[i, ]
      phi_txt   <- if (is.na(r$phi[1L]))   "NA" else signif(r$phi[1L], 3L)
      beta1_txt <- if (is.na(r$beta1[1L])) "NA" else signif(r$beta1[1L], 3L)
      neff_txt  <- if (is.na(r$n_eff[1L])) "NA" else round(r$n_eff[1L], 1L)
      interp    <- if (is.na(r$interpretation[1L])) "" else r$interpretation[1L]
      sprintf("%s: phi=%s beta1=%s n_eff=%s\n%s",
              r$source[1L], phi_txt, beta1_txt, neff_txt, interp)
    }, character(1L))
    paste(lines, collapse = "\n")
  } else {
    "not estimated"
  }

  # ---- x-axis ----------------------------------------------------------------
  conc_vals <- d$predicted_concentration

  if (isTRUE(x_log)) {
    x_lo_log <- if (!is.null(x_limits)) log10(max(x_limits[1L], 1e-12))
                else floor(log10(min(conc_vals, na.rm = TRUE)))
    x_hi_log <- if (!is.null(x_limits)) log10(x_limits[2L])
                else ceiling(log10(max(conc_vals, na.rm = TRUE)))

    d$x_plot <- log10(d$predicted_concentration)
    x_breaks <- pretty(c(x_lo_log, x_hi_log), n = 5L)
    x_labels <- vapply(x_breaks, function(b) {
      v <- 10^b
      if (v >= 1e6)       formatC(v, format = "g",  digits = 2L)
      else if (v >= 1e3)  formatC(v, format = "fg", digits = 4L)
      else                formatC(v, format = "fg", digits = 3L)
    }, character(1L))
    hist_x  <- d$x_plot
    x_scale <- ggplot2::scale_x_continuous(
      breaks = x_breaks, labels = x_labels,
      limits = c(x_lo_log, x_hi_log),
      expand = ggplot2::expansion(mult = c(0.02, 0.03))
    )
    x_title <- "Predicted concentration (log10 scale)"
  } else {
    x_lo <- if (!is.null(x_limits)) x_limits[1L] else 0
    x_hi <- if (!is.null(x_limits)) x_limits[2L]
            else max(conc_vals, na.rm = TRUE) * 1.04
    d$x_plot <- d$predicted_concentration
    hist_x   <- d$x_plot
    x_scale  <- ggplot2::scale_x_continuous(
      breaks = pretty(c(x_lo, x_hi), n = 5L),
      limits = c(x_lo, x_hi), expand = ggplot2::expansion(add = c(0, 0))
    )
    x_title <- "Predicted concentration"
  }

  # ---- y-axis (w_norm; mean = 1 by construction) -----------------------------
  w_vals <- d$w_norm[is.finite(d$w_norm) & d$w_norm > 0]
  if (!is.null(y_limits) && all(is.finite(y_limits)) &&
      y_limits[1L] > 0 && y_limits[2L] > y_limits[1L]) {
    w_lo <- y_limits[1L]; w_hi <- y_limits[2L]
  } else {
    w_lo <- min(w_vals, na.rm = TRUE); w_hi <- max(w_vals, na.rm = TRUE)
  }
  if (!is.finite(w_lo) || !is.finite(w_hi) || w_lo >= w_hi) {
    w_lo <- min(w_vals, na.rm = TRUE); w_hi <- max(w_vals, na.rm = TRUE)
  }

  d$y_val  <- d$w_norm
  y_breaks <- pretty(c(0, w_hi), n = 5L); y_breaks <- y_breaks[y_breaks >= 0]
  y_labels <- formatC(y_breaks, format = "g", digits = 3L)
  Y_REF    <- 1            # mean(w_norm) = 1: anchor for gate overlay
  Y_BOTTOM <- 0
  Y_TOP    <- w_hi + 0.08 * w_hi
  y_title  <- if (is_first_col) expression(Precision~weight~(w[i]~norm.)) else NULL

  # ---- concentration histogram (below axis floor) ----------------------------
  hist_height <- (Y_TOP - Y_BOTTOM) * 0.22
  h <- graphics::hist(hist_x[is.finite(hist_x)], breaks = hist_bins, plot = FALSE)
  hist_df <- data.frame(
    xmin   = h$breaks[-length(h$breaks)],
    xmax   = h$breaks[-1L],
    height = h$density / max(h$density, na.rm = TRUE) * hist_height * 0.88
  )
  hist_df <- hist_df[
    hist_df$height > 0 & is.finite(hist_df$xmin) & is.finite(hist_df$xmax),
    , drop = FALSE
  ]
  hist_df$ymin_val <- Y_BOTTOM - hist_height
  hist_df$ymax_val <- Y_BOTTOM - hist_height + hist_df$height

  d_loess <- d[is.finite(d$x_plot) & is.finite(d$y_val), , drop = FALSE]
  x_right <- max(d$x_plot, na.rm = TRUE)
  x_left  <- min(d$x_plot, na.rm = TRUE)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x_plot, y = y_val,
                                       colour = .data[["plate"]])) +

    ggplot2::geom_rect(
      data = hist_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin_val, ymax = ymax_val),
      fill = "grey72", colour = "grey45",
      linewidth = 0.18, alpha = 0.70, inherit.aes = FALSE
    ) +

    ggplot2::geom_hline(yintercept = Y_BOTTOM, colour = "grey35", linewidth = 0.55) +

    ggplot2::geom_hline(yintercept = Y_REF, colour = "#2166ac",
                        linewidth = 0.55, linetype = "dashed") +

    { if (nrow(d_loess) >= 5L)
        ggplot2::geom_smooth(
          data    = d_loess,
          ggplot2::aes(x = x_plot, y = y_val, group = 1L),
          method  = "loess", formula = y ~ x, span = 0.65, se = TRUE,
          colour  = "grey15", fill = "grey75",
          linewidth = 0.70, alpha = 0.22,
          show.legend = FALSE, inherit.aes = FALSE
        )
      else list()
    } +

    ggplot2::geom_point(shape = 16L, size = point_size, alpha = 0.65,
                        stroke = 0.25, show.legend = FALSE) +

    # Black circle overlay: binary pcov_pass gate reference.
    # pcov_pass = TRUE  -> black circle at Y_REF    (in range)
    # pcov_pass = FALSE -> black circle at Y_BOTTOM (excluded)
    { if (has_gate) {
        gw_size <- point_size * 0.85
        d_in  <- d[!is.na(d$gate_val) & d$gate_val == 1L, , drop = FALSE]
        d_out <- d[!is.na(d$gate_val) & d$gate_val == 0L, , drop = FALSE]
        layers <- list()
        if (nrow(d_in) > 0L)
          layers <- c(layers, list(
            ggplot2::geom_point(data = d_in, ggplot2::aes(x = x_plot), y = Y_REF,
                                shape = 16L, colour = "black",
                                size = gw_size, alpha = 0.45, stroke = 0,
                                inherit.aes = FALSE, show.legend = FALSE)
          ))
        if (nrow(d_out) > 0L)
          layers <- c(layers, list(
            ggplot2::geom_point(data = d_out, ggplot2::aes(x = x_plot), y = Y_BOTTOM,
                                shape = 16L, colour = "black",
                                size = gw_size, alpha = 0.45, stroke = 0,
                                inherit.aes = FALSE, show.legend = FALSE)
          ))
        layers
      } else list()
    } +

    ggplot2::annotate("text", x = x_right, y = Y_TOP, label = phi_ann,
                      hjust = 1L, vjust = 1L, size = 2.6,
                      colour = "grey28", lineheight = 1.15) +

    ggplot2::annotate("text", x = x_left,
                      y = Y_REF + (Y_TOP - Y_BOTTOM) * 0.025,
                      label = "mean w_norm = 1",
                      hjust = 0L, vjust = 0L, size = 2.5, colour = "#2166ac") +

    ggplot2::scale_colour_manual(values = plate_cols, guide = "none") +
    x_scale +
    ggplot2::scale_y_continuous(
      limits = c(Y_BOTTOM - hist_height * 1.2, Y_TOP * 1.02),
      breaks = y_breaks, labels = y_labels, expand = ggplot2::expansion(0)
    ) +
    ggplot2::labs(title = panel_title, x = x_title, y = y_title) +
    .pwp_theme() +
    ggplot2::theme(
      panel.border        = ggplot2::element_blank(),
      panel.grid.major.x  = ggplot2::element_blank(),
      axis.line.x.bottom  = ggplot2::element_line(colour = "grey40", linewidth = 0.45),
      axis.line.y.left    = ggplot2::element_line(colour = "grey40", linewidth = 0.45),
      axis.title.x        = ggplot2::element_text(size = 10L, margin = ggplot2::margin(t = 3L)),
      axis.title.y        = ggplot2::element_text(size = 10L, angle = 90L)
    )

  if (!is_first_col)
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank())
  p
}

# ---- Public: one panel per antigen, wrapped into an ncol-wide grid --------

#' Precision-weight panel: one subplot per antigen, from the persisted
#' calib_weights/calib_weights_fit tables.
#'
#' Rows/columns: wraps `antigens` into a grid `ncol` columns wide (not an
#' antigen x source grid like the original precision_weight_panel_m16() --
#' an antigen's sources are combined within its ONE panel, coloured by
#' plate, with one phi/beta1 annotation line per contributing source).
#'
#' @param weights_df  fetch_weights_panel_data() result (antigen/feature/
#'   source/plate/curve_id/sampleid/w_norm/predicted_concentration/pcov_pass).
#' @param fit_df      fetch_weights_panel_fit() result (antigen/feature/
#'   source/multiplate_group_id/method/phi/beta1/n_eff/interpretation/...).
#' @param antigens    antigen values to include (one panel each).
#' @param features    optional, same length as `antigens`: a feature to pin
#'   that panel to (NA = combine all of that antigen's features, today's
#'   behavior). Lets one antigen carrying several analytes (e.g. a combined
#'   flow experiment) get one panel per analyte instead of pooling them.
#' @param ncol         panel grid width (default 3).
#' @param x_log        log10 x-axis? (default TRUE)
#' @param point_size   point size (default 2.0).
#' @return a patchwork figure.
precision_weight_panel <- function(weights_df, fit_df, antigens, features = NULL,
                                   ncol = 3L, x_log = TRUE, point_size = 2.0) {
  antigens <- as.character(antigens)
  n_ag <- length(antigens)
  if (n_ag == 0L) stop("precision_weight_panel: no antigens to plot.")
  if (is.null(features)) features <- rep(NA_character_, n_ag)
  features <- as.character(features)
  if (length(features) != n_ag)
    stop("precision_weight_panel: features must be the same length as antigens.")

  all_plates <- if (nrow(weights_df))
    .pwp_natural_sort_plates(unique(as.character(weights_df$plate))) else character(0)
  plate_cols <- if (length(all_plates)) .pwp_plate_palette(all_plates) else character(0)

  panels <- vector("list", n_ag)
  for (i in seq_len(n_ag)) {
    ag   <- antigens[i]
    feat <- features[i]
    ag_rows <- weights_df[weights_df$antigen == ag, , drop = FALSE]
    has_feat_col <- "feature" %in% names(ag_rows)
    d_ag <- if (!is.na(feat) && has_feat_col)
      ag_rows[ag_rows$feature == feat, , drop = FALSE] else ag_rows
    fit_ag_all <- if (nrow(fit_df)) fit_df[fit_df$antigen == ag, , drop = FALSE] else fit_df
    fit_ag <- if (!is.na(feat) && "feature" %in% names(fit_ag_all))
      fit_ag_all[fit_ag_all$feature == feat, , drop = FALSE] else fit_ag_all
    is_first <- ((i - 1L) %% ncol) == 0L
    # One subplot title per (antigen, feature) when the antigen actually
    # carries more than one feature in scope; bare antigen otherwise --
    # identical to today's title when every antigen has a single feature.
    ag_feature_count <- if (has_feat_col) length(unique(ag_rows$feature)) else 1L
    panel_title <- if (is.na(feat) || ag_feature_count <= 1L) toupper(ag)
                   else sprintf("%s / %s", toupper(feat), toupper(ag))

    if (nrow(d_ag) == 0L) {
      panels[[i]] <- ggplot2::ggplot() + ggplot2::theme_void() +
        ggplot2::annotate("text", x = .5, y = .5,
                          label = paste0("no weights data\n(", panel_title, ")"),
                          colour = "grey50", size = 3.5) +
        ggplot2::labs(title = panel_title)
      next
    }

    conc_ok <- d_ag$predicted_concentration[
      is.finite(d_ag$predicted_concentration) & d_ag$predicted_concentration > 0]
    x_lims <- if (length(conc_ok) >= 2L) range(conc_ok, na.rm = TRUE) else NULL

    panels[[i]] <- .build_antigen_weight_panel(
      d_antigen    = d_ag,
      fit_rows     = fit_ag,
      plate_cols   = plate_cols,
      panel_title  = panel_title,
      is_first_col = is_first,
      x_log        = isTRUE(x_log),
      x_limits     = x_lims,
      point_size   = point_size
    )
  }

  cap <- paste0(
    "Precision weights for ", n_ag, " target", if (n_ag > 1L) "s" else "",
    ". Model: sigma_i = phi * se_i^beta1 (curveRweights joint Bayesian/frequentist ",
    "location-scale fit); w_i = 1/sigma_i^2, normalised to mean = 1 (w_norm; dashed ",
    "blue line). Annotation per panel: one line per contributing standard curve ",
    "source -- phi = baseline precision scaling; beta1 = precision exponent ",
    "(near 1 = se is a direct proxy for residual SD, > 1 = amplified weighting, ",
    "< 1 = compressed); n_eff = effective sample size. Colour = plate.",
    "\n\n",
    "BLACK CIRCLE OVERLAY -- binary pcov_pass reference for direct comparison with ",
    "the old pass/fail gate: pcov_pass = TRUE -> black circle at y = 1 (mean line); ",
    "pcov_pass = FALSE -> black circle at axis floor (y = 0). The vertical position ",
    "of each coloured point relative to its black circle shows how the continuous ",
    "precision weight departs from the binary gate for that observation. ",
    "LOESS ribbon = concentration-weight trend (span = 0.65). Grey histogram = ",
    "sample concentration distribution (below axis floor)."
  )

  patchwork::wrap_plots(panels, ncol = ncol) +
    patchwork::plot_annotation(
      caption = cap,
      theme = ggplot2::theme(
        plot.caption = ggplot2::element_text(
          size = 9L, hjust = 0, lineheight = 1.45,
          colour = "grey20", margin = ggplot2::margin(t = 12L)
        )
      )
    )
}
