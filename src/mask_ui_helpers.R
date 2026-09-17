# =============================================================================
# mask_ui_helpers.R  --  shared point-masking UI/logic.
#
# Every masking action in the app -- however the point was clicked -- resolves
# to ONE curve_id and reuses the SAME backend contract from calib_data_access.R
# (resolve_std_mask_ids / resolve_blk_mask_ids / curve_group_members /
# curve_ids_for_standards / curve_ids_for_blanks / calib_group_rowcounts /
# apply_mask / apply_unmask). This file holds the presentation layer that sits
# on top of that contract: the "Apply to:" scope radio, the "keep fits"
# deferred-recalc checkbox, the scope-aware dry-run plan builder + its two
# render bodies (mask / unmask), and the click + double-click JS shim that
# turns plotly point clicks into staged mask/unmask keys.
#
# Consumers (as of this writing):
#   * std_curve_view_module.R        (Explore fits -- one curve at a time)
#   * plate_dilution_series_module.R (Plate Dilution Series: Analytes tab --
#     many plates/antigens visible at once, but each staged batch is still
#     pinned to ONE curve_id; see that module's a_active_cid)
#
# NOTE: std_curve_view_module.R currently has its own inline copies of
# scope_input()/keep_fits_input()/build_change_plan() predating this file. It
# was deliberately left as-is when this file was introduced (avoiding touching
# a working, already-shipped module) rather than migrated onto these shared
# versions. The two are kept behaviorally identical by hand; migrating
# std_curve_view_module.R onto these shared helpers is a reasonable, low-risk
# follow-up cleanup, not done here.
#
# Mask/unmask KEY format expected by build_mask_change_plan(): "std|<well>|
# <dilution>" or "blk|<well>|". A caller whose plot needs a richer per-point
# identity (e.g. to disambiguate the same well across several plates/antigens
# shown at once) should keep its OWN, separate key format for on-plot
# click/toggle identity, and translate to this "std|well|dilution" shape only
# when it hands a staged set to build_mask_change_plan() -- do not widen this
# shared format, since resolve_std_mask_ids()/resolve_blk_mask_ids() (and
# parse_mask_key() below) match on segments 1 and 2 only, and both already pin
# their own scope via ONE curve_id.
# =============================================================================

parse_mask_key <- function(k) strsplit(k, "|", fixed = TRUE)[[1]]

# "Apply to:" scope radio, shared by mask and unmask modals in both modules.
# ns : the caller's session$ns.
mask_scope_input <- function(ns, id) {
  shiny::radioButtons(ns(id), "Apply to:",
    choices = c(
      "This feature/antigen only" = "antigen",
      "All features/antigens in this well (whole plate)" = "plate"),
    selected = "antigen")
}

# Deferred-recalculation checkbox, shared by mask and unmask modals. Unchecked
# (default) = delete the affected group's calib_* fits immediately (original
# behavior). Checked = keep the existing fits, flag the group stale instead
# (calib_recalc_flag, see calib_data_access.R), so several plates'/antigens'
# worth of masking can be staged before paying for one recompute.
mask_keep_fits_input <- function(ns, id) {
  shiny::tags$div(style = "margin-top:6px;",
    shiny::checkboxInput(ns(id),
      "Keep existing fits and mark them out of date instead of deleting now",
      value = FALSE),
    shiny::tags$div(style = "font-size:11px;color:#787878;",
      "Use this when staging masking changes across more than one plate or antigen. ",
      "Existing frequentist/Bayesian fits stay visible (marked stale) until ",
      "you submit a fit job on the Compute-fits tab; that recompute clears ",
      "the flag. Leave unchecked to delete the affected fits right away, as before."))
}

# Shared, scope-aware DRY-RUN plan builder for BOTH mask and unmask, and for
# every caller regardless of how points were selected. Pure (read-only):
# resolve staged wells -> xmap ids at the requested scope, then compute the
# calib_* delete/stale blast radius. In "antigen" scope the standard side
# stays within the viewed curve's multiplate group (original behavior); in
# "plate" scope every analyte's group the masked wells feed is included
# (curve_ids_for_standards / curve_ids_for_blanks fan-outs). The viewed group
# is always a floor.
#
# sel   : character vector of keys, EXACTLY "std|well|dilution" or "blk|well|"
#         (see the file header note above -- widen your own click-identity
#         key format if needed, but translate to this shape before calling).
# cid   : the ONE curve_id every key in `sel` is resolved against.
# scope : "antigen" (default) or "plate".
build_mask_change_plan <- function(pool, sel, cid, scope = "antigen") {
  if (!length(sel) || !shiny::isTruthy(cid)) return(NULL)
  scope <- if (identical(scope, "plate")) "plate" else "antigen"
  keys <- lapply(sel, parse_mask_key)
  is_blk <- vapply(keys, function(p) identical(p[1], "blk"), logical(1))
  std_w <- vapply(keys[!is_blk], function(p) p[2], character(1))
  std_d <- vapply(keys[!is_blk], function(p) if (length(p) > 2) p[3] else "", character(1))
  blk_w <- vapply(keys[is_blk],  function(p) p[2], character(1))
  std_ids <- tryCatch(resolve_std_mask_ids(pool, cid, std_w, std_d, scope = scope),
                      error = function(e) integer(0))
  blk_ids <- tryCatch(resolve_blk_mask_ids(pool, cid, blk_w, scope = scope),
                      error = function(e) integer(0))
  grp <- tryCatch(curve_group_members(pool, cid), error = function(e) integer(0))  # floor
  # Plate scope: standards now span analytes, so every group each masked
  # standard row feeds is invalidated (antigen scope keeps the viewed group).
  if (identical(scope, "plate") && length(std_ids))
    grp <- c(grp, tryCatch(curve_ids_for_standards(pool, std_ids), error = function(e) integer(0)))
  # A masked blank invalidates EVERY group it feeds (source-less fan-out) in
  # BOTH scopes; plate scope also fanned it across analytes via the resolver.
  if (length(blk_ids))
    grp <- c(grp, tryCatch(curve_ids_for_blanks(pool, blk_ids), error = function(e) integer(0)))
  grp <- sort(unique(grp))
  counts <- tryCatch(calib_group_rowcounts(pool, grp), error = function(e) integer(0))
  list(std_ids = std_ids, blk_ids = blk_ids, grp = grp, counts = counts,
       n_std = length(std_w), n_blk = length(blk_w), scope = scope)
}

# Shared dry-run body for the MASK modal (the "This will: ..." bullet list +
# scope banner + keep/delete note). `pl` is a build_mask_change_plan() result;
# `keep` is the current state of the "keep fits" checkbox.
mask_dryrun_body <- function(pl, keep) {
  total_del <- sum(pl$counts)
  scope_banner <- if (identical(pl$scope, "plate"))
    shiny::tags$div(style = "color:#8a6d3b;background:#fcf8e3;border:1px solid #faebcc;padding:4px 6px;font-size:12px;",
      "Whole-plate scope: this masks the staged well(s) for EVERY feature/antigen on this plate ",
      "(same project/study/experiment, plateid, nominal dilution, source, wavelength), not just the viewed one.")
  else NULL
  fit_action_li <- if (keep)
    shiny::tags$li(sprintf(
      "KEEP the existing calib_* fits for %d affected curve%s (all groups touched, incl. every group a masked blank feeds: %d row(s)) and mark %s stale, pending recalculation",
      length(pl$grp), if (length(pl$grp) == 1) "" else "s", total_del,
      if (length(pl$grp) == 1) "it" else "them"))
  else
    shiny::tags$li(sprintf(
      "DELETE all calib_* fits for %d affected curve%s (all groups touched, incl. every group a masked blank feeds): %d row(s) total",
      length(pl$grp), if (length(pl$grp) == 1) "" else "s", total_del))
  shiny::tagList(
    shiny::tags$hr(),
    scope_banner,
    shiny::tags$strong("This will:"),
    shiny::tags$ul(
      shiny::tags$li(sprintf("set masked = true on %d standard row(s) [xmap_standard_id: %s]",
        length(pl$std_ids), paste(pl$std_ids, collapse = ", "))),
      shiny::tags$li(sprintf("set masked = true on %d blank row(s) [xmap_buffer_id: %s]",
        length(pl$blk_ids), paste(pl$blk_ids, collapse = ", "))),
      fit_action_li),
    if (length(pl$counts))
      shiny::tags$div(style = "font-size:11px;color:#787878;",
        paste(sprintf("%s: %d", names(pl$counts), as.integer(pl$counts)), collapse = "  \u00b7  ")),
    if ((length(pl$std_ids) + length(pl$blk_ids)) != (pl$n_std + pl$n_blk))
      shiny::tags$div(style = "color:#B2182B;",
        "\u26a0 Some staged points did not resolve to a unique row \u2014 review before applying."),
    shiny::tags$div(style = "margin-top:6px;color:#555;",
      if (keep)
        shiny::tags$em("The plot will keep showing the ", shiny::tags$b("existing"), " fit, ",
                       "ringed in red and labelled ", shiny::tags$b("out of date"),
                       ", until you submit a fit job (any scope covering this curve) on the Compute-fits tab.")
      else
        shiny::tags$em("After applying, this curve's fit is removed and its group shows ",
                       shiny::tags$b("needs calculation"),
                       ". Recompute on the Compute-fits tab to get a revised fit.")))
}

# Same shape for the UNMASK modal (no "reason" line involved; different
# closing note about points being restored rather than removed).
mask_dryrun_body_unmask <- function(pl, keep) {
  total_del <- sum(pl$counts)
  scope_banner <- if (identical(pl$scope, "plate"))
    shiny::tags$div(style = "color:#8a6d3b;background:#fcf8e3;border:1px solid #faebcc;padding:4px 6px;font-size:12px;",
      "Whole-plate scope: this unmasks the staged well(s) for EVERY feature/antigen on this plate ",
      "(same project/study/experiment, plateid, nominal dilution, source, wavelength), not just the viewed one.")
  else NULL
  fit_action_li <- if (keep)
    shiny::tags$li(sprintf(
      "KEEP the existing calib_* fits for %d affected curve%s (all groups touched, incl. every group an unmasked blank feeds: %d row(s)) and mark %s stale, pending recalculation",
      length(pl$grp), if (length(pl$grp) == 1) "" else "s", total_del,
      if (length(pl$grp) == 1) "it" else "them"))
  else
    shiny::tags$li(sprintf(
      "DELETE all calib_* fits for %d affected curve%s (all groups touched, incl. every group an unmasked blank feeds): %d row(s) total",
      length(pl$grp), if (length(pl$grp) == 1) "" else "s", total_del))
  shiny::tagList(
    shiny::tags$hr(),
    scope_banner,
    shiny::tags$strong("This will:"),
    shiny::tags$ul(
      shiny::tags$li(sprintf("set masked = false and clear the mask reason on %d standard row(s) [xmap_standard_id: %s]",
        length(pl$std_ids), paste(pl$std_ids, collapse = ", "))),
      shiny::tags$li(sprintf("set masked = false and clear the mask reason on %d blank row(s) [xmap_buffer_id: %s]",
        length(pl$blk_ids), paste(pl$blk_ids, collapse = ", "))),
      fit_action_li),
    if (length(pl$counts))
      shiny::tags$div(style = "font-size:11px;color:#787878;",
        paste(sprintf("%s: %d", names(pl$counts), as.integer(pl$counts)), collapse = "  \u00b7  ")),
    if ((length(pl$std_ids) + length(pl$blk_ids)) != (pl$n_std + pl$n_blk))
      shiny::tags$div(style = "color:#B2182B;",
        "\u26a0 Some staged points did not resolve to a unique row \u2014 review before applying."),
    shiny::tags$div(style = "margin-top:6px;color:#555;",
      if (keep)
        shiny::tags$em("The plot will keep showing the ", shiny::tags$b("existing"), " fit, ",
                       "ringed in red and labelled ", shiny::tags$b("out of date"),
                       ", until you submit a fit job (any scope covering this curve) on the Compute-fits tab.")
      else
        shiny::tags$em("After applying, this curve's fit is removed and its group shows ",
                       shiny::tags$b("needs calculation"),
                       ". Recompute on the Compute-fits tab to get a fit with the points restored.")))
}

# Attach the shared click/double-click identification shim to a plotly object
# whose traces were built with an explicit `customdata` per marker (see
# std_curve_view_module.R's curve_plot for the canonical pattern this
# mirrors). plotly emits no per-point double-click event, so we detect it
# ourselves: pair two plotly_click events on the SAME point within 500 ms and
# push that point's customdata to `dblclick_input_id`. Single clicks are read
# the ordinary way by the caller, via plotly::event_data("plotly_click",
# source = ...). doubleClick = FALSE stops plotly's own double-click
# axis-reset from firing alongside our double-click gesture.
#
# `p` must already have been through plotly::event_register(p, "plotly_click")
# is NOT required of the caller -- this function does it, so callers should
# NOT also call event_register() themselves (double-registering is harmless
# but redundant).
attach_mask_click_shim <- function(p, dblclick_input_id) {
  p <- plotly::config(p, doubleClick = FALSE)
  p <- plotly::event_register(p, "plotly_click")
  htmlwidgets::onRender(p, "
    function(el, x, inputId) {
      var lastKey = null, lastT = 0;
      el.on('plotly_click', function(d) {
        if (!d || !d.points || !d.points.length) return;
        var cd = d.points[0].customdata;
        if (cd === undefined || cd === null) return;
        var now = Date.now();
        if (cd === lastKey && (now - lastT) < 500) {
          Shiny.setInputValue(inputId, {key: cd, nonce: now}, {priority: 'event'});
          lastKey = null; lastT = 0;
        } else { lastKey = cd; lastT = now; }
      });
    }", data = dblclick_input_id)
}
