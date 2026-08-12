# =============================================================================
# study_overview_ui.R  --  RETIRED (logic moved to study_overview.R module)
# -----------------------------------------------------------------------------
# The former contents were one large observeEvent(input$study_level_tabs) that
# eagerly ran preprocess_plate_data() -- every pull_* query, including the stale
# best_* fit paths -- the moment the Study Overview tab opened. That eager load
# is what crashed the whole app.
#
# Study Overview is now the lazy module in study_overview.R
# (studyOverviewUI / studyOverviewServer), mounted on the main-sidebar
# "study_overview" tab. Each view loads on demand; nothing touches fit data
# until the fit pane is opened.
#
# This file is intentionally a no-op so any existing source("study_overview_ui.R")
# call stays valid during migration; drop it from the source list once app.R no
# longer references it.
# =============================================================================

invisible(NULL)
