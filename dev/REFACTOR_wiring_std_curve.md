# Wiring the std-curve split into app.R (pure refactor, two sub-tabs)

The monolithic `std_curve_module.R` is replaced by two modules that behave
identically, arranged as two sub-tabs:

- `std_curve_view_module.R`  -> `stdCurveViewUI(id)` / `stdCurveViewServer(id, conn, scope, calib_dirty, selected_curve)`
- `std_curve_calc_module.R`  -> `stdCurveCalcUI(id)` / `stdCurveCalcServer(id, conn, api, scope, calib_dirty, selected_curve)`

They communicate through two shared reactiveVals created in the parent:
- `calib_dirty`  : bumped by calc when a job completes; view re-reads calib_* on change.
- `selected_curve`: view writes the current pick; reserved for a future heatmap click.

## 1. Sourcing (around the old lines 1118 / 1149)

REMOVE the dead parallel UI (this is the second `output$curve_plot` we kept
tripping over):
```r
-      source("std_curver_ui.R", local = TRUE)      # <- DELETE (retired)
```

REPLACE the single-module source:
```r
-      source("std_curve_module.R", local = TRUE)
+      source("std_curve_view_module.R", local = TRUE)
+      source("std_curve_calc_module.R", local = TRUE)
```
(Keep `std_curve_module.R` on disk until the split is verified; just stop
sourcing it. Delete it once the two sub-tabs are confirmed working.)

## 2. UI: where the Standard Curve tab is placed

Wherever `stdCurveModuleUI("std_curve")` was rendered in the QC / Standard Curve
tab, replace it with a two-sub-tab tabset:
```r
shiny::tabsetPanel(
  id = ns("std_curve_subtabs"),
  shiny::tabPanel("Explore fits", stdCurveViewUI(ns("sc_view"))),
  shiny::tabPanel("Compute fits", stdCurveCalcUI(ns("sc_calc")))
)
```
(If this block is not itself inside a module, drop the `ns(...)` wrapper and use
plain string ids: `stdCurveViewUI("sc_view")`, `stdCurveCalcUI("sc_calc")`, and
`tabsetPanel(id = "std_curve_subtabs", ...)`.)

## 3. Server: replace the single stdCurveServer(...) call

Old (≈ line 1168):
```r
-      stdCurveServer("std_curve", conn = db_pool, api = compute_api_client(),
-                     scope = <the scope reactive>)
```
New -- create the shared reactives, pass both to each module:
```r
      calib_dirty    <- shiny::reactiveVal(0)
      selected_curve <- shiny::reactiveVal(NULL)

      sc_scope <- <the SAME scope reactive as before>   # reactive(list(study, experiment, project_id))

      sc_view <- stdCurveViewServer("sc_view", conn = db_pool, scope = sc_scope,
                                    calib_dirty = calib_dirty, selected_curve = selected_curve)
      stdCurveCalcServer("sc_calc", conn = db_pool, api = compute_api_client(),
                         scope = sc_scope, calib_dirty = calib_dirty,
                         selected_curve = selected_curve)
```
`sc_view` still returns `list(curve_id, method, bundle)` -- if other consumers
(study overview, dilutional linearity) used the old module's return value, point
them at `sc_view` now.

## 4. Namespacing note
If the two-sub-tab UI is created inside an existing module, the `id`s you pass to
`stdCurveViewUI` / `stdCurveViewServer` etc. must match, and the UI ids must be
`ns("sc_view")` / `ns("sc_calc")` while the server ids are the bare `"sc_view"` /
`"sc_calc"` (Shiny's usual module id/ns pairing). Keep them consistent.

## 5. Verify (pure refactor -- behavior must be IDENTICAL)
- "Explore fits" tab: antigen/curve/method/model selectors populate; plot renders
  with overlays + legend + diagnostics block; tables tabs work. Same as before.
- "Compute fits" tab: models-to-fit / engine / scope / target controls; Submit
  posts a job; job status updates; calc-status table lists the experiment.
- Submit a job on "Compute fits", let it complete -> switch to "Explore fits" ->
  the new fit shows (calib_dirty bridged the two). This is the one cross-module
  behavior to confirm.
- No second/ghost curve plot (std_curver_ui.R retired).
