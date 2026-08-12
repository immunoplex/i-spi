# Understanding the Bayesian precision profile and the measurement-error switch

This note explains what the precision profile is, the two variance definitions
the worker can now compute, why measurement (assay) error is genuinely hard to
estimate, and how to choose. It is written for both the lab and the developers
wiring the new control through the stack.

---

## 1. What the precision profile is

For a calibration curve `y = f(x)` (response `y`, `x = log10 concentration`), the
**precision profile** is the coefficient of variation of the *back-calculated
concentration* plotted against concentration: pcov(x) = %CV of x̂ where a sample
reading `r` is converted to a concentration by inverting the curve, x̂ = f⁻¹(r).
It is the curve the app draws in the Precision tab, and it defines LLOQ/ULOQ as
the concentrations where pcov crosses the precision budget (default 20%).

A geometric identity follows directly: a test sample whose reading back-calculates
to x̂ must carry (up to scatter) the pcov the profile predicts at x̂. **Plotted
together, the sample cloud must lie on the profile.** This is the check
`diagnose_precision_gap.R` performs, and it is the acceptance criterion for the
fix described below.

## 2. Two variance definitions — and why they must be applied consistently

Back-calculation precision has two distinct sources, and by the delta method they
combine (approximately) as

```
                Var_param( f(x) )   +   sigma^2(x)
Var( x_hat ) ≈ ───────────────────────────────────
                          f'(x)^2
```

- **Var_param( f(x) )** — uncertainty in the *calibration curve itself* (the fitted
  parameters). In the Bayesian fit this is the spread of the posterior over
  a, b, c, d (, g).
- **sigma²(x)** — the *measurement / assay noise*: the variability of a single
  response reading at a given concentration. In immunoassay this is typically
  **not constant** — it grows with signal level — which is why the profile is
  U-shaped and asymmetric rather than flat (O'Connell, Belanger & Haaland 1993).
- **f'(x)** — the local slope of the curve; dividing by it is what turns response
  variability into concentration variability and makes the profile blow up toward
  the flat asymptotes.

The precision profile can legitimately be computed with **both** terms
(measurement precision) or with the **first term only** (curve precision). Either
is valid — but the grid and the per-sample pcov must use the **same** definition,
or the sample cloud will not lie on the profile. The defect this work fixes was
exactly that inconsistency: the grid included sigma²(x) while the per-sample pcov
did not, so samples floated ~9.6 %CV below the profile with a slope that grew
toward the upper asymptote (where f'(x) is small and the missing sigma²/f'² term
is largest).

The refactor introduces **one switch, `include_measurement_error`**, threaded
identically into the grid and the sample paths, so they can never diverge again.

| `include_measurement_error` | `noise_mode` written | Var included | Interpretation |
|---|---|---|---|
| `TRUE` (default) | `measurement_homoscedastic` / `measurement_heteroscedastic` | param + sigma²(x) | Classical assay precision profile: precision of a concentration recovered from a single reading. |
| `FALSE` | `curve_only` | param only | Calibration-curve uncertainty alone; a lower bound on real-world precision. |

When measurement error is included, the *shape* of sigma²(x) is chosen by the
existing `use_heteroscedastic_noise` flag (constant `sigma_obs` vs the
power-of-mean form `sigma_i = exp(log_sigma0 + log_sigma_slope·log|mu_i|)`), which
is the standard heteroscedastic response-error relationship (RER) for immunoassay
(O'Connell, Belanger & Haaland 1993; Davidian & Giltinan 1995).

## 3. Why we default to *including* measurement error

The measurement-inclusive profile is the classical immunoassay precision profile
(O'Connell, Belanger & Haaland 1993; AAPS consensus, Azadeh et al. 2018). LLOQ and
ULOQ are meaningful only against a profile that reflects how precisely a real,
noisy reading pins down a concentration — i.e. one that includes sigma²(x). A
curve-only profile will report a spuriously optimistic usable range because it
omits the dominant real-world error term. So the default is `TRUE`.

## 4. Why the user may need to turn it *off* — measurement error is hard to estimate

The catch is that sigma²(x) — and especially its concentration dependence, the RER
— is **difficult to estimate well**, and a poorly estimated variance function does
real damage to the profile:

- Belanger, Davidian & Giltinan (1996) showed for immunoassay 4PL calibration that
  interval inference **"depend[s] critically on the quality with which the variance
  parameters are estimated,"** and warned that **"the common practice of setting
  variance parameters to fixed values without adequate investigation may lead to
  erroneous calibration inference."** The variance function is not a nuisance you
  can hand-wave; it drives the profile and the LOQs.
- Estimating a response-error relationship needs **replication across the signal
  range** — several standards, ideally replicated, and QC/control wells spanning
  low-to-high response. Variance-function estimation theory (Davidian & Carroll
  1987; Carroll & Ruppert 1988) is explicit that the variance parameters are
  estimated far less precisely than the mean-curve parameters and need adequate
  replication to be trustworthy.
- On a plate with only a handful of standard points and no replication, the fitted
  `sigma_obs` (or `log_sigma0`, `log_sigma_slope`) is a **crude estimate**. The
  measurement term it produces can then dominate a precision profile that is itself
  largely guesswork about the noise — a confident-looking curve resting on a shaky
  variance estimate.

In that low-information regime some users legitimately prefer the **curve-only**
profile: it reports the part of the precision the data *can* support (the posterior
curve uncertainty) without asserting a measurement-error structure the plate cannot
justify. It is an honest lower bound, and the difference between the two profiles
is itself diagnostic — a large gap means the reported precision is being driven by
a weakly-identified noise model, which is a signal to add standards/controls rather
than to trust either number blindly.

**The switch is not a substitute for data.** The durable fix for an untrustworthy
measurement term is more standards and replicated controls per plate spanning the
response range (Belanger, Davidian & Giltinan 1996; Higgins, Davidian, Chew & Burge
1998, on how upstream assay error propagates into calibration inference). The
switch lets an analyst choose the honest presentation for the data they have; it
does not manufacture precision.

## 5. How this maps to the code and the request

- `curveRbayes::fit_calibration_bayes(..., include_measurement_error = TRUE)` — new
  argument, default `TRUE`, passed unchanged into both `predict_grid_bayes()` and
  `predict_samples_bayes()`.
- The worker (`worker_curveR.R`) forwards a CLI flag; the API/queue carry it as a
  job parameter; the i-spi app exposes it as a per-fit choice. See
  `PLAN_noise_mode_api_worker.md` and `HANDOFF_ispi_app_noise_mode.md`.
- Persisted `calib_grid.noise_mode` / `calib_samples.noise_mode` record which
  definition produced a stored profile, so a plot is always interpretable after the
  fact.

## 6. References (verified)

- O'Connell MA, Belanger BA, Haaland PD. **Calibration and assay development using
  the four-parameter logistic model.** *Chemometrics and Intelligent Laboratory
  Systems* 1993; 20(2): 97–114. doi:10.1016/0169-7439(93)80008-6. — Defines the
  precision profile, MDC, RDL, LOQ, and the response-error relationship for 4PL
  calibration.
- Belanger BA, Davidian M, Giltinan DM. **The effect of variance function estimation
  on nonlinear calibration inference in immunoassay data.** *Biometrics* 1996;
  52(1): 158–175. PMID 8934590. — Interval inference depends critically on
  variance-function quality; fixing variance parameters without investigation can
  give erroneous inference.
- Higgins KM, Davidian M, Chew G, Burge H. **The effect of serial dilution error on
  calibration inference in immunoassay.** *Biometrics* 1998; 54(1): 19–32.
  PMID 9544505.
- Davidian M, Carroll RJ. **Variance function estimation.** *Journal of the American
  Statistical Association* 1987; 82(400): 1079–1091.
- Carroll RJ, Ruppert D. **Transformation and Weighting in Regression.** Chapman &
  Hall, 1988.
- Davidian M, Giltinan DM. **Nonlinear Models for Repeated Measurement Data.**
  Chapman & Hall, 1995. — Heteroscedastic/power-of-mean variance models.
- Gottschalk PG, Dunn JR. **The five-parameter logistic: a characterization and
  comparison with the four-parameter logistic.** *Analytical Biochemistry* 2005;
  343(1): 54–65. doi:10.1016/j.ab.2005.04.035.
- Rodbard D, Munson PJ, De Lean A. (five-parameter logistic; origin of the 5PL used
  here), 1974.
- Azadeh M, et al. **Calibration Curves in Quantitative Ligand Binding Assays:
  Recommendations and Best Practices…** *The AAPS Journal* 2018; 20: 22.
  doi:10.1208/s12248-017-0159-4. — Practical consensus on calibration curves,
  precision profiles, and LOQs.

> Code note: `predict_bayes.R` comments refer to the measurement-inclusive profile
> as "O'Malley (2008) CDAN" (concentration-dependent analytical noise). That label
> connects the code to the heteroscedastic-RER concept above; I was not able to
> independently verify a specific 2008 O'Malley citation, so this document anchors
> the concept to the confirmed sources rather than that internal attribution. If
> you have the original O'Malley reference, add it here.
