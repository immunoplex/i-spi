# Precision-gap diagnostic
working dir = C:/Users/d78039e/Documents/R-git/i-spi-refactor
OUT = C:/Users/d78039e/Documents/R-git/i-spi-refactor/REPORT_precision_gap.md
CURVE = 88979

Guide: both ~0 -> agree; different pairing ~0 -> H1 definition;
x-sloped -> H2 alignment; tracks roughness (bayesian only) -> MC ratio-bias.

### method = frequentist
grid points: 117   samples (with valid conc & pcov): 70
residuals (sample - grid@x):
  pcov - grid.pcov         mean=+0.078 sd=0.91 abs_mean_over_sd=0.09
  pcov - grid.pcovRmse     mean=+0.078 sd=0.91 abs_mean_over_sd=0.09
  pcovRmse - grid.pcov     mean=+0.078 sd=0.91 abs_mean_over_sd=0.09
H2 x-slope: slope=-0.242 p=0.0887  -> flat (alignment OK)
MC signature: cor(roughness,gap)=0.22  -> not roughness (definition/alignment)

### method = bayesian
grid points: 200   samples (with valid conc & pcov): 71
residuals (sample - grid@x):
  pcov - grid.pcov         mean=-9.630 sd=15.48 abs_mean_over_sd=0.62
  pcov - grid.pcovRmse     mean=-12.770 sd=16.32 abs_mean_over_sd=0.78
  pcovRmse - grid.pcov     mean=-9.630 sd=15.48 abs_mean_over_sd=0.62
H2 x-slope: slope=+8.631 p=6.5e-06  -> x-correlated (alignment suspect)
MC signature: cor(roughness,gap)=-0.27  -> not roughness (definition/alignment)
