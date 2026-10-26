# Updated bootstrap text after resolving seed-number model stability

The previous provisional seed-number statement is superseded by the validated analysis described in Seed_number_model_stability_resolution_20261025.md.

For seed-number projections, the initial NB2 model with nested block and plot random intercepts estimated both variance components below 10^-8 and produced non-finite fixed-effect standard errors. We therefore refitted the model without these negligible random-intercept components, retaining the NB2 distribution, log link and the same standardised elevation, soil PC1, temperature and precipitation predictors. The reduced model converged with a positive-definite Hessian and finite fixed-effect standard errors. We assessed residual uniformity, dispersion and outliers using 1,000 DHARMa simulations and repeated the checks after aggregating residuals by block and source plot. These checks detected no departures at P < 0.05. The Gaussian projection models retained their original random-effects structures.

All 1,000 seed-number bootstrap refits converged, had positive-definite Hessians and finite fixed-effect standard errors, and produced finite baseline and future predictions. No refitting errors or warnings were recorded. All 1,000 replicates were retained, and the 2.5th and 97.5th percentiles of mean changes defined the 95% percentile bootstrap intervals. These intervals are conditional on the selected NB2 model structure and specified climate projections.

Removing the negligible random effects changed scenario mean projections by less than 0.00004 seeds per plant. Mean changes were -6.27 seeds per plant under SSP2-4.5 (95% percentile interval -41.18 to 49.80) and -11.08 under SSP5-8.5 (-67.66 to 73.06). Both intervals included zero. The numerical instability identified in the original seed-number projection model was resolved under the reduced structure.

The separate diagnostics for the Gaussian traits remain applicable; see Table_bootstrap_refit_diagnostic_summary.csv.
