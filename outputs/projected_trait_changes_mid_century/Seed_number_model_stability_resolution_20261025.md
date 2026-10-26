# Seed-number projection model stability: resolved

## Methods replacement

For seed-number projections, the initial NB2 model with nested block and plot random intercepts estimated both variance components below 10^-8 and produced non-finite fixed-effect standard errors. We therefore refitted the model without these negligible random-intercept components, retaining the NB2 distribution, log link and the same standardised elevation, soil PC1, temperature and precipitation predictors. The reduced model converged with a positive-definite Hessian and finite fixed-effect standard errors. We assessed residual uniformity, dispersion and outliers using 1,000 DHARMa simulations and repeated the checks after aggregating residuals by block and source plot. These checks detected no departures at P < 0.05. The Gaussian projection models retained their original random-effects structures.

## Bootstrap replacement

All 1,000 seed-number bootstrap refits converged, had positive-definite Hessians and finite fixed-effect standard errors, and produced finite baseline and future predictions. No refitting errors or warnings were recorded. All 1,000 replicates were retained, and the 2.5th and 97.5th percentiles of mean changes defined the 95% percentile bootstrap intervals. These intervals are conditional on the selected NB2 model structure and specified climate projections.

## Results

Removing the negligible random effects changed scenario mean projections by less than 0.00004 seeds per plant. Mean changes were -6.27 seeds per plant under SSP2-4.5 (95% percentile interval -41.18 to 49.80) and -11.08 under SSP5-8.5 (-67.66 to 73.06). Both intervals included zero. The numerical instability identified in the original seed-number projection model was resolved under the reduced structure.

## Validation and scope

- All 581 plants and all four environmental fixed effects were retained.
- The change applies to the seed-number climate-projection model in script 10; it does not alter the primary H5 analysis or the other projection models.
- Original nested-model log likelihood: -3395.8306292; reduced-model log likelihood: -3395.8306288. Fixed-effect coefficients differed by less than 0.0000002.
- Maximum absolute difference in individual baseline/future predictions: 0.000125 seeds per plant.
- Seed-number bootstrap: 1,000 attempted, 1,000 retained, 1,000 numerical passes; zero warnings, errors, convergence flags, Hessian flags or non-finite standard errors.
- No random intercepts remain in the seed-number model, so singularity of random effects is no longer applicable to that model.
- Current seed residual diagnostic P values: uniformity 0.307, dispersion 0.792, outliers 0.636; block-aggregated uniformity 0.809 and dispersion 0.234; plot-aggregated uniformity 0.936 and dispersion 0.848. Absence of significant departures does not establish model truth or prove zero residual clustering.
- Table 3, its spreadsheet and formatted exports, the combined table workbook, and the descriptive/numbered projection figures were refreshed from validated bootstrap draws.
- The earlier four-trait diagnostic run remains archived in data/migration_records/projections_before_seed_stability_20261025. It is superseded for the current seed-number projection model.
- Gaussian bootstrap diagnostics are unchanged: onset has one convergence flag and duration two; their singular-fit counts remain reported separately. The clean seed-number result does not imply every refit for every trait passed every check.
- Equal weighting of three GCMs estimates an ensemble mean. These bootstrap intervals do not incorporate all between-GCM, scenario, random-structure-selection or ecological uncertainty.

## Evidence files

- tables/Table_seed_number_projection_structure_resolution.csv
- tables/Table_seed_number_stability_prediction_comparison.csv
- tables/Table_seed_number_stability_residual_checks.csv
- tables/Table_bootstrap_refit_diagnostic_summary.csv
- tables/Table_bootstrap_refit_log_all_traits.csv
- tables/Table_3.csv and Table_3.xlsx
- models/Models_RQ6_full_climate_trait_models.rds
- models/Bootstrap_RQ6_change_draws.rds
- figures/Figure_S7_bootstrap_trait_changes.png
