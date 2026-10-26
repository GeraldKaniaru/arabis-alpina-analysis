Diagnostic-only fits made directly from current workbooks: H1-H4 environmental candidates, H3 duration/final-date and within/among models, H7 and H8. Production model files are not created; run scripts 06-10 for publication analyses.
Raw-response comparisons reverse the existing natural-log response and refit the same model on the same plants.
Conditional residuals assess within-plant response errors after the fitted random effects; histogram shape alone does not justify transforming the response.
Shapiro-Wilk P values are descriptive: estimated mixed-model residuals are not independent observations and large samples detect small deviations.
Flags use |skewness| > 1, |Spearman correlation of absolute residual with fitted| > 0.3, or >1% residuals beyond 3 residual SD. These are review screens, not automatic decision rules.
Review Q-Q tails, scale-location, influential records, convergence and singularity together. Do not compare raw/log AIC without accounting for the transformation Jacobian.
No production model, manuscript or existing figure is changed by this script. Gaussian glmmTMB models cause a stop rather than silent omission; non-Gaussian models are outside its scope.
