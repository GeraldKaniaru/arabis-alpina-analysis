Gaussian models are benchmarks; sensitivity fits do not overwrite publication estimates.
All models use 581 accessions, the same elevation scaling, and block/source-plot random intercepts.
Ordinal cumulative-logit fits respect the observed ordered daily categories and avoid continuous normal errors; proportional odds is an additional assumption.
Positive ordinal coefficients indicate a shift towards later first germination or wider time spread.
GP uses germinated/ungerminated counts out of 100; beta-binomial allows extra-binomial variation.
MGR uses a Gamma distribution with a log link; confidence intervals and P values are two-sided Wald approximations.
Compare AIC only between binomial and beta-binomial GP fits, not across continuous and ordinal likelihoods.
Report convergence, Hessian and boundary variance flags; singularity is not evidence that a response family is unsuitable.
DHARMa checks use 1000 simulations with seed 12191; P < 0.05 flags a diagnostic departure.
Ordinal models have no DHARMa checks here; converged ordinal fits alone do not establish proportional-odds adequacy.
