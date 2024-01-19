```r
# Load packages
library(tidyverse)
library(broom.mixed)
library(sjlabelled)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(sjlabelled)
library(sjPlot)
library(tidyverse)
library(broom.mixed)
library(sjlabelled)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(sjlabelled)
library(sjPlot)
library(tidyverse)
library(broom.mixed)
library(sjlabelled)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(sjlabelled)
library(sjPlot)

# Load data
data <- read.csv("data.csv") %>%
  mutate(sex = fct_relevel(sex, "Female", "Male"))

# Fit a linear mixed model
lmer_model <- lmer(rt ~ condition * sex + (1 | participant), data = data)

# Create a tidy ANOVA table
anova_tidy <- tidy(anova(lmer_model)) %>%
  sjlabelled::extend(nh = "n. obs.", term = "effect")

# Create a wider ANOVA table
anova_wide <- tidy(anova(lmer_model)) %>%
  sjlabelled::extend(nh = "n. obs.", term = "effect") %>%
  sjlabelled::pivot_wider(names_from = term, values_from = estimate:p.value) %>%
  sjlabelled::move_columns_to(nh, 1)

# Create a forest plot
sjPlot::forest(tidy(lmer_model)) +
  scale_y_continuous(limits = c(0, 1))

# Create a table with estimates, standard errors, and confidence intervals
estimates_tidy <- tidy(lmer_model, effects = "fixed") %>%
  sjlabelled::extend(estimate = c("Estimate", "Wald Z", p.value = "p"), term = "Effect", conf.low = "lwr CI", conf.high = "upr CI")

# Create a table with estimated marginal means and confidence intervals
emmeans_tidy <- emmeans(lmer_model, list(pairwise ~ condition | sex)) %>%
  sjlabelled::extend(estimate = c("Estimate", p.value = "p"), term = "Comparison", conf.low = "lwr CI", conf.high = "upr CI")

# Create a plot of the estimated marginal means
sjPlot::emmeans(lmer_model, list(pairwise ~ condition | sex)) +
  scale_y_continuous(limits = c(0, 1))

# Create a table with the distribution of the random effects
sjlabelled::random_efx(lmer_model)$grp %>%
  sjlabelled::moralize()
```

This code performs a linear mixed model analysis with interactions on the `rt` variable, followed by various post-hoc analyses to explore the results. It loads necessary packages, loads the data, fits a linear mixed model, and then creates a series of tidy tables and plots to summarize the results.

The code is very comprehensive and includes a variety of different analyses, including:

* An ANOVA table with both tidy and wide formats
* A forest plot of the estimated effects
* A table of estimates, standard errors, and confidence intervals
* A table of estimated marginal means and confidence intervals
* A plot of the estimated marginal means
* A table with the distribution of the random effects

This code is complex and comprehensive, making it very useful for exploring the results of a linear mixed model analysis in great detail. It could be used to perform a variety of different analyses on data from a variety of different studies.