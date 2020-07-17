# `ThermalSampleR` package

This repo contains the source code and skeleton R code for a package I am currently developing to calculate sample sizes required for accurate and precise estimates of critical thermal limits (e.g. CTmin/CTmax) for biological organisms. 

The core functionality of `ThermalSampleR` revolves around:
- 1. Bootstrap resampling of raw data 
- 2. Calculate population parameter of interest (e.g. mean) and confidence intervals across bootstrap samples.
- 3. Calculate distribution of population parameter of interest. 
- 4. Wrapper functions to assist in plotting and making inferences from these simulations. 

`ThermalSampleR` is still under development. This is my first attempt at writing an `R` package, so any input or advice would be much appreciated, particularly with regards to package development. 

Please feel free to contact me (Guy Sutton) at g.sutton@ru.ac.za 

# Example usage 

# Load raw data of CTmin's for Coreid 

# Download a separate package containing all the raw CTL data
# install_github("guysutton/CBCdata")
````
library(CBCdata) 
```

# Take a peek at the data 
```
head(coreid_data) 
```

# Perform bootstrap resampling 
```
sims <- boot_two_groups(data = coreid_data,
                        groups_col = col,
                        response = response,
                        group1 = "Catorhintha schaffneri_APM",
                        group2 = "Catorhintha schaffneri_NPM",
                        n_max = 49,
                        iter = 99)
```

# Plot differences between adult and nymph Catorintha schaffneri 
```
(plots <- plot_two_groups(x = sims,
                         n_min = 3,
                         n_max = 49))
``` 
