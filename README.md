# TerraclimateR

The terraclimate data set[^1] provides data on 14 climatic variables through time since 1958 at ~4km resolution in space and monthly resolution in time. 
TerraclimateR is a lightweight package that implements a single function (`get_terraclim`) to access these data and import them directly into R in long format.

<h3>General information</h3>
https://www.climatologylab.org/terraclimate.html

<h3>Installation</h3>

```
devtools::install_github("julianselke/TerraclimateR", build_vignettes = TRUE)
# or
remotes::install_github("julianselke/TerraclimateR", build_vignettes = TRUE)

library(TerraclimateR)

vignette("TerraclimateR")

?get_terraclim
```

[^1]: Abatzoglou, J., Dobrowski, S., Parks, S. et al. TerraClimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958–2015. Sci Data 5, 170191 (2018). https://doi.org/10.1038/sdata.2017.191
