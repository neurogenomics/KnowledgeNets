KGExplorer
================
<img src='https://github.com/neurogenomics/KGExplorer/raw/master/inst/hex/hex.png' title='Hex sticker for KGExplorer' height='300'><br>
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/badge/devel%20version-0.99.05-black.svg)](https://github.com/neurogenomics/KGExplorer)
[![](https://img.shields.io/github/languages/code-size/neurogenomics/KGExplorer.svg)](https://github.com/neurogenomics/KGExplorer)
[![](https://img.shields.io/github/last-commit/neurogenomics/KGExplorer.svg)](https://github.com/neurogenomics/KGExplorer/commits/master)
<br> [![R build
status](https://github.com/neurogenomics/KGExplorer/workflows/rworkflows/badge.svg)](https://github.com/neurogenomics/KGExplorer/actions)
[![](https://codecov.io/gh/neurogenomics/KGExplorer/branch/master/graph/badge.svg)](https://app.codecov.io/gh/neurogenomics/KGExplorer)
<br>
<a href='https://app.codecov.io/gh/neurogenomics/KGExplorer/tree/master' target='_blank'><img src='https://codecov.io/gh/neurogenomics/KGExplorer/branch/master/graphs/icicle.svg' title='Codecov icicle graph' width='200' height='50' style='vertical-align: top;'></a>  
<h4>  
Authors: <i>Brian Schilder, Hiranyamaya Dash</i>  
</h4>
<h4>  
README updated: <i>Dec-18-2024</i>  
</h4>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `KGExplorer`: Biomedical Knowledge Network Construction and Analysis

### Query, construct, and analyse large-scale biomedical knowledge graphs and ontologies.

If you use `KGExplorer`, please cite:

<!-- Modify this by editing the file: inst/CITATION  -->

> Kitty B. Murphy, Robert Gordon-Smith, Jai Chapman, Momoko Otani, Brian
> M. Schilder, Nathan G. Skene (2023) Identification of cell
> type-specific gene targets underlying thousands of rare diseases and
> subtraits. medRxiv, <https://doi.org/10.1101/2023.02.13.23285820>

## Installation

``` r
if(!require("BiocManager")) install.packages("BiocManager")

BiocManager::install("neurogenomics/KGExplorer")
library(KGExplorer)
```

## Documentation

### [Website](https://neurogenomics.github.io/KGExplorer)

### [Getting started](https://neurogenomics.github.io/KGExplorer/articles/KGExplorer)

<hr>

## Session Info

<details>

``` r
utils::sessionInfo()
```

    ## R version 4.4.2 (2024-10-31)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS Sequoia 15.2
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6        jsonlite_1.8.9      renv_1.0.11        
    ##  [4] dplyr_1.1.4         compiler_4.4.2      BiocManager_1.30.25
    ##  [7] tidyselect_1.2.1    rvcheck_0.2.1       scales_1.3.0       
    ## [10] yaml_2.3.10         fastmap_1.2.0       here_1.0.1         
    ## [13] ggplot2_3.5.1       R6_2.5.1            generics_0.1.3     
    ## [16] knitr_1.49          yulab.utils_0.1.8   tibble_3.2.1       
    ## [19] desc_1.4.3          dlstats_0.1.7       rprojroot_2.0.4    
    ## [22] munsell_0.5.1       pillar_1.9.0        RColorBrewer_1.1-3 
    ## [25] rlang_1.1.4         utf8_1.2.4          badger_0.2.4       
    ## [28] xfun_0.49           fs_1.6.5            cli_3.6.3          
    ## [31] magrittr_2.0.3      rworkflows_1.0.3    digest_0.6.37      
    ## [34] grid_4.4.2          rstudioapi_0.17.1   lifecycle_1.0.4    
    ## [37] vctrs_0.6.5         evaluate_1.0.1      glue_1.8.0         
    ## [40] data.table_1.16.2   fansi_1.0.6         colorspace_2.1-1   
    ## [43] rmarkdown_2.29      tools_4.4.2         pkgconfig_2.0.3    
    ## [46] htmltools_0.5.8.1

</details>
