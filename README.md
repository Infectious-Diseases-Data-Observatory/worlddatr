
# iddoverse

<!-- badges: start -->
<!-- badges: end -->

The {iddoverse} contains R functions to convert IDDO-curated SDTM
domains into analysis datasets, similar to ADaM datasets. These reusable
functions aim to provide a toolbox for researchers to modify the
analysis dataset to their study-specific needs, speeding up the time it
takes to create analysable data.

This package takes inspiration from the Pharmaverse, specifically the
[{admiral}](https://github.com/pharmaverse/admiral)R package, however,
IDDO-SDTM formats are not strictly compliant with standards required by
pharma companies, as a result, the purpose for the {iddoverse} is to map
the IDDO-SDTM format to IDDO-ADaM datasets.

    IDDO - Infectious Disease Data Observatory
    SDTM - Study Data Tabulation Model, an internation data storage model from CDISC.
    ADaM - Analysis Data Model, an internation data analysis model from CDISC.

## Installation

You can install the development version of IDDOverse from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") #if you have not previously installed 'devtools' on your machine
devtools::install_github("RhysPeploe/iddoverse")
FALSE vctrs     (0.6.3  -> 0.6.4 ) [CRAN]
FALSE rlang     (1.1.1  -> 1.1.2 ) [CRAN]
FALSE stringi   (1.7.12 -> 1.8.2 ) [CRAN]
FALSE stringr   (1.5.0  -> 1.5.1 ) [CRAN]
FALSE Rcpp      (1.0.9  -> 1.0.11) [CRAN]
FALSE dplyr     (1.1.3  -> 1.1.4 ) [CRAN]
FALSE lubridate (1.9.2  -> 1.9.3 ) [CRAN]
FALSE 
FALSE   There is a binary version available but the source version is later:
FALSE         binary source needs_compilation
FALSE stringi  1.8.1  1.8.2              TRUE
FALSE 
FALSE package 'vctrs' successfully unpacked and MD5 sums checked
FALSE package 'rlang' successfully unpacked and MD5 sums checked
FALSE package 'stringr' successfully unpacked and MD5 sums checked
FALSE package 'Rcpp' successfully unpacked and MD5 sums checked
FALSE package 'dplyr' successfully unpacked and MD5 sums checked
FALSE package 'lubridate' successfully unpacked and MD5 sums checked
FALSE 
FALSE The downloaded binary packages are in
FALSE   C:\Users\rpeploe\AppData\Local\Temp\RtmpEhyrel\downloaded_packages
FALSE ── R CMD build ─────────────────────────────────────────────────────────────────
FALSE          checking for file 'C:\Users\rpeploe\AppData\Local\Temp\RtmpEhyrel\remotes704846d313b7\RhysPeploe-iddoverse-67f327e/DESCRIPTION' ...     checking for file 'C:\Users\rpeploe\AppData\Local\Temp\RtmpEhyrel\remotes704846d313b7\RhysPeploe-iddoverse-67f327e/DESCRIPTION' ...   ✔  checking for file 'C:\Users\rpeploe\AppData\Local\Temp\RtmpEhyrel\remotes704846d313b7\RhysPeploe-iddoverse-67f327e/DESCRIPTION' (463ms)
FALSE       ─  preparing 'iddoverse': (1.6s)
FALSE    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
FALSE       ─  checking for LF line-endings in source and make files and shell scripts (755ms)
FALSE   ─  checking for empty or unneeded directories
FALSE       ─  building 'iddoverse_0.2.0.tar.gz'
FALSE      
FALSE 
library(iddoverse)
```

Alternatively, to load the most recent update:

``` r
pkg = c('RhysPeploe/iddoverse')
pacman::p_load_current_gh(char = pkg)
```

We recommend updating the install regularly as the package is developing
constantly. Version 1.0 will be released in the next few months. Until
then, versions starting with ‘0.’ should be expected to change without
notification.

## Why is this useful?

The code will speed up the time taken to produce analysis datasets and
provide consistent outputs. The user should be able to simply input
their domains and the code should create an analysis dataset.

## Citation

To cite iddoverse in publications, see
[CITATION](https://github.com/RhysPeploe/iddoverse/blob/main/inst/CITATION)

## Issues

Improvements to the code are constantly being made, if you notice
errors, bugs, want to suggest improvements or have ideas for better
functionality, please describe them in
[Issues](https://github.com/RhysPeploe/iddoverse/issues).

## Contact

Please contact Rhys Peploe (<rhys.peploe@iddo.org> or
<rhyspeploe1998@gmail.com>) if you would like to know more.
