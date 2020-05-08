
## Installation

To install from Bioconductor, use the following code:

```{r}
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("bbeaR")
```

To install directly from github:

```{r}
library(devtools)
install_github('bbeaR')
```

## Usage

See the full vignette in [rmarkdown](https://github.com/msuprun/bbeaR/vignettes/) or visit Bioconductor for details:

- [Release version](http://www.bioconductor.org/packages/release/bioc/html/bbeaR.html)
- [Devel version](http://www.bioconductor.org/packages/devel/bioc/html/bbeaR.html)
