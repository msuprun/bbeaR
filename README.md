
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
install_github('msuprun/bbeaR')
```

## Usage

See the full vignettes for [Egg (single assay plate)](https://msuprun.github.io/bbeaR/bbeaR_EggExample.html) or  [Milk (multi-plate assay)](https://msuprun.github.io/bbeaR/bbeaR_MilkExample.html) or visit Bioconductor for details:

- [Release version](http://www.bioconductor.org/packages/release/bioc/html/bbeaR.html)
- [Devel version](http://www.bioconductor.org/packages/devel/bioc/html/bbeaR.html)
