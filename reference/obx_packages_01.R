


# PACKAGES function - This will check to see if a package is
# installed or not - if not, it will install it. if it is, it ignores it

installPackage <- function(x) {
  x <- as.character(match.call()[[2]])
  if (!require(x, character.only = TRUE)) {
    install.packages(pkgs = x, repos = "http://cran.r-project.org")
    require(x, character.only = TRUE)
  }
}

# running list of packages used in CORE 2.0
# CHECK CODE BELOW
# Final check on packages after all coding completed. We should aim to remove plyr
# which often causes namespace issues

installPackage(here)
installPackage(devtools)

## data wrangling packages
installPackage(tidyverse)
installPackage(lubridate)
installPackage(scales)

## mapping packages
# installPackage(LaTex)
installPackage(sf)
installPackage(ggrepel)
installPackage(ggforce)
installPackage(cli)

# installPackage(pdflscape)
# installPackage(fancyhdr)
installPackage(kableExtra)

# installPackage(rmarkdown)
installPackage(knitr)
# installPackage(tinytex)

# Read in R
installPackage(readxl)
installPackage(writexl)

# Graphing
installPackage(cowplot)
installPackage(RColorBrewer)
installPackage(patchwork)
installPackage(gridExtra)
installPackage(ggspatial)
installPackage(ggpubr)
installPackage(flextable)
installPackage(grid)
installPackage(ggnewscale)
installPackage(extrafont)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
installPackage(ggspatial)
installPackage(ggrepel)
installPackage(ggsflabel)
installPackage(cli)
installPackage(ggtext)

# cluster analysis
installPackage(cluster)

# # SIR Functions
# devtools::install_github("https://github.com/nish-kishore/sirfunctions", "temp-fix")
installPackage(sirfunctions)

# Writing to Sharepoint
installPackage(officer)
installPackage(Microsoft365R)
installPackage(measurements)

#clean up
rm(installPackage)

