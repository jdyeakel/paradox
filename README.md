This is a work in progress.

The `paradox` `R` package contains C++. Therefore, you will need a C++ compiler installed to build the package from source. See <http://www.rstudio.com/ide/docs/packages/prerequisites>

Once you've installed a C++ compiler, you can install, load the package, and read the help with:

```S
install.packages("devtools")
devtools::install_github("paradox", user = "jdyeakel")
library(paradox)
help(package = "paradox")
```
