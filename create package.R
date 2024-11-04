library(devtools)
library(roxygen2)
use_git()
use_r("Integrand")

load_all()

exists("integral.s1", where = globalenv(), inherits = FALSE)

check()

use_mit_license()

use_testthat()

document()

install()

library(ClinTrialPredict)

use_package('pracma')

use_readme_rmd()

export(TrialPred.TwoArm)

