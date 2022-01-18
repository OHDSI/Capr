# @file PackageMaintenance
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Capr
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Format and check code
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("Capr")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()


# Create manual and vignettes:
unlink("extras/Capr.pdf")

# for linux
system("R CMD Rd2pdf ./ --output=extras/Capr.pdf")

# for windows
#shell('R CMD Rd2pdf ./ --output=extras/Capr.pdf')

# Capr_tutorial vignette
dir.create(path = "./inst/doc/", showWarnings = FALSE)
rmarkdown::render("vignettes/CAPR_tutorial.Rmd",
                  output_file = "../inst/doc/CAPR_tutorial.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
  toc = TRUE, number_sections = TRUE))
unlink("inst/doc/CAPR_tutorial.tex")


# complex-cohort-example
dir.create(path = "./inst/doc/", showWarnings = FALSE)
rmarkdown::render("vignettes/complex-cohort-example.Rmd",
                  output_file = "../inst/doc/complex-cohort-example.pdf",

  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("inst/doc/complex-cohort-example.tex")


# build site
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

