# @file PackageMaintenance
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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

# for linux system('R CMD Rd2pdf ./ --output=extras/Capr.pdf')

# for windows
shell("R CMD Rd2pdf ./ --output=extras/Capr.pdf")

# Capr design
dir.create(path = "./extras/pdf_vignette/", showWarnings = FALSE)
rmarkdown::render("vignettes/capr_design.Rmd",
                  output_file = "../extras/pdf_vignette/capr_design.pdf",

  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/capr_design.tex")


# Examples
#dir.create(path = "./extras/pdf_vignette/", showWarnings = FALSE)
rmarkdown::render("vignettes/Examples.Rmd",
                  output_file = "../extras/pdf_vignette/Examples.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
  toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/Examples.tex")


# Using Capr
#dir.create(path = "./extras/pdf_vignette/", showWarnings = FALSE)
rmarkdown::render("vignettes/Using-Capr.Rmd", output_file = "../extras/pdf_vignette/Using-Capr.pdf",
  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/Using-Capr.tex")


# capr templates dir.create(path = './extras/pdf_vignette/', showWarnings = FALSE)
rmarkdown::render("vignettes/capr_templates.Rmd",
                  output_file = "../extras/pdf_vignette/capr_templates.pdf",

  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/capr_templates.tex")


# capr concept sets dir.create(path = './extras/pdf_vignette/', showWarnings = FALSE)
rmarkdown::render("vignettes/Capr-conceptSets.Rmd",
                  output_file = "../extras/pdf_vignette/Capr-conceptSets.pdf",

  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/Capr-conceptSets.tex")


# capr components
rmarkdown::render("vignettes/capr_objects.Rmd",
                  output_file = "../extras/pdf_vignette/capr_objects.pdf",

  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/capr_objects.tex")

# build site
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

