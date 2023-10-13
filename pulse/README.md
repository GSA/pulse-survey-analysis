In October 2021, the General Services Administration, Office of Management and Budget, and Office of Personnel Management launched a pilot pulse survey initiative. The pulse survey invited federal employees to share their thoughts on 3-4 questions, to help inform the administration’s actions on how best to support the Federal workforce. You can read more about the pulse surveys on the website of GSA's [Office of Evaluation Sciences](https://oes.gsa.gov/collaborations/government-wide-pulse-survey/).

After completion, survey responses were analyzed together with transactional and demographic data inside OPM's secure [EHRI environment](https://www.opm.gov/policy-data-oversight/data-analysis-documentation/enterprise-human-resources-integration/#url=Overview). This R package was written to standardize the data prep and analysis pipeline. The code relies on data that only exists within EHRI; it cannot be run without access to this data.

This package was made using [`devtools`](https://www.r-project.org/nosvn/pandoc/devtools.html). Functions are defined in the `\R` subfolder. Documentation was generated in the `\man` subfolder using `roxygen2`.

To use this package, you can download this repository and run `devtools::install("pulse-survey-analysis")` and then `library("pulse-survey-analysis")`.

Contact: Neil Miller, neil.miller@gsa.gov
