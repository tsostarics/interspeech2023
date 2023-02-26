This repository contains the analysis files used for the Interspeech 2023 paper *Pitch Accent Variation and the Interpretation of Rising Declaratives*.
The OSF page, which also contains the stimuli, is available here: [https://osf.io/8hrfv/](https://osf.io/8hrfv/)

Explanation of directories:

 - Data: Contains data from each experiment
 - Figures: Figures used in the paper available as `.svg` and `.pdf` files
 - Helpers: `.R` files containing various helper functions. Some are redundant with the [sosprosody package](https://github.com/tsostarics/sosprosody) and others aren't used in this portion of the project.
 - Models: Models reported in the paper, as `.rds` files
 - Writeups: `.qmd` and standalone `.html` files for running the models and generating the figures. Re-rendering the `.qmd` files will overwrite the `.html` files.
 - renv: `renv` environment for the packages used in this analysis. Please refer to [this page](https://rstudio.github.io/renv/articles/collaborating.html) for more information on using renv.
