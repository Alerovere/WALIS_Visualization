FROM rocker/r-ver:4.1.0
ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
WORKDIR merging
COPY . .
RUN R -e 'renv::restore()'
CMD Rscript run_analysis.R walis_merging.csv 100 F
