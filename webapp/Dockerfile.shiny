FROM rocker/tidyverse
MAINTAINER contact@mikebudnick.com

RUN Rscript -e "install.packages(c('shiny', 'shinythemes', 'ggplot2', 'dplyr'))"

# See https://github.com/rocker-org/rocker-versioned/blob/master/rstudio/README.md
# for shiny options
RUN export ADD=shiny && bash /etc/cont-init.d/add

ADD app.R /srv/shiny-server/myapp/
