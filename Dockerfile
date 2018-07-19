FROM rocker/tidyverse
MAINTAINER contact@mikebudnick.com

ARG AWS_PG_HOST
ARG AWS_PG_USER
ARG AWS_PG_PW

ENV AWS_PG_HOST=$AWS_PG_HOST
ENV AWS_PG_USER=$AWS_PG_USER
ENV AWS_PG_PW=$AWS_PG_PW

RUN Rscript -e "install.packages(c('shiny', 'shinythemes', 'DT', 'ggplot2', 'dplyr'))"

# See https://github.com/rocker-org/rocker-versioned/blob/master/rstudio/README.md
# for shiny options
RUN export ADD=shiny && bash /etc/cont-init.d/add

ADD app.R /srv/shiny-server/myapp/
