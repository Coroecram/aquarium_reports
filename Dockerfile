FROM rocker/tidyverse
MAINTAINER contact@mikebudnick.com

RUN Rscript -e "install.packages(c('shiny', 'shinythemes', 'DT', 'ggplot2', 'dplyr', 'pool'))"

# See https://github.com/rocker-org/rocker-versioned/blob/master/rstudio/README.md
# for shiny options
RUN export ADD=shiny && bash /etc/cont-init.d/add

COPY app.R /srv/shiny-server/aquarium_report/

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
