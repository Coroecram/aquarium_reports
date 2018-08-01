FROM rocker/tidyverse
MAINTAINER contact@mikebudnick.com

RUN Rscript -e "install.packages(c('shiny', 'shinythemes', 'DT', 'ggplot2', 'dplyr', 'lubridate', 'pool'))"

# See https://github.com/rocker-org/rocker-versioned/blob/master/rstudio/README.md
# for shiny options
RUN export ADD=shiny && bash /etc/cont-init.d/add


COPY app.R /srv/shiny-server/aqua_report/
COPY palettes.R /srv/shiny-server/aqua_report/
COPY aws.Renviron /srv/shiny-server/aqua_report/
COPY www /srv/shiny-server/aqua_report/www

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]

USER root
ENV TZ=America/New_York
RUN sudo echo "America/New_York" > /etc/timezone && sudo dpkg-reconfigure --frontend noninteractive tzdata
