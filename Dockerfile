FROM rocker/r2u:24.04

RUN R -q -e "install.packages(c('shiny','bslib','dplyr','tidyr','lubridate','tsibble','fable','fabletools','feasts','ggplot2','scales','DT','purrr','vctrs','ggtime'), repos='https://cloud.r-project.org')"

WORKDIR /home/app
COPY . .

EXPOSE 3838
ENV R_HOST=0.0.0.0
CMD ["R", "-q", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=3838)"]