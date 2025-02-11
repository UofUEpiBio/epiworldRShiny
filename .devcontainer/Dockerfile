FROM rocker/tidyverse:4.4.1

# Installing only packages that are not already installed in the base image
RUN install2.r -s \
  shinyjs \
  utils \
  shinydashboard \
  DT \
  ggplot2 \
  epiworldR \
  shinycssloaders \
  plotly \
  pkgload \
  shiny


RUN install2.r -s rsconnect

CMD ["bash"]