FROM rocker/tidyverse:4.4.1

# Installing only packages that are not already installed in the base image
RUN install2.r -s \
  utils \
  DT \
  ggplot2 \
  epiworldR \
  plotly \
  pkgload \
  shiny \
  bslib


RUN install2.r -s rsconnect languageserver httpgd

CMD ["bash"]