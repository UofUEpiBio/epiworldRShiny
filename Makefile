# Capture the current directory
DIR_NAME:=$(shell basename `pwd`)
PKG_NAME:= epiworldRShiny

help:
	@echo "Makefile commands:"
	@echo "  docs          - Generate documentation using roxygen2"
	@echo "  build         - Build the R package"
	@echo "  install       - Install the R package"
	@echo "  run           - Run the Shiny application"
	@echo "  dev           - Build, install, and run the Shiny app"
	@echo "  check         - Check the R package for CRAN compliance"
	@echo "  docker-build  - Build the Docker image for the Shiny app"
	@echo "  docker-push   - Push the Docker image to Docker Hub"
	@echo "  docker-run    - Run the Shiny app in a Docker container"
	@echo "  deploy        - Deploy the Shiny app to shinyapps.io"
	@echo "  README.md     - Generate README.md from README.Rmd"
	@echo "  update-data   - Update data from raw sources"


docs:
	Rscript -e 'devtools::document()'

build:
	R CMD build .

install:
	Rscript -e 'devtools::install()'

run:
	Rscript --vanilla -e 'epiworldRShiny::epiworldRShiny(".")'

dev: build install run

check:
	Rscript --vanilla -e 'devtools::check()'

docker-build:
	docker build -t gvegayon/epiworldrshiny .

docker-push:
	docker push gvegayon/epiworldrshiny

docker-run:
	docker run -i --rm -v$(PWD):/epiworld/ uofuepibio/epiworldrshiny


deploy:
	Rscript --vanilla epishiny/deploy.R

README.md: README.Rmd
	Rscript -e 'rmarkdown::render("README.Rmd")'

.PHONY: docs build install run check docker-build

update-data: update-other-data
	@echo "Note: Utah measles school data is not updated automatically."
	@echo "To update Utah data, first obtain 'data-raw/measles_school_data_final.csv'"
	@echo "from the Utah DHHS (see data-raw/README.md), then run: make update-utah-data"

# NOTE: update-utah-data depends on 'data-raw/measles_school_data_final.csv',
#   which is not tracked in git. Obtain this file from the Utah Department of
#   Health and Human Services (DHHS) and place it at that path before running.
update-utah-data: data-raw/01_utah_school_data.R data-raw/measles_school_data_final.csv
	R CMD BATCH data-raw/01_utah_school_data.R \
		data-raw/01_utah_school_data.Rout

update-other-data: data-raw/02_download_and_combine.R
	R CMD BATCH data-raw/02_download_and_combine.R \
		data-raw/02_download_and_combine.Rout
