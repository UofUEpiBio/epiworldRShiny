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


docs:
	Rscript --vanilla -e 'devtools::document()'

build:
	R CMD build .

install:
	Rscript --vanilla -e 'devtools::install()'

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