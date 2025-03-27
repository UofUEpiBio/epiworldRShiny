# Get the version of the R package and save it
PKG_VERSION:=$(shell Rscript -e 'x<-readLines("DESCRIPTION");cat(gsub(".+[:]\\s*", "", x[grepl("^Vers", x)]))')

# Capture the current directory
DIR_NAME:=$(shell basename `pwd`)
PKG_NAME:= epiworldRShiny

which:
	@echo "PKG_VERSION: $(PKG_VERSION)"
	@echo "PKG_NAME: $(PKG_NAME)"

docs:
	Rscript --vanilla -e 'devtools::document()'

build:
	cd .. && R CMD build $(DIR_NAME)

install:
	cd .. && R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

run:
	Rscript --vanilla -e 'epiworldRShiny::epiworldRShiny(".")'

dev: build install run

check:
	cd .. && R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

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