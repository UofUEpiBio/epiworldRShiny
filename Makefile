# Get the version of the R package and save it
PKG_VERSION:=$(shell Rscript -e 'x<-readLines("DESCRIPTION");cat(gsub(".+[:]\\s*", "", x[grepl("^Vers", x)]))')
PKG_NAME := epiworldRShiny

which:
	@echo "PKG_VERSION: $(PKG_VERSION)"
	@echo "PKG_NAME: $(PKG_NAME)"

docs:
	Rscript --vanilla -e 'devtools::document()'

build:
	cd .. && R CMD build $(PKG_NAME)

install:
	cd .. && R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

run:
	Rscript --vanilla -e 'epiworldRShiny::epiworldRShiny()'

check:
	cd .. && R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz
