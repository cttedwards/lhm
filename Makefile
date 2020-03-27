# Makefile for generating the R package
#
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
R_FILES := $(wildcard R/*.R)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES)
VIGNETTE_FILES :=$(wildcard vignettes/*.Rmd)

ifeq ($(OS),Windows_NT) 
	RM = rm -rf
	CP = cp -f
	CD = cd
else
	RM = rm -rf
	CP = cp -f
	CD = cd
endif

all: build clean

test:
	Rscript inst\tests\tests.R
    
build: $(PKG_FILES)
	Rcmd build .
	Rcmd INSTALL $(PKG_NAME)_*.tar.gz
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript version_update.R

vignettes: $(VIGNETTE_FILES)
	$(CD) vignettes; Rcmd Sweave *.Rmd
	
clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/
