#!/bin/bash
export PKG_LIBS=`Rscript -e "Rcpp::LdFlags()"`
export PKG_CXXFLAGS=`Rscript -e "Rcpp::CxxFlags"`
R CMD SHLIB test.cpp