[![Build Status](https://travis-ci.org/cttedwards/lhm.svg?branch=master)](https://travis-ci.org/cttedwards/lhm)

lhm
===

Module for the storage and processing of life-history data,
designed to allow the calculation of derived parameters from life-history
theory. Currently it only allows calculation of the intrinsic growth rate $r$ using
the Euler-Lotka equation.

Outputs from this package, specifically $r$, are used by the `bdm` package, which implements a 
Bayesian biomass dynamic model for estimation of population status using empirical catch and abundance data. In
this instance the $r$ calculated by `lhm` is used as prior for model fitting by `bdm`.
