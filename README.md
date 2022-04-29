Life history module
===================

[![R-CMD-check](https://github.com/cttedwards/lhm/actions/workflows/check-release.yaml/badge.svg)](https://github.com/cttedwards/lhm/actions/workflows/check-release.yaml)

Module for the storage and processing of life-history data,
designed to allow the calculation of derived parameters from life-history
theory. Currently it only allows calculation of the intrinsic growth rate $r$ using
the Euler-Lotka equation.

Outputs from this package, specifically <img src="https://render.githubusercontent.com/render/math?math=\large r">, are used by the `bdm` package, which implements a 
Bayesian biomass dynamic model for estimation of population status using empirical catch and abundance data. In
this instance the $r$ calculated by `lhm` is used as prior for model fitting by `bdm`.
