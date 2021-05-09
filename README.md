# Differential Privacy (DP) - Meta population model
## Overview
This repository contains code and examples to run a simple metapopulation model
using transition data between the population sources. The model is adapted from
https://github.com/SenPei-CU/COVID-19 into the [R language](https://www.r-project.org/about.html) from [MATLAB](https://www.mathworks.com/products/matlab.html) by
[Dr. Ayesha Mahmud](https://ayeshamahmud.github.io/).

The purpose of this repository is to create a standard model which produces
known outputs and vary the input human mobility data. These data will be processed
using varying differential privacy settings and produce epidemiologic metrics
of interest.

## Inputs
Standard inputs will eventually be stored in `init/*.yaml`, currently all parameters
other than the transition matrices are hard coded.
- Time varying transition matrix

## Outputs
Outputs of the model are determined by the epidemiologically relevant metrics
of interest that are usually extracted from models which use time-varying
transition data.
- the basic reproduction number
- the effective reproduction number
- measures of synchronicity
- measures of the incubation period
- epidemic size
- number of locations with epidemic
- etc.

## Organization
- `init`: Contains the parameter files
- `src`: Contains all code files

##Reproducibility of the model 

The execution of the model is modular and each of the section depend on the previews on. For replicability do use this following process
- Clone the Github repository and from the repository open one of the R files 
- `source(here("src", "dependencies.R"))`
- `source(here("src", "NY_model.R"))`
- `source(here("src", "metrics.R"))`
- `run_seir_model() %>%   run_metrics() # Insert here your news data sets and parameters` 
