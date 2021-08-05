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
The Python command line and the R files are in the same top-level directory, currently. This may change in the future, as necessary.

- `init`: Contains the parameter files
- `src`: Contains all code files

- `Makefile`: Simplifies calling certain commands
    - `make test`: Will run test (currently Python-only)
    - `make build`: Will use [poetry](https://python-poetry.org) to get dependencies

## Python CLI

### Requirements

To run and test the CLI, you will need [poetry](https://python-poetry.org) as a dependency manager. To install poetry, run

```bash
$ pip install --user poetry
$ poetry -v   # Run this to confirm installation
```

Installing the dependencies can be done through `make build`. This currently just runs `poetry install`. The current dependencies are on Pandas, Numpy, Typer, and the OpenDP libraries. 

Running `poetry build`/`make build` will create a Python virtual environment. All further python commands should be prefaced with `poetry run` in the directory that has `pyproject.toml` and `poetry.lock` in it.

### Command Line

The Python CLI code is in `metapopulation.py`. It uses [Typer](https://typer.tiangolo.com/) to set up the command line structure, which has two subcommands. Running the command can be done through:

```bash
$ poetry run python metapopulation.py --help  # This will show you help for the command
```

#### simulate Subcommand

The simulation is run by executing:

```bash
$ poetry run python metapopulation.py simulate \
    [--mechanism] \        # Noise type to be added. Current implementations support laplace and gaussian
    [--epsilon] \          # Specify the privacy budget
    [--iterations]         # The number of noisy iterations to run
```

This is *not* implemented, but susequent versions will directly generate noise data for different locations/time periods based on default noise type and privacy budget. The simulation can be run by executing:

```bash
$ poetry run python metapopulation.py simulate \
    [--fips state fips] \        # The state FIPS to filter down to, default to NY
    [--iterations count] \       # The number of noisy iterations to run
    [--start-date YYYY-MM-DD]  \ # The start date filter by, inclusive
    [--end-date YYYY-MM-DD]  \   # The end date filter by, exclusive
    [local data path] 
```

The `local data path` is the location on your own machine that houses the data. The subdirectories where the data should be stored be of the form `activity_day=YYYY-MM-DD`, matching the data sets as they are stored in Google Cloud Platform. See the `tests/data` directory for an example layout.


#### pull Subcommand

This is *not* implemented, but it will eventually extract from Google Cloud Platform.


### Testing

The code uses [pytest](https://pytest.org) for its test infrastructure. Any functions named `test_` in files named `test_` in the `tests` directory will be executed. (That's a real sentence.)

For unit tests, primarily test the reasonability of outputs on small data sets, not necessarily the full statistics. If there is a separate, long-running test that is needed to validate data sanity, create a script for that and run it independently.

Note: Automated CI through GitHub Actions is not set up yet.
