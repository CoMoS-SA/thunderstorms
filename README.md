Replication material for ["Raided by the storm: How three decades of thunderstorms shaped U.S. incomes and wages"](https://doi.org/10.1016/j.jeem.2024.103074) (Coronese et al., JEEM 2024) [![DOI](doi)](https://doi.org/10.1016/j.jeem.2024.103074)

This folder contains the replication codes for figures and tables contained both in the main paper and in the Supplementary Information. All codes are included. The raw data supporting the replication can be accessed at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14254071.svg)](https://doi.org/10.5281/zenodo.14254071)
. To ensure proper functionality, the raw data should be placed in a folder named "data" to match the relative paths.

The repo is structured as follows:

- `scripts`: this folder contains all main scripts, written in `R`. It includes scripts for data cleaning, estimation, robustness checks, figure generation, and exporting data to `Stata` format.
- `stata_rob`: This folder contains `Stata` scripts for estimating the various models allowing for spatial dependence and arbitrary serial correlation.
- `utilities`: This folder contains auxiliary data needed for the functionality of other scripts (e.g., shapefiles).

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

A copy of the GNU General Public License is available at http://www.gnu.org/licenses/.
