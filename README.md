# Study of the influence of extrapyramidal symptoms on cognitive function in patients with paranoid schizophrenia: A comprehensive data analysis.
Authors: Glebus Alexander, Likholetova Daria, Krashennikova Anna, Tumova Marianna

This repository contains code to conduct statistical analysis of provided data. 

### Folders: 

`/app` -- contains code of application for exploratory data analysis. Note: it utilises simulation data. 

`/data` -- directory-placeholder for storage of real data, which is not versionised

`/docs` -- directory which stores compiled web app from `/app` directory

`/scripts` -- directory containing scripts and RMD filed with analitical workflows developed during the project. 

`/scripts/0_preprocessing` -- directory containing functions and scripts for preliminary transformation and cleaning of the data

`/scripts/1_eda` -- directory intended to store data exploration results

`/scripts/2_stats` -- directory containing RMD with finalised statistical analysis

## How to run

requirements: R >= 4.2

run ` renv::restore() ` to restore environemnt [WIP]

1. Contact owner of the data in order to get real observations. 

2. Run `\scripts\2_stats\Analisys.Rmd` 


## Where is the resulting statistical workflow
The main work was carried out in Biostat2023_proj_antipsych_vs_chisophrenia\scripts\2_stats\Analisys.Rmd


