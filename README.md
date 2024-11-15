# PHQ Mental Health + Chronic Pain Comorbidity

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## A compendium of code, data, and author's manuscript accompanying the manuscript:

#### TBD


## Overview
This repository is organized as a reproducible research compendium. Future updates to this compendium will include a Dockerfile and Binder Container

## File Organization

    analysis/
    |
    ├── logs/
    │   └── log.md          # log of any progress or relevant information
    |
    ├── figures/            # location of the figures produced for the manuscript
    |
    ├── data/
    |   ├── rawData/        # data obtained from elsewhere
    │   └── derivedData/    # data generated from rawData/ and scripts.*
    |   
    └── supplementaryMaterials/
        ├── supplementaryFigures/     
        |                   # supplementary figures for the main manuscript
        └── supplementaryTables/      
                            # supplementary tables for the main manuscript 
    
    src/           # R scripts to run in the following order (also see associated README.md)
        └── analysis.R        # Analysis script used to wrangle the raw data, produce figures, analyses, and supplementary materials

        

