# message-australia-degrowth
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10490965.svg)](https://doi.org/10.5281/zenodo.10490965)

Scripts to recreate figures for the analysis as presented in "Downscaling Down Under: Towards degrowth in integrated assessment models" (Kikstra et al., 2024) in Economic Systems Research.
DOI: https://doi.org/10.1080/09535314.2023.2301443

For reproducing the figures from the manuscript:
Step 1: download IPCC AR6 Scenario data (see below)
Step 2: adjust the paths for the AR6 data in `process-data.R` and `f10.R`, being the following two variables:
- `IPCC.DATA.FILE.R10` (`process-data.R`)
- `IPCC.META.FILE` (`process-data.R` and `f10.R`)
Step 3:
- To reproduce all figures in one go, run the script `produce_all_figures.R`.
- Alternatively, you can run each figure separately. (after running `process-data.R`)

### AR6 scenario database downloads
PLEASE NOTE: to make this work, you first need to download the AR6 Scenario Database (R10).
The data that these scripts use need to be downloaded from the AR6 Scenario Database hosted by IIASA. Here, we used release version 1.1.
The scenarios that were used for the IPCC Sixth Assessment Report, Working Group III contribution on Climate Change Mitigation (AR6 WGIII), can be downloaded from https://data.ece.iiasa.ac.at/ar6.

More specifically, go to the 'Downloads' tab and click on the "AR6_Scenarios_Database_R10_regions_v1.1" download link, to obtain the following two files:
1. `AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx`
2. `AR6_Scenarios_Database_R10_regions_v1.1.csv`

For a more detailed description of that data, please refer to:
Edward Byers, Volker Krey, Elmar Kriegler, Keywan Riahi, Roberto Schaeffer, Jarmo Kikstra, Robin Lamboll, Zebedee Nicholls, Marit Sanstad, Chris Smith, Kaj-Ivar van der Wijst, Alaa Al Khourdajie, Franck Lecocq, Joana Portugal-Pereira, Yamina Saheb, Anders Strømann, Harald Winkler, Cornelia Auer, Elina Brutschin, Matthew Gidden, Philip Hackstock, Mathijs Harmsen, Daniel Huppmann, Peter Kolp, Claire Lepault, Jared Lewis, Giacomo Marangoni, Eduardo Müller-Casseres, Ragnhild Skeie, Michaela Werning, Katherine Calvin, Piers Forster, Celine Guivarch, Tomoko Hasegawa, Malte Meinshausen, Glen Peters, Joeri Rogelj, Bjorn Samset, Julia Steinberger, Massimo Tavoni, Detlef van Vuuren.
AR6 Scenarios Database hosted by IIASA, version 1.1.
International Institute for Applied Systems Analysis, 2022.
doi: [10.5281/zenodo.5886911](https://doi.org/10.5281/zenodo.5886911) | url: [data.ece.iiasa.ac.at/ar6/](https://data.ece.iiasa.ac.at/ar6)
Download citation: [bibtex](https://data.ece.iiasa.ac.at/ar6/static/files/ar6_scenario_data.bib)
