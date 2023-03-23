# Raw Data Sources

## Medicaid Expansion

Information on the status of ACA Medicaid Expansion for states in the US was retrieved from the [Kaiser Family Foundation](https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions-interactive-map/).

 - `expansion-status-interactive-map_1.2.19.csv` was downloaded June 21, 2020 and used to created the treatment condition indicator for analyses.
 - `expansion-status-interactive-map_11.9.2022.csv` was downloaded January 18, 2023 and used to create an updated table of expansion status for the manuscript.

## Flu View
All files in `flu-view/` were downloaded from [FluView Interactive](https://gis.cdc.gov/grasp/fluview/mortality.html) from the CDC between 8/11/2020 to 2/18/21. The data was updated weekly, so to get the same files, a special request would need to be made.

## Institute for Health Metrics and Evaluation (IHME)
All files in directories that begin with `IHME` are downloaded from the [Global Health Data Exchange](https://ghdx.healthdata.org/us-data). Each sub-directory contains a README file with citation and download information.

## CDC WONDER
All files in `wonder/` were downloaded using CDC WONDER [Multiple Cause of Death, 1999-2020](https://wonder.cdc.gov/mcd-icd10.htm) requests.

Some specific requests, as examples can befound at the links below:
- `wonder_2014_ac_age_adj.txt`- [link](https://wonder.cdc.gov/controller/saved/D77/D297F348)
- `wonder_2014_ac_white_age_adj.txt`- [link](https://wonder.cdc.gov/controller/saved/D77/D297F349)
- `wonder_2014_hca_age_adj.txt`- [link](https://wonder.cdc.gov/controller/saved/D77/D297F349)
- `wonder_2014_hca_white_age_adj.txt`- [link](https://wonder.cdc.gov/controller/saved/D77/D297F352)

`wonder_2012_age_adj.txt` and `wonder_2013_age_adj.txt` use the same request as `wonder_2014_ac_age_adj.txt`, but with the year changed.

## American Community Survey

Data on multigenerational households (`ACSST5Y2013.S1002_data_with_overlays_2020-06-21T223140.csv`) come from the ACS table S1002, 2013 5-Year Estimates and were downloaded from the [US Census Bureau](https://data.census.gov/table?q=S1002&g=0100000US$050000,$0500000&tid=ACSST5Y2013.S1002).


## County Adjacency

County adjacency data (`county_adjacency2010.csv`) was downloaded from the [National Bureau of Economic Research](https://www.nber.org/research/data/county-adjacency).
