# Visualization and data query for the WALIS database

[![DOI](https://zenodo.org/badge/329045377.svg)](https://zenodo.org/badge/latestdoi/329045377)

This is the code for a shiny app that takes the data from the WALIS database (https://warmcoasts.eu/world-atlas.html#). The code was prepared by Sebastian Garzòn (https://github.com/SbastianGarzon) under the supervision of A. Rovere.

The interface shows the results of a script (running offline) that summarizes the output of the WALIS sea level database as follows, starting from the "Summary" table in the database.

## Calculate RSL percentiles
The script takes information on relative sea level values and calculates RSL percentiles in the following way.
1. If the RSL Indicator is a "Single Coral": the percentiles are obtained from a gamma function interpolated considering the upper limit of living range inserted in the database as, respectively, the 2.3 and 97.7 percentiles of the distribution.
2. If the RSL Indicator is a "Sea Level Indicator" or "Single Speleothem": the percentiles on paleo RSL are calculated from the gaussian distribution represented by the field "Paleo RSL (m)" and its associated uncertainty (1-sigma).
3. If the RSL Indicator is a "Terrestrial Limiting" or "Marine Limiting", the RSL percentiles are not calculated.

## Calculate age percentiles
Then, the script takes information on age values and calculates age percentiles according to the table below. The following modifications are done on the original data:
- If a percentile goes below zero, it is set to zero.
- If Lower age > Upper age, the two values are reversed.
- If there is no age, the corresponding record is deleted.

| Dating technique | Pre-selection | Lower age | Age (ka) 0.1 perc | Age (ka) 2.3 perc | Age (ka) 15.9 perc | Age (ka) 50 perc | Age (ka) 84.1 perc | Age (ka) 97.7 perc | Age (ka) 99.5 perc | Upper age |
|-|-|-|-|-|-|-|-|-|-|-|
| U-series / coral | Recalculated age used if available. If not, Reported age is used | NaN | Average age - 3 Sigma age | Average age - 2 Sigma age | Average age - 1 Sigma age | Average age | Average age + 1 Sigma age | Average age + 2 Sigma age | Average age + 3 Sigma age | NaN |
| U-series / speleothem | Recalculated age used if available. If not, Reported age is used | NaN | Average age - 3 Sigma age | Average age - 2 Sigma age | Average age - 1 Sigma age | Average age | Average age + 1 Sigma age | Average age + 2 Sigma age | Average age + 3 Sigma age | NaN |
| U-series / mollusks or algae | Upper and lower age derived from the MIS to which the sample is associated with | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| AAR / Age reported | | NaN | Average age - 3 Sigma age | Average age - 2 Sigma age | Average age - 1 Sigma age | Average age | Average age + 1 Sigma age | Average age + 2 Sigma age | Average age + 3 Sigma age | NaN |
| AAR / Only MIS reported | Upper and lower age derived from the MIS to which the sample is associated with | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| ESR / Age reported | | NaN | Average age - 3 Sigma age | Average age - 2 Sigma age | Average age - 1 Sigma age | Average age | Average age + 1 Sigma age | Average age + 2 Sigma age | Average age + 3 Sigma age | NaN |
| ESR / Only MIS reported | Upper and lower age derived from the MIS to which the sample is associated with | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| Luminescence / Age reported | | NaN | Average age - 3 Sigma age | Average age - 2 Sigma age | Average age - 1 Sigma age | Average age | Average age + 1 Sigma age | Average age + 2 Sigma age | Average age + 3 Sigma age | NaN |
| Luminescence / Only MIS reported | Upper and lower age derived from the MIS to which the sample is associated with | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| Stratigraphic constraint / Age reported| Upper and lower age derived from the reported age | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| Stratigraphic constraint / Only MIS reported| Upper and lower age derived from the MIS to which the sample is associated with | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| Other age constraint / Age reported| Upper and lower age derived from the reported age | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |
| Other age constraint / Only MIS reported| Upper and lower age derived from the MIS to which the sample is associated with | Lower age |<--|--|--| Uniform distribution |--|--|-->| Upper age |

## Delete rows and raise an exception
1. If the RSL is "null"
2. If the RSL error is "null"
3. If there is no age associated with the datapoint

## Funding
This software is part of a project that has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (Grant agreement No. ERC-StG-802414)
