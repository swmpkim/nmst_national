# Namaste National

This repository contains code that compiled national-level data frames from the reserve-level data frames. These national data frames were used in all national-level analyses for the project. This repository also contains code for national-level multivariate analyses, and various exploratory scripts.

## Data Processing from reserve-level to national

### Inputs and Outputs

Input files for the national data frames were:

-   *Reserve-level data files* - files ending with “\_veg.xlsx” in the Data & templates / FINAL Nat’l data frames / reserve_level folder of the google drive (not linked publicly as not all data files are shared).

-   *Reserve-level analysis spec files* - for definitions of Ecotone invaders/migrators - files ending with “\_veg-specs.xlsx” in the Data & templates / FINAL Nat’l data frames / reserve_level folder of the google drive.

-   *Explanatory matrix*, a spreadsheet of compiled information about each reserve and/or site.

**Note:** national data frames were compiled before EIR (Ecotone Invader Ratio) was renamed to EMI (Ecotone Migration Index). Additionally, the spec files changed quite a bit before the final iteration of reserve-level code. Thus, if national-level data frames need to be regenerated in the future, some code modifications will need to be made for the newer versions of spec files. For these national analyses, we used the national data frames that referred to EIR and changed that to EMI in the code.

Outputs of these processing steps were the following csv files. See the national data frame data dictionary for details on the columns in each.

-   *national_plot-level.csv* - combined file with one row per vegetation plot per date, where columns represented vegetation groups or categories and other metrics of interest (e.g. species richness, EMI). Sites identified for exclusion from national analyses (due to e.g. not having a minimum number of plots, or being a restoration site) are not included in this file.

-   *national_plot-levelAllSites.csv* - same as above, but with all sites, even those excluded from national analyses.

-   *veg_and_expl.csv* - national plot-level file, combined with information from the explanatory matrix. Excluded sites not included in this file.

-   *slopesAndExpl_byPlot.csv* - estimates of change through time for each metric of interest, calculated for each vegetation plot via simple linear regression ( metric \~ year).

-   *slopesAndExpl_bySite.csv* - plot-level estimates of change through time, averaged to the site level.

-   *slopesAndExpl_byZone.csv* - plot-level estimates of change through time, averaged to the vegetation zone (within site) level. Vegetation zone was lumped to 'low', 'mid', and 'up' from 11 possible categories earlier in data processing.

### Data processing steps

Data processing was conducted using scripts in this repository in the following order:

1.  ***Pre-processing Reserve files:*** `01_pre-processing.R`. For each reserve, data and ‘spec’ files were read into R and the following procedures were conducted. This had to be run at the reserve level to take into account specific species and ecotone invaders/migrators at each reserve, and so a csv was generated for each reserve.

    a.  check explanatory matrix to see if Reserve uses PI (point intercept) or OC (ocular cover) methodology for cover data. If PI, then convert to OC based on coefficient values in the PI-to-OC tab of the explanatory matrix.

    b.  force all rows’ cover readings to total 100.

    c.  calculate EIR (later renamed to EMI) for each plot on each date, based on “Ecotone_Invaders” tab of reserve spec file.

    d.  calculate species richness and shannon-weiner diversity, both for only species or categories identified as “Live vegetation” in the “Species_Names” tab of each reserve’s data file, for each plot on each date. The the ‘vegan’ package was used for these calculations.

    e.  calculate cover by species or cover group (e.g., A-Algae, B-Brackish, H-Halophyte; Bare, Dead) by summing cover by species for each category, based on categorizations in the “Species_Names” tab of the reserve data files. Species groups were used rather than individual species because individual species are so different at a national scale that comparisons would be impossible.

    f.  calculate additional covers or ratios as determined by the TWG; e.g. non-native invasives; Salt-to-Total live vegetation ratio (Salt = plants labelled as Algae, Brackish, or Halophyte; when total live was 0, ratio was set to NA); Unvegetated-to-live vegetated ratio

    g.  certain individual species of interest were also retained in case we want to analyze them in the future: Spartina alterniflora, Juncus roemerianus, Spartina patens, Salicornia pacifica

    h.  attach reserve-defined vegetation zone to each plot, along with lat, long, orthometric height, and distance to water; all as defined in the “Station_Table” sheet of the reserve data file

    i.  save csv of the grouped data frame for use in the next steps. 

2.  ***Combining data across reserves:*** `02_combining.R`. The grouped data from each reserve, from step 1 above, was read and all data frames combined into a single data frame, with all readings from all vegetation plots on all dates. Sites identified for exclusion in the screening matrix (see Site screening section of the national analysis plan) were noted in the explanatory matrix, and removed before combination of data. Combined file written out as `national_plot-level.csv`. A later version was created that did not exclude any sites, for generation of graphics for the dashboard. This later version is `national_plot-level_AllSites.csv`.

3.  **Combining plot-level data with explanatory variables:** 03_explanatory_matrix.R. The national, plot-level data frame from step 2 was combined with additional explanatory factors, resulting in the `veg_and_expl.csv file`.

    a.  Information from the “Time added” and “Time removed” sheets of the explanatory matrix was bound to the national plot-level csv from above, by Reserve and Site.

    b.  Vegetation Zones were lumped: in reserve-level data, there were 11 zone options. For easier comparison at the national scale, these were combined into:

        i.  Low: Mudflat, Seaward Edge, Low Marsh, Pools/Pannes

        ii. Mid: Transition, High Marsh

        iii. Up: Upland Edge, Freshwater Tidal, Upland

4.  ***Calculation of change at various aggregation levels:*** `04_national_slope_df_construction.R`. The plot-level vegetation + explanatory factor file was used to generate estimates of change-through-time for each metric of interest at the plot scale, then averaged to zone-within-site and site. Three files were generated, one for each level of aggregation.

    a.  Estimates of change-through-time for each metric of interest (vegetation groups, EMI, and ratios generated in step 1; additionally on some explanatory factors such as average temperature and sum of precipitation) were calculated using simple linear regression at the level of the individual vegetation plot. The output file is `slopesAndExpl_byPlot.csv`.

    b.  Slopes for each plot were averaged to the level of site. The output file is `slopesAndExpl_bySite.csv`.

    c.  Slopes for each plot were also averaged to the level of vegetation zone within site. The output file is `slopesAndExpl_byZone.csv`. The site averages do not take vegetation zone into account.

## Multivariate analyses

Multivariate analyses were conducted primarily at the site level, in the script `00_national_multivariate_bySite.qmd`, with the input file `slopesAndExpl_bySite.csv`. NMDS was also performed at both zone- and plot- levels, using the script `00_national_multivariate_byZone.qmd`.
