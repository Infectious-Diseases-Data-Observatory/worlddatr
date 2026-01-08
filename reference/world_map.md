# Global Coordinates of Countries and Territories, with Country Codes and Income Group Data

A dataset with coordinate data of the perimeter of each
country/territory in the world, along with information on the
country/territory ISO codes and income group as defined by the World
Bank.

## Usage

``` r
world_map
```

## Format

### A data frame with 99,338 rows and 11 columns:

- alpha_3_code:

  The three digit ISO code for the country/territory

- alpha_2_code:

  The two digit ISO code for the country/territory

- numeric:

  Numeric ISO code for the country/territory

- long:

  Longitude of point

- lat:

  Latitude of point

- group:

  Numerical identifer for the region

- order:

  Sequence of points within region

- region:

  Name of Country or Territory

- subregion:

  Name of subregion, if appropiate

- country:

  Name of country or territory according to ISO data

- economy:

  Name of country or territory according to the World Bank data

- income_group:

  Income classification of the country/territory

- redcap_number:

  The numeric value for the country/territory in the IDDO REDCap
  codebook

- centroid_long:

  Longitude of the country/territory centre point/centroid

- centroid_lat:

  Latitude of the country/territory centre point/centroid

## Details

Note that region refers to the area which the points create a perimeter
for.

Dates that data was accessed on:

- World Bank: 09/09/2025

- ISO: 09/09/2025

- Centroids: 09/09/2025

- REDCap: 15/11/2024

- map_data: 18/11/2024 (ggplot2 v3.4.3)
