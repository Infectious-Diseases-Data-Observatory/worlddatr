# Convert country names to ISO country codes

Matches a column of country names in your data with a bank of various
country names (`country_name_lookup.xlsx` in `inst/extdata`) and appends
two new columns for the ISO 3 and 2 letter codes in order to use
standardised codes instead of messy country names. The function reports
in the R console how many rows did not get matched with the data bank.

## Usage

``` r
convert_country_to_iso(data, country_name_col)
```

## Arguments

- data:

  Data frame with a column of country names

- country_name_col:

  Character. The name of the column containing the country names.

## Value

The input data frame with two new columns alpha_3_code and alpha_2_code
representing the ISO standardised 3 & 2 letter country codes

## Examples

``` r
country_names = data.frame(country = sample(world_income$country, 100))

if (FALSE) { # \dontrun{
  convert_country_to_iso(country_names, "country")
} # }
```
