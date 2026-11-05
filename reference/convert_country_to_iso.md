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

convert_country_to_iso(country_names, "country")
#> [1] "Number of rows missing an ISO country code after convert_country_to_iso: 0"
#>                         country_name alpha_3_code alpha_2_code
#> 1                   Papua New Guinea          PNG           PG
#> 2                            Morocco          MAR           MA
#> 3                           Slovenia          SVN           SI
#> 4                           Dominica          DMA           DM
#> 5                       Turkmenistan          TKM           TM
#> 6                            Hungary          HUN           HU
#> 7                              Italy          ITA           IT
#> 8                   Christmas Island          CXR           CX
#> 9                        New Zealand          NZL           NZ
#> 10                      Saudi Arabia          SAU           SA
#> 11    Turks and Caicos Islands (the)          TCA           TC
#> 12                         Singapore          SGP           SG
#> 13                           Senegal          SEN           SN
#> 14                    American Samoa          ASM           AS
#> 15                             Benin          BEN           BJ
#> 16                  French Polynesia          PYF           PF
#> 17                            France          FRA           FR
#> 18                         Lithuania          LTU           LT
#> 19        Saint Martin (French part)          MAF           MF
#> 20                     Cote d'Ivoire          CIV           CI
#> 21               Palestine, State of          PSE           PS
#> 22                          Djibouti          DJI           DJ
#> 23    Central African Republic (the)          CAF           CF
#> 24    United States of America (the)          USA           US
#> 25                          Suriname          SUR           SR
#> 26                              Chad          TCD           TD
#> 27                       Saint Lucia          LCA           LC
#> 28                 Brunei Darussalam          BRN           BN
#> 29                           Estonia          EST           EE
#> 30         Saint Pierre and Miquelon          SPM           PM
#> 31                           Algeria          DZA           DZ
#> 32                           Nigeria          NGA           NG
#> 33                              Niue          NIU           NU
#> 34        Syrian Arab Republic (the)          SYR           SY
#> 35            Svalbard and Jan Mayen          SJM           SJ
#> 36                           Turkiye          TUR           TR
#> 37                          Malaysia          MYS           MY
#> 38                            Brazil          BRA           BR
#> 39                             Qatar          QAT           QA
#> 40                       Timor-Leste          TLS           TL
#> 41                        Antarctica          ATA           AQ
#> 42                        Montserrat          MSR           MS
#> 43               Antigua and Barbuda          ATG           AG
#> 44                         Nicaragua          NIC           NI
#> 45                             Kenya          KEN           KE
#> 46                        Tajikistan          TJK           TJ
#> 47                     Bahamas (The)          BHS           BS
#> 48                       Puerto Rico          PRI           PR
#> 49          Russian Federation (the)          RUS           RU
#> 50                           Belgium          BEL           BE
#> 51                     Guinea-Bissau          GNB           GW
#> 52                     Liechtenstein          LIE           LI
#> 53                         Sri Lanka          LKA           LK
#> 54                   Solomon Islands          SLB           SB
#> 55                          Portugal          PRT           PT
#> 56                          Paraguay          PRY           PY
#> 57                           Ecuador          ECU           EC
#> 58                           Bermuda          BMU           BM
#> 59                   North Macedonia          MKD           MK
#> 60                          Anguilla          AIA           AI
#> 61                       Niger (the)          NER           NE
#> 62                           Ireland          IRL           IE
#> 63                           Georgia          GEO           GE
#> 64                         Guatemala          GTM           GT
#> 65  Bonaire, Sint Eustatius and Saba          BES           BQ
#> 66     Cocos (Keeling) Islands (the)          CCK           CC
#> 67                              Fiji          FJI           FJ
#> 68                          Bulgaria          BGR           BG
#> 69                           Ukraine          UKR           UA
#> 70                            Greece          GRC           GR
#> 71        Iran (Islamic Republic of)          IRN           IR
#> 72                        Luxembourg          LUX           LU
#> 73                        Cabo Verde          CPV           CV
#> 74                        San Marino          SMR           SM
#> 75                            Malawi          MWI           MW
#> 76                       South Sudan          SSD           SS
#> 77                            Monaco          MCO           MC
#> 78                     Aland Islands          ALA           AX
#> 79          Dominican Republic (the)          DOM           DO
#> 80                          Cameroon          CMR           CM
#> 81                             Nepal          NPL           NP
#> 82                           Armenia          ARM           AM
#> 83                            Canada          CAN           CA
#> 84              Cayman Islands (the)          CYM           KY
#> 85                             Egypt          EGY           EG
#> 86                 Wallis and Futuna          WLF           WF
#> 87                           Iceland          ISL           IS
#> 88                            Bhutan          BTN           BT
#> 89                           Czechia          CZE           CZ
#> 90                         Australia          AUS           AU
#> 91                           Tunisia          TUN           TN
#> 92                            Jordan          JOR           JO
#> 93                         Hong Kong          HKG           HK
#> 94                            Zambia          ZMB           ZM
#> 95                             Aruba          ABW           AW
#> 96                             Haiti          HTI           HT
#> 97        United Arab Emirates (the)          ARE           AE
#> 98               Trinidad and Tobago          TTO           TT
#> 99                            Tuvalu          TUV           TV
#> 100                        Argentina          ARG           AR
```
