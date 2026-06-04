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

## Details

Path to extdata does not work outside of package project. Fix unresolved
so the function is not currently exported

## Examples

``` r
country_names = data.frame(country = sample(world_income$country, 100))

convert_country_to_iso(country_names, "country")
#> [1] "Number of rows missing an ISO country code after convert_country_to_iso: 0"
#>                                                   country_name alpha_3_code
#> 1                                                     Paraguay          PRY
#> 2                                                   Mozambique          MOZ
#> 3                                              Solomon Islands          SLB
#> 4                                     Dominican Republic (the)          DOM
#> 5                                                       Tuvalu          TUV
#> 6                                                      Iceland          ISL
#> 7                                                      Jamaica          JAM
#> 8                                Cocos (Keeling) Islands (the)          CCK
#> 9                                                    Nicaragua          NIC
#> 10                                                     Senegal          SEN
#> 11                                                     Turkiye          TUR
#> 12                                   Sint Maarten (Dutch part)          SXM
#> 13                                                      Serbia          SRB
#> 14                                                     Andorra          AND
#> 15                                                     Bermuda          BMU
#> 16                           French Southern Territories (the)          ATF
#> 17                                               French Guiana          GUF
#> 18                                                  Luxembourg          LUX
#> 19                                   Saint Pierre and Miquelon          SPM
#> 20                                                        Cuba          CUB
#> 21                                                      Panama          PAN
#> 22                                                    Dominica          DMA
#> 23                                                        Chad          TCD
#> 24                                                     Uruguay          URY
#> 25                                      Svalbard and Jan Mayen          SJM
#> 26                                                       Chile          CHL
#> 27                                  Saint Martin (French part)          MAF
#> 28                                                    Bulgaria          BGR
#> 29                                                    Eswatini          SWZ
#> 30                            Saint Vincent and the Grenadines          VCT
#> 31                                              American Samoa          ASM
#> 32                                                        Niue          NIU
#> 33                                              Norfolk Island          NFK
#> 34                                  Taiwan (Province of China)          TWN
#> 35                                                      Sweden          SWE
#> 36                              Turks and Caicos Islands (the)          TCA
#> 37                                                    Maldives          MDV
#> 38                        British Indian Ocean Territory (the)          IOT
#> 39                                                     Romania          ROU
#> 40                                                        Togo          TGO
#> 41                                         Antigua and Barbuda          ATG
#> 42                                                     Morocco          MAR
#> 43                                                   Argentina          ARG
#> 44                                                 Niger (the)          NER
#> 45                                                    Kiribati          KIR
#> 46                            Tanzania, the United Republic of          TZA
#> 47                                                     Bahrain          BHR
#> 48                                                       Qatar          QAT
#> 49                                                     Reunion          REU
#> 50                                                      Belize          BLZ
#> 51                                                      Guyana          GUY
#> 52                                                   Lithuania          LTU
#> 53                                                 Sudan (the)          SDN
#> 54                                                     Somalia          SOM
#> 55                                                 Puerto Rico          PRI
#> 56                                                        Peru          PER
#> 57                                                       Egypt          EGY
#> 58                                                      Bhutan          BTN
#> 59                              Northern Mariana Islands (the)          MNP
#> 60                                                  Antarctica          ATA
#> 61                                                     Nigeria          NGA
#> 62                                                 Isle of Man          IMN
#> 63                                                     Germany          DEU
#> 64                                                    Guernsey          GGY
#> 65                                      Bosnia and Herzegovina          BIH
#> 66                                                    Colombia          COL
#> 67                                                     Finland          FIN
#> 68                                                Burkina Faso          BFA
#> 69                                  United Arab Emirates (the)          ARE
#> 70                                                   Greenland          GRL
#> 71                                                        Iraq          IRQ
#> 72                                                       Macao          MAC
#> 73                                                    Cambodia          KHM
#> 74                                       Sao Tome and Principe          STP
#> 75                                                    Malaysia          MYS
#> 76                                                       Spain          ESP
#> 77                                                    Mongolia          MNG
#> 78                                                     Albania          ALB
#> 79                                                     Ecuador          ECU
#> 80                                                      Canada          CAN
#> 81                                Netherlands (Kingdom of the)          NLD
#> 82                                                       Aruba          ABW
#> 83                                        Cayman Islands (the)          CYM
#> 84                              Central African Republic (the)          CAF
#> 85                                                 El Salvador          SLV
#> 86                                              Western Sahara          ESH
#> 87                                                       India          IND
#> 88                            Bolivia (Plurinational State of)          BOL
#> 89                                                     Denmark          DNK
#> 90                                                     Austria          AUT
#> 91                                                Turkmenistan          TKM
#> 92                                                  Kazakhstan          KAZ
#> 93                                                     Hungary          HUN
#> 94                                                    Zimbabwe          ZWE
#> 95                                                   Australia          AUS
#> 96                           Heard Island and McDonald Islands          HMD
#> 97  United Kingdom of Great Britain and Northern Ireland (the)          GBR
#> 98                                                     Tunisia          TUN
#> 99                                                      Uganda          UGA
#> 100                                                    Armenia          ARM
#>     alpha_2_code
#> 1             PY
#> 2             MZ
#> 3             SB
#> 4             DO
#> 5             TV
#> 6             IS
#> 7             JM
#> 8             CC
#> 9             NI
#> 10            SN
#> 11            TR
#> 12            SX
#> 13            RS
#> 14            AD
#> 15            BM
#> 16            TF
#> 17            GF
#> 18            LU
#> 19            PM
#> 20            CU
#> 21            PA
#> 22            DM
#> 23            TD
#> 24            UY
#> 25            SJ
#> 26            CL
#> 27            MF
#> 28            BG
#> 29            SZ
#> 30            VC
#> 31            AS
#> 32            NU
#> 33            NF
#> 34            TW
#> 35            SE
#> 36            TC
#> 37            MV
#> 38            IO
#> 39            RO
#> 40            TG
#> 41            AG
#> 42            MA
#> 43            AR
#> 44            NE
#> 45            KI
#> 46            TZ
#> 47            BH
#> 48            QA
#> 49            RE
#> 50            BZ
#> 51            GY
#> 52            LT
#> 53            SD
#> 54            SO
#> 55            PR
#> 56            PE
#> 57            EG
#> 58            BT
#> 59            MP
#> 60            AQ
#> 61            NG
#> 62            IM
#> 63            DE
#> 64            GG
#> 65            BA
#> 66            CO
#> 67            FI
#> 68            BF
#> 69            AE
#> 70            GL
#> 71            IQ
#> 72            MO
#> 73            KH
#> 74            ST
#> 75            MY
#> 76            ES
#> 77            MN
#> 78            AL
#> 79            EC
#> 80            CA
#> 81            NL
#> 82            AW
#> 83            KY
#> 84            CF
#> 85            SV
#> 86            EH
#> 87            IN
#> 88            BO
#> 89            DK
#> 90            AT
#> 91            TM
#> 92            KZ
#> 93            HU
#> 94            ZW
#> 95            AU
#> 96            HM
#> 97            GB
#> 98            TN
#> 99            UG
#> 100           AM
```
