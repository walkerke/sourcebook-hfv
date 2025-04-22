# QCEW Reference Tables

Last updated: 2025-04-22

Useful links:

* [QCEW Data Files](https://www.bls.gov/cew/downloadable-data-files.htm)
* [QCEW Data File Documentation Guide](https://www.bls.gov/cew/about-data/documentation-guide.htm)
* [QCEW Open Data Access: CSV Data Slices](https://www.bls.gov/cew/additional-resources/open-data/csv-data-slices.htm)
* [Release Calendar](https://www.bls.gov/cew/release-calendar.htm)

## File Layouts

### Quarterly Layout

`naics-based-quarterly-layout-csv.csv`

| Column Name         | Type | Example               |
|---------------------|------|-----------------------|
| `field_number`      | Num  | 1                     |
| `field_name`        | Chr  | area_fips             |
| `data_type`         | Chr  | Text                  |
| `max_length`        | Num  | 5                     |
| `field_description` | Chr  | 5-character FIPS code |

Documentation:

<https://www.bls.gov/cew/about-data/downloadable-file-layouts/quarterly/naics-based-quarterly-layout.htm>

> *This table describes the file layout for NAICS-based, quarterly CSV files. There are four types of quarterly CSV files. There are by-area, by-industry, singlefile, and size files. These files are available from the historical [NAICS-Based Data Files Table](https://www.bls.gov/cew/downloadable-data-files.htm#NAICS_BASED). The quarterly CSV files have several title fields. These fields, fields #9 through #13, are excluded from the singlefiles.*

### Annual Layout

`naics-based-annual-layout-csv.csv`

| Column Name | Type | Example |
|-----------------------|---------------|-----------------------------------|
| `field_number` | Num | 14 |
| `field_name` | Chr | annual_avg_emplvl |
| `data_type` | Chr | Numeric |
| `max_length` | Num | 8 |
| `field_description` | Chr | Annual average of monthly employment levels for a given year |

Documentation:

<https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm>

> *This table describes the file layout for NAICS-based, CSV files containing annual averages. There are three types of annual CSV files. There are annual files by area, by industry, and as a singlefiles. These files are available from the historical [NAICS-Based Data Files](#0) table. The annual CSV files have several title fields. These fields, fields #9 through #13, are excluded from the singlefiles.*

## Ownership Titles

`ownership-titles-csv.csv`

| Column Name | Type | Example |
|-------------|------|---------|
| `own_code`  | Num  | 5       |
| `own_title` | Chr  | Private |

Documentation:

<https://www.bls.gov/cew/classifications/ownerships/ownership-titles.htm>

> *These ownership codes are to be used with NAICS coded QCEW data. A subset of these codes are also used in the SIC based data.*

## Aggregation Level Codes

`agg-level-titles-csv.csv`

| Column Name    | Type | Example                                     |
|----------------|------|---------------------------------------------|
| `agglvl_code`  | Num  | 74                                          |
| `agglvl_title` | Chr  | County, NAICS Sector -- by ownership sector |

> [!TIP]
> Aggregation level codes in this table differ by geographic level. For data reported at the county level, valid values include `70` (total county employment) through `78` (NAICS 6-digit level).

Documentation:

<https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm>

> *These are the codes used on summary records produced by the Quarterly Census of Employment and Wages (QCEW) program to indicate the aggregation level of the data summarized on the record. These aggregation level codes are for QCEW records coded to the North American Industry Classification System (NAICS).*

## Industry Codes and Titles

`industry-titles.csv`

| Column Name      | Type | Example                                      |
|------------------|------|----------------------------------------------|
| `industry_code`  | Num  | 8111                                         |
| `industry_title` | Chr  | NAICS 8111 Automotive repair and maintenance |

Documentation:

<https://www.bls.gov/cew/classifications/industry/industry-titles.htm>

> *The QCEW program uses the [North American Industry Classification System](https://www.bls.gov/cew/classifications/industry/home.htm) (NAICS) as to assign establishments to industries and to report industry data at highly detailed as well as at aggregated levels.*

## High-Level Industry Titles and NAICS Crosswalk

Documentation:

<https://www.bls.gov/cew/classifications/industry/high-level-industries.htm>

> *The highest levels of aggregation in the North American Industry Classification System (NAICS) are NAICS Sectors. The QCEW aggregates data to three higher levels in order to provide comparability to other programs that have less detailed data. These levels are the SuperSector level, the twelve of which represent combinations of from one to four NAICS Sectors, the Domain level. Two of which represent combinations of three and nine Supersectors, the overall all industry total. While these aggregates do not have official NAICS codes, the QCEW program assigns them codes to provide ready reference. The codes and their corresponding titles are shown in the QCEW High-Level Industry Titles table below. The NAICS codes that make up each SuperSector are shown in the second table below titled QCEW High-Level Industry Crosswalk Table.*

### High-Level Industry Titles

`high-level-industries-csv.csv`

| Column Name      | Type | Example              |
|------------------|------|----------------------|
| `Industry Code`  | Num  | 1023                 |
| `Industry Title` | Chr  | Financial Activities |

### High-Level Industry Crosswalk

`high-level-crosswalk-csv.csv`

| Column Name    | Type | Example                                     |
|----------------|------|---------------------------------------------|
| `total`        | Chr  | 10 Total, All Industries                    |
| `domain`       | Chr  | 102 Service-Providing                       |
| `super_sector` | Chr  | 1023 Financial Activities                   |
| `naics_sector` | Chr  | NAICS 53 Real Estate and Rental and Leasing |

## NAICS Hierarchy Crosswalk

`qcew-naics-hierarchy-crosswalk.xlsx`

-   Sheet 1: `v2022`

-   Sheet 2: `v2017`

-   Sheet 3: `v2012`

-   Sheet 4: `v2007`

-   Sheet 5: `v2002`

| Column Name         | Type | Example                           |
|---------------------|------|-----------------------------------|
| `naics6_code`       | Num  | 624410                            |
| `naics6_title`      | Chr  | Child Day Care Services           |
| `naics5_code`       | Num  | 62441                             |
| `naics5_title`      | Chr  | Child Day Care Services           |
| `naics4_code`       | Num  | 6244                              |
| `naics4_title`      | Chr  | Child Day Care Services           |
| `naics3_code`       | Num  | 624                               |
| `naics3_title`      | Chr  | Social Assistance                 |
| `sector_code`       | Num  | 62                                |
| `sector_title`      | Chr  | Health Care and Social Assistance |
| `supersector_code`  | Num  | 1025                              |
| `supersector_title` | Chr  | Education and Health Services     |
| `domain_code`       | Num  | 102                               |
| `domain_title`      | Chr  | Service-Providing                 |

Documentation:

<https://www.bls.gov/cew/classifications/industry/qcew-naics-hierarchy-crosswalk.htm>

> *QCEW has created a full-industry crosswalk for all versions of the North American Industry Classification System (NAICS), with the most recent version (NAICS 2022) included in the table below, along with High-Level BLS industries. This is a tool created to help data users quickly reference industry relationships while using QCEW database options. QCEW is also providing a downloadable option of this table for data users, along with additional historical crosswalks for previous versions of NAICS.*
