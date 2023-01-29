# DIB-Hamby

Data in Brief for the Random Forest based on Hamby sets 173 and 252

## Repository structure

    ├── code
    ├── data
    ├── images
    ├── writeup



## Code

There are three R scripts in the code folder:

    ├── code
    │   ├── create-features.R
    │   ├── fitting-randomforest.R
    │   └── publishing-data.R

#### code/create-features.R

**input:**

-   (automatically triggered) download of the x3p files from the [NIST Ballistics Toolmark Database](https://tsapps.nist.gov/NRBTD/Studies/Search)
-    file `data/meta-info.csv` with meta information for each scan, such as (manual) groove locations of each profile, and an identifier with ground truth for all land-to-land comparisons
    -   if crosscut or groove locations do not yet exist, Human-in-the-middle algorithms are run to derive these values.

**output:\
**\
`data/hamby-comparisons.csv`, this file contains features based on pairwise comparisons of striation marks on bullet lands\

#### code/fitting-randomforest.R

**input:**\
\
`data/hamby-comparisons.csv` (as created in code/create-features.R)

**output:**\
\
`data/csafe-isu2.rds` a binary object of a random forest for scoring the similarity of pairs of lands\

#### code/publishing-data.R

**input:**

-   `data/hamby-comparisons.csv`
-   `data/csafe-isu2.rds`

**output:**

This script re-creates all figures in file `writeup/hamby-dib.Rnw`
