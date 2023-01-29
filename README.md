# DIB-Hamby

Data in Brief for the Random Forest based on Hamby sets 173 and 252

## Repository structure

The four main elements to this repository are the four folders: `data`, `images`, `code`, and `writeup`. The contents of each of these folders is described in detail below.

### Images

The `images` folder contains a total of 420 images in png format. These images are renderings from the original topographic scans of land engraved areas from fired bullets.  The renderings are overlaid by annotations from the matching procedure. The white lines correspond to the profile used for comparisons. 
All images are checked (manually) for problems with the physical bullet (such as tank rash or holes), that make the scans unsuitable for automatic comparisons, and problems stemming from the matching algorithm, such as an unfortunate choice of crosscut. 
The images with identified problems are moved into the corresponding folders.

    ├── images
    │   ├── crosscut
    │   ├── damaged
    │   ├── Hamby173-Br1-B1-L1.png
    .   .
    .   .
    .   .
    │   ├── Hamby173-BrUkn-BX-L6.png
    │   ├── Hamby252-Br1-B1-L1.png
    .   .
    .   .
    .   .
    │   ├── Hamby173-Br1-B1-L1.png


### Code

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
