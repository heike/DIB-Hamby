# DIB-Hamby
Data in Brief for the Random Forest based on Hamby sets 173 and 252


## Code

there are three R scripts in the code folder:

code/create-features.R

input: 
- downloads the x3p files from the NIST Ballistics Toolmark Database 
- file data/meta-info.csv with meta information for each scan, such as (manual) groove locations of each profile, and an identifier with ground truth for all land-to-land comparisons

output: data/hamby-comparisons.csv, this file contains features based on pairwise comparisons of striation marks on bullet lands


code/fitting-randomforest.R

input: data/hamby-comparisons.csv (as created in code/create-features.R)
output: data/csafe-isu2.rds a binary object of a random forest for scoring the similarity of pairs of lands


code/publishing-data.R

this file contains all of the code to re-create figures and tables in file writeup/hamby-dib.Rnw
