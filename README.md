# DIB-Hamby
Data in Brief for the Random Forest based on Hamby sets 173 and 252


## Code

there are three R scripts in the code folder:

code/create-features.R

input: 
- downloads the x3p files from the NIST Ballistics Toolmark Database 
- file data/. with meta information, such as (manual) groove locations of each profile and land-to-land matches
output: data/hamby-comparisons.csv, this file contains features based on pairwise comparisons of striation marks on bullet lands
