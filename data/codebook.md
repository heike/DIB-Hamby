
# CODEBOOK 


## Common Fields
These fields are identifiers used across multiple files; they will be described once here.

Name | Type | Description 
---- | ---- | -----------
land | character | Index from 1-6 describing the sequential position of the land on the bullet during the scanning process. 
study | character | Identifier indicating which test kit the scans were obtained from
barrel | character | Identifier indicating which of the 10 barrels used in the study the bullet was fired from. May be Unk, indicating that the bullet is a questioned bullet that examiners are meant to match to a known barrel (knowns are numbered 01 - 10).
bullet | character | Bullet Identifier. If known, this will be 1 or 2; if unknown, this will be A-Z. 
damaged | logical | Indicates whether one of the land engraved areas shows visible damage such as pitting (holes in the bullet surface from gun powder exploding in the barrel next to it) or "tank rash" (damage to the bullet after it exited the barrel after the firing). Assigned by the authors.

The first 4 fields are combined to make a unique identifier for each scan.


## Meta Information (`meta_info.csv`)

- Number Of Variables/Columns: 10
- Number Of Cases/Rows: 420
- Missing Data Codes: NA


Name | Type | Description 
---- | ---- | -----------
source | file path | path to the x3p file corresponding to the LEA
land\_id | character | identifier of a land engraved area of the form `study\_barrel\_bullet\_land`
barrel_index | integer | numeric identifier containing ground truth for unknown bullets, and the numerical equivalent of barrel for known bullets
land_index | integer | land identifier corresponding to bullet 1 of Hamby 173 for the corresponding barrel.
H173\_B1\_index | character | matching land id to bullet 1 of the Hamby 173 set of the barrel the bullet was fired from.  This index enables to determine ground truth in any comparison: two land engraved areas come from the same source if their corresponding `H173\_B1\_index` matches.



## Comparison Information (`hamby_comparisons.csv`)

- Number Of Variables/Columns: 41
- Number Of Cases/Rows: 81003
- Missing Data Codes: NA


Name | Type | Description 
---- | ---- | -----------
land{1,2} | character | unique identifiers for the scans in the comparisons
study{1,2} | character | see above
barrel{1,2} | character | see above
bullet{1,2} | character | see above
l{1,2} | character | see above (land)
ccf | numeric | maximized cross-correlation between two LEA signatures, 
rough\_cor | numeric | correlation after detrending aligned signatures.\\
length | numeric | length of the shorter of two signatures -- as number of vector entries.
length\_mm | numeric | length of the shorter of two signatures converted to millimeter taking resolution into account.
lag | numeric | lag between signatures to achieve alignment (as number of vector entries)
lag\_mm | numeric | lag between signatures to achieve alignment converted to mm taking resolution into account 
abs\_lag | numeric | absolute lag between signatures to achieve alignment. 
abs\_lag\_mm | numeric | absolute lag between signatures to achieve alignment converted to mm taking resolution into account 
overlap | numeric | ratio of overlap between the signatures after alignment. The length of the shorter of the two signatures is used as denominator in the ratio. 
matches, matches\_per\_mm | numeric | number of matching peaks between aligned signatures. Matching in this context is defined as the same (or similar) location along the $x$ axis. _per_mm variant is normalized by the length of the aligned sequence.
mismatches, mismatches\_per\_mm | numeric | number of peaks on a signature that does not have a counterpart peak in close proximity along the $x$ axis on the aligned signature. _per_mm variant is normalized by the length of the aligned sequence.
cms, cms\_per\_mm | numeric | CMS are \underline{c}onsecutively \underline{m}atching \underline{s}triae \citep{biasotti} between two aligned land engraved areas (LEAs). Striations, as defined by firearms and toolmarks examiners, exhibit as peaks in the  corresponding signature. Because confocal light microscopy also captures depths, we can expand the notion of striations to peaks and valleys. `cms` is defined as the number of consecutively matching peaks and valleys of two aligned signatures, while `cms2` is the number of just the peaks. _per_mm variant is normalized by the length of the aligned sequence.
cms2, cms2\_per\_mm | numeric | `cms2` includes only the number of peaks, corresponding to a measure similar to that used by firearms and toolmark examiners. _per_mm variant is normalized by the length of the aligned sequence.
non\_cms, non\_cms\_per\_mm | numeric | longest sequence of non-matching peaks between two aligned signatures. _per_mm variant is normalized by the length of the aligned sequence.
left\_cms, right\_cms | numeric | longest sequence of matching striae from the left/right of two aligned signatures. 
D, sd\_D | numeric | Euclidean distance (in millimeters) between two aligned signatures and its standard deviation. 
sum\_peaks | numeric | the depth of peaks measured as the sum of matching peaks between two aligned signatures (in microns).
samesource | logical |  ground truth whether a pair is from the same source (TRUE) or from different sources (FALSE). 

## Random Forest Model Object (`csafe_rf2.rds`)

This data structure is the fitted model object created when the following code is run in R:

```
library(randomForest)
# Read in the features
features <- read.csv("hamby-comparisons.csv")

set.seed(20140105)
# fit the random forest
csafe_rf2 <- randomForest(
  factor(samesource) ~ ccf + rough_cor + matches_per_mm + D + sd_D +
    cms_per_mm + mismatches_per_mm + overlap + abs_lag_mm + sum_peaks,
  data = features)

saveRDS(csafe_rf2, "csafe_rf2.rds")
```

The object is a list containing the following components:

- call: the call used to fit the model object
- type: the type of tree fit (regression or classification)
- predicted: predictions for each of the values in the input data
- err.rate: Out-of-bag errors for each of the trees in the random forest
- confusion: confusion matrix for the random forest
- votes: records of the vote percentage for each of the rows in the input data
- oob.times: number of times each row was part of the out-of-bag sample
- classes: the two classes used in the classification tree
- importance: variable importance information
- importanceSD: not saved
- localImportance: not saved
- proximity: not saved
- ntree: number of trees in the forest
- mtry: number of variables randomly sampled as candidates at each split.
- forest: Object containing the trees which make up the random forest
- y: response variable from the input data
- test: not saved
- inbag: not saved
- terms: Terms from the model formula

This object is of class randomForest and can be manipulated using functions from the `randomForest` R package. 