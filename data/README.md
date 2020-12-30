## GENERAL INFORMATION 

*This section establishes basic information about the data, particularly the ???who??? and ???what???.* 

DATA TITLE: Comparison Features Data from pairs of Bullet Land Engraved Areas
PROJECT TITLE: CSAFE Firearms and Toolmark Analysis
DATA ABSTRACT: 
Land engraved areas (LEAs) on fired bullets form the basis of forensic examinations regarding whether these bullets were fired from the same source (the same barrel).  For algorithmic comparisons of similarity 3d topographic imaging forms the basis of assessing similarity. Here, three parts of the statistical analysis are included: 
(a) descriptive information for each scan, including its suitability for comparisons, (b) statistically derived features characterizing similarity and differences of each pair of topographic LEA scans}, and (c) a fitted Random Forest model object used to predict the similarity of two scans based on their corresponding similarity features.
The original raw data is available from the NIST Ballistics Toolmark Database; here, we provide the code and intermediate data necessary to create the Random Forest model described in ["Automatic Matching of Bullet Land Impressions"](https://doi.org/10.1214/17-aoas1080) to make this work  accessible and reproducible. This also allows future algorithms to be benchmarked against this random forest bullet matching model.

AUTHORS: 

	Author: Heike Hofmann
	ORCID: 0000-0001-6216-5183
	Institution: Iowa State University; Center for Statistics and Applications in Forensic Evidence
	Email: hofmann@iastate.edu

  Author: Susan Vanderplas
	ORCID: 0000-0002-3803-0972
	Institution: University of Nebraska-Lincoln
	Email: susan.vanderplas@unl.edu

  Author: Eric Hare
	ORCID: 0000-0002-4277-3146
	Institution: OmniAnalytics
	Email: ericrhare@gmail.com

  Author: Alicia Carriquiry
	ORCID: 0000-0002-6428-9427
	Institution: Iowa State University; Center for Statistics and Applications in Forensic Evidence
	Email: alicia@iastate.edu

	Corresponding author: Heike Hofmann

ASSOCIATED PUBLICATIONS: 

- Hare, E., Hofmann, H., Carriquiry, A., & others. (2017). Automatic matching of bullet land impressions. The Annals of Applied Statistics, 11(4), 2332???2356. https://doi.org/10.1214/17-AOAS1080
- Vanderplas, S., Nally, M., Klep, T., Cadevall, C., & Hofmann, H. (2020). Comparison of three similarity scores for bullet LEA matching. Forensic Science International. https://doi.org/10.1016/j.forsciint.2020.110167

COLLECTION INFORMATION:

	Time period(s):  
	Location(s): NA

### FILE DIRECTORY 

#### FILE LIST

1. `meta-info.csv` - descriptive information about each raw x3p file containing a 3-dimensional scan of a bullet land surface. 
2. `hamby-comparisons.csv` -  features quantifying the similarity between pairs of 3d topographic surface scans detailing striation marks on land engraved areas (LEAs) of fired bullets. Features were extracted using the R software packages [x3ptools](github.com/heike/x3ptools@164e631) and [bulletxtrctr](github.com/heike/bulletxtrctr@dfa140a) as outlined in the pipeline in Hare et al (2017) and detailed in the accompanying R script. From available meta information ground truth regarding source is added for each comparison. 
3. `csafe_rf2.rds` - the fitted random forest model object, containing all of the component trees and the features used to fit the random forest. This model uses `hamby-comparisons.csv` as training data to predict same source pairs and different source pairs. Fitted using R package randomForest 4.6-14
4. `codebook.md` - a codebook describing variable names and associated information.
5. `create-features.R` - code which generates the features in `hamby-comparisons.csv` using `x3ptools` and `bulletxtrctr`
6. `fitting-randomforest.R` - code which fits the random forest using `hamby-comparisons.csv`


## METHODS AND MATERIALS

### DATA COLLECTION METHODS 

The random forest model provided in this paper is fitted based on two sets of bullets from a study conducted by Hamby and Brundage.

The raw scans can be downloaded from the [NBTRD](https://tsapps.nist.gov/NRBTD/Studies/Search) by searching for ``Hamby (2009) Barrel Set" and selecting the two study results matching that name. The studies should be downloaded separately and each placed into a folder. Our code assumes that the files have the following file structure:

- NBTRD
    - Hamby 173 (2009) Barrel
        - bullets
            - ... 210 x3p files ...
    - Hamby 252 (2009) Barrel
        - bullets
            - ... 210 x3p files ...

The scans from both studies are approximately __ GB in size. 

### DATA PROCESSING METHODS 

The script `create-features.R` contains the full set of data processing R commands. 

Here, we provide a very brief description of the steps taken to preprocess the scans and produce bullet signatures for comparison.

1. The scans are oriented so that the base of the bullet is at the bottom and the bullet scan is curving from left to right. Fired bullets have \emph{breakoff}, where some of the metal flies off of the bullet during firing as a result of heat and pressure. The scans are oriented such that any breakoff should be located in the bottom right corner of the scan. For Hamby 173, bullets were flipped along the $y$ axis; for Hamby 252, the bullets were rotated counterclockwise by 90 degrees and then flipped along the $y$ axis. Finally, scans are converted so that all measurements are in $\mu$m.
2. Once scans are oriented properly, a stable cross-section (one that does not cross through any regions of breakoff) that is as low as possible on the bullet (so as to cross deeper, more well-engraved stria) is identified algorithmically using the cross-correlation between adjacent cross-sections to quantify stability. These cross sections are checked manually using graphics and are set by hand when the algorithmic identification fails. In this dataset, there were no manually identified cross sections.
3. Using the cross-section location identified in step 2, the measurements at that crosscut are extracted from the scan.
4. Damaged scans not suitable for comparison are marked.
5. The grooves that demarcate the land engraved area are identified algorithmically and then manually checked and set as necessary using a Shiny app (code is provided in the file).
6. The areas between the grooves are extracted and a smooth is fit to the data to remove the bullet surface curvature, producing a "bullet signature". This signature is used to represent the entire bullet land engraved area scan.
7. Once signatures have been identified, each scan is compared to every other scan to produce a set of pairwise comparisons. The signatures in each pairwise scan are identified and features are computed using the `bulletxtrctr` software describing how well the two scans are aligned.
\item The ground truth is included in the feature set for evaluating the prediction algorithm.
\item Duplicate pairwise comparisons are removed so as not to compromise the model fitting process.
\end{enumerate}

### SOFTWARE

*This section informs users about any specialized software used to generate/process the data or needed to use the data. Create a new entry for each program and/or add-on.*
 
Name:
Version:
System Requirements:
URL:
Developer:
Additional Notes: 

### EQUIPMENT
*If your data was generated by equipment or a tool running special software you should record its information here. Create a new entry for each piece of 
equipment.*

Manufacturer:
Model:
Embedded Software/Firmware Name: (if applicable)
Embedded Software/Firmware Version: (if applicable)
Additional Notes:

### LICENSING 

*This section should include a statement on how other people can access, reuse, and/or redistribute the data, i.e. licensing information. Template language for CC-BY and CC-0 licenses are included below, delete the license information that does not apply.* 

This work is licensed under the Creative Commons Attribution (CC-BY) 4.0 International License. 
For more information visit: [https://creativecommons.org/licenses/by/4.0 ](https://creativecommons.org/licenses/by/4.0)


This work is licensed under the Creative Commons Universal Public Domain Dedication (CC0 1.0) License. 
For more information visit: [https://creativecommons.org/publicdomain/zero/1.0/ ](https://creativecommons.org/publicdomain/zero/1.0/)

