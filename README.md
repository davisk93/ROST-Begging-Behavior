# Begging Behavior as an Honest Signal of Need and Parent-Offspring Association During the Post-fledging Dependency Period

### Davis, K.L., S.M. Karpanty, J.A. Spendelow, J.B. Cohen, M.A. Althouse, K.C. Parsons, C.F. Luttazi. 

### Ecology and Evolution

### Code/Data DOI:[![DOI](https://zenodo.org/badge/149470858.svg)](https://zenodo.org/badge/latestdoi/149470858)

### Please contact the first author for questions about the data or code: Kayla Davis (davisk93@msu.edu)
_______________________________________________________________________________________________________________________________________
## Abstract
Honest signaling mechanisms can function to appropriate care to hungry offspring and avoid misdirected care of unrelated offspring. Begging, the behavior by which offspring solicit food and parental care, may be an honest signaling mechanism for need, as well as association of parents and offspring. Roseate terns (Sterna dougallii) exhibit prolonged parental care during the post-breeding staging period, offering an ideal system in which to study begging as an honest signaling mechanism. We conducted focal sampling during two pre-migratory staging seasons (2014 and 2015) at Cape Cod National Seashore, MA to determine whether post-fledging tern begging behavior was an honest signal for need and parent-offspring association. Based on honest signaling theory, we expected begging behavior to be highest during times of high perceived need, and we expected to see a decrease in begging behavior as young terns became increasingly independent of the care-giving parent. Also, we predicted that young terns would be more likely to beg at parents than non-parents. We found that young roseate terns begged at their parents more often than non-parents; however, they did not always beg at parents. Model predictions of begging probability showed a linear relationship between begging and time of day and date of season, such that begging increased with time of day and decreased with date of season, respectively. Our results provide evidence for honest parent-offspring interactions and are inconsistent with parent-offspring conflict theory but suggest that begging may play a complex role in post-fledging parent-offspring interactions.    
## Data
[HY_Parent_Begs.csv](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/HY_Parent_Begs.csv): This file contains begging interactions between uniquely marked HY ROST "HY" and adult ROST "Receiving". Known parents of each HY are included in "Adult". If HY ROST begging behavior was directed at a known parent, the "Parent" column has a "1" and "0" otherwise. 

[ROST_BegData.csv](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/ROST_BegData.csv): This file contains data from 970 focal samples from 664 uniquely marked HY ROST. 
"PFR": HY ROST unique ID;
"SITE": Study site at Cape Cod National Seashore where focal sample recorded;
"DayNum": Day of season that focal sample recorded;
"HOUR": Hour of sample;
"TIME": Time of sample;
"BEG": Total number of begging events recorded during the five-minute focal sample.

[ROST_BegBandingData_Github.csv](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/ROST_BegBandingData.csv): This file contains known banding dates for 504 HY ROST. These data are used to test the correlation between HY ROST banding date and days since first observation on the Cape Cod staging grounds. Results of this analysis are described in Appendix 1 of the publication. 
## Code
[ROST_BegAnalysis](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/ROST_BegAnalysis.R): R code to complete begging behavior analyses
