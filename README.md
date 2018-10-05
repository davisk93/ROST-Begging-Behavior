# Begging Behavior as an Honest Signal of Need and Parent-Offspring Association in Roseate Terns 

Davis, K.L., S.M. Karpanty, J.A. Spendelow, J.B. Cohen, M.A. Althouse, K.C. Parsons, C.F. Luttazi. In Review. Begging behavior as an honest signal of need and parent-offspring association in roseate terns. Behavioral Ecology.

### Please contact the first author for questions about the data or code: Kayla Davis (davisk93@msu.edu)
_______________________________________________________________________________________________________________________________________
## Abstract
Honest signaling mechanisms between parents and offspring, such as begging, function to direct appropriate care to hungry offspring and avoid misdirected care of unrelated offspring. Roseate terns (Sterna dougallii) exhibit prolonged parental care by a single care-giving parent during the post-breeding staging period at Cape Cod, MA. During the staging period, hatch-year roseate terns are dependent on the attending parent for food because they cannot fish for themselves. We used focal sampling methods during two pre-migratory staging seasons (2014 and 2015) at Cape Cod National Seashore, MA to determine whether hatch-year roseate tern begging behavior was an honest signal for need and parent-offspring association. Based upon honest signaling theory, we expected begging behavior to be highest during times of high perceived need, such as the end of the day before roseate terns departed daytime staging sites for overnight roost sites. In addition, we expected to see a decrease in begging behavior as hatch-year roseate terns became increasingly independent of the care-giving parent over the course of a staging season. Finally, we predicted that hatch-year roseate terns would be more likely to beg at parents than non-parents, as would be expected if begging was an honest signal for parent-offspring association. We found that hatch-year roseate terns begged at their parents more often than would be expected if begging was random; however, hatch-year roseate terns did not always beg only at parents (86.0% ± 0.004). Model-averaged predictions showed a quadratic relationship between begging and time of day and day of season, such that probability of begging increased with time of day and decreased with day of season. Our results presumably indicate higher hunger levels or perceived need at the end of the day and increasing hatch-year roseate tern independence late in the season, respectively  and are consistent with existing parent-offspring conflict theory.  
## Data
[HY_Parent_Begs.csv](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/HY_Parent_Begs.csv): This file contains begging interactions between uniquely marked HY ROST "HY" and adult ROST "Receiving". Known parents of each HY are included in "Adult". If HY ROST begging behavior was directed at a known parent, the "Parent" column has a "1" and "0" otherwise. 

[ROST_BegData.csv](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/ROST_BegData.csv): This file contains data from 970 focal samples from 664 uniquely marked HY ROST. 
"PFR": HY ROST unique ID;
"SITE": Study site at Cape Cod National Seashore where focal sample recorded;
"DayNum": Day of season that focal sample recorded;
"HOUR": Hour of sample;
"TIME": Time of sample;
"BEG": Total number of begging events recorded during the five-minute focal sample.
## Code
[ROST_BegAnalysis](https://github.com/davisk93/ROST-Begging-Behavior/blob/master/ROST_BegAnalysis.R): R code to complete begging behavior analyses
