# BGE Species priorization
## Background
The Biodiversity Genomics Europe (BGE) project aims to sequence the critical biodiversity by a community-driven approach. In the task “T5.5 Critical biodiversity community sequencing”, the scientific community is invited to nominate species for the sequencing of a reference genome by BGE. Such an open call format results in more species nominations than can be sequenced with a limited budget/framework. Hence, a species selection process is necessary. The selection process is a crucial part of all large-scale genome projects. Initiatives might be initially and intentionally limited to taxonomic target groups (such as to vertebrates in VGP, the Vertebrate Genomes Project) or bioregions (DToL, Darwin Tree of Life). Projects might allow for species suggestions from the larger research and public community; however, the selection process can include subjective elements like motivation letters. Further, selection criteria are generally not developed by the larger community. This might generate intrinsic, undesirable biases towards iconic, economically important and better-known species or taxa, while lesser-known taxa might be neglected or regarded as less important. This bias is clearly present in present-day taxonomic representation of genomic resources across the eukaryotic tree of life. However, the goals of EBP, of BGE and fighting the biodiversity crisis require consideration of the whole biodiversity independent of the celebrity of individual taxa.

To this aim, BGE approached its species selection process differently. The larger ERGA community was involved in developing the prioritisation process, it is based on explicit and objective criteria and the process is semi-automated to avoid human intervention in the decision process. Human intervention is necessary only in the curation of the database to ensure the automatization of the process. 

The details of the species selection process will be described in a separate paper and is shown schematical in the pdf file "FlowchartSpeciesSelectionProcess". In brief, it is a four-stage process including (1) an exclusion stage, (2) a prioritisation stage employing a decision-tree model and additional ranking to ensure country and researcher representation, (3) a feasibility check with additional adjustment for genera with multiple species suggestions and (4) a final check of legal compliance. The species selection process is based on a total of 28 criteria. In the paper, we will explain each criterion in detail, to which stage it has been assigned and how it is applied in the selection process. The process generates lists of feasible species (selected, on waiting list or non-selected species), non-feasible species and excluded species (exclude due to lack of a voucher specimen or the presence of a reference genome or an ongoing reference-genome project).

## Implementation
The species selection process is run by R script and additional relevant information needs to be provided by the user while it is running. Therefore, in RStudio the script should be run using "Source". The R script is provided here. 

It will complete the first three stages of the selection process outlined in the flow chart.



The input file is an excel sheet as the provided example "Suggestions_examples".



Additionally, three files need to be provided:

"Widening_countries.txt" - list of countries that should be prioritized (e.g., Widening countries in the EU)

"Countries_selected.txt" - list of countries selected in previous rounds and which shall be less prioritized than countries not selected before 

"Individuals_selected.txt" - list of individuals (i.e., email) selected in previous rounds and which shall be less prioritized than individuals not selected before

Please note: If any of these three options shall not be applied, leave the file empty except for the first row with the header(s). The files themselves have to be provided for the program to run.




