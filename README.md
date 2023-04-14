## LEVERAGING LOCAL EFFORTS TO SOLVE REGIONAL-SCALE ECOLOGICAL QUESTIONS: USING MULTIPLE SOURCES OF DATA AND A MULTI-SPECIES OCCUPANCY MODEL TO EXPLORE BEE-PLANT INTERACTIONS

### [MICHELLE J. LEE](https://michelleleelifesci.weebly.com/), [GRAZIELLA V. DIRENZO](https://grazielladirenzo.weebly.com), & [KATJA C. SELTMANN](https://orcid.org/0000-0001-5354-6048)

### In prep

[```Abstract```](#Abstract) / [```Methods```](#Methods) / [```Interaction Dataset```](#interaction-dataset) / [```Other Datasets```](#other-datasets) /  [```Repository Directory```](#repository-directory)

### Please contact either Michelle or Grace for questions about the code or data
__________________________________________________________________________________________________________________________________________

### Abstract: 
Background: We are trying to account for sampling bias of museum records across taxonomy. The question we are focusing on answering is: Do generalist bees have more parasites than solitary bees?

### Methods: 
We are using an N-mixture model, with bee species as our "sites" and study as out "survey replicates". The data set consist of presence-only data.

## Interaction Dataset: 
Data used from the Global Biotic Interactions website are constantly being updated. The version used for this paper were accessed on 14 September 2021. The data file can be found on https://zenodo.org/record/7753956 and can be downloaded directly. This file is not included in our github workflow as the files are too large. To run the following code, this data should be downloaded first.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7753956.svg)](https://doi.org/10.5281/zenodo.7753956)


## Other Datasets: 
Datasets used in this analysis are listed below.

1. Bee traits for checklist species - coloration_NEW.csv

A file that includes color patters of bees coded by Yolanda Diao from bee images.

2. Bee traits for checklist species - size.csv

A file of bee size as reported in (citations)

3. Bee traits for checklist species - sociality.csv
4. Globi-names-not-in-discoverlife - Sheet1 2022 04 01.csv
5. SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv

Citations for SCI bee checklist (SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv)  
a. Seltmann, K. C., Dewey, D., McLaren, L., & Thrift, C. (2022). Native and non-native bees (Anthophila) of Santa Cruz Island: An annotated checklist. Zenodo. https://doi.org/10.5281/zenodo.7216845  
b. GBIF citation  
  
  
6. citation-list-type.csv
_A file that includes a list of citation type for each interaction._
  
  
7. discoverlife-Anthophila-2022 04 01.csv
_A file that includes bee species names, resolved names, and relevant sources._

Citations for bee names (discoverlife-Anthophila-2022 04 01.csv)  
a. Ascher, J. S., & Pickering, J. (2020). Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). https://www.discoverlife.org/mp/20q?guide=Apoidea_species
  
  
8. final-globi-citations-unique 2022 01 21.csv  
_A file that includes the filtered down list of unique citations that have contributed entries to the final interaction dataset used for analyses._
  
  
9. institutioncodes_2021_12_16.csv  
_A file that includes all institutions included in the Symbiota Collections of Arthropods Network (SCAN) that can be merged with contributed entries in GloBI dataset. This matches allow us to attribute collections to different organizations._

SCAN website: https://scan-bugs.org/portal/
  
  
10. short_noapis_resolvedplantsci_041122.csv  
_A file that includes the published (as of 2022) Santa Cruz Island plant checklist. Additional phenology data was collected from Calflora and addiitonal trait data were coded using photographs on Calflora and the Jepson eFlora by Michelle Lee. The full plant list was then filtered down to only include plant species in genera that had a recorded interaction with the bees in our GloBI dataset._

Citations for plantlist (short_noapis_resolvedplantsci_041122.csv)  
a. Hasenstab-Lehman, K.E., Guilliams, C.M., & Junak, S.A. (2022). Checklist of the Vascular Flora of Santa Cruz Island, California, Version 2  
b. Calflora: Information on California plants for education, research and conservation. (2022). Berkeley, California: The Calflora Database [a Non-Profit Organization]. https://www.calflora.org  
c. Jepson Flora Project (eds.) 2022. Jepson eFlora, https://ucjeps.berkeley.edu/eflora/  

## Repository Directory
### [Code](https://github.com/lee-michellej/globi_tritrophic_networks/tree/master/Code): Contains code for modeling, analysis, and results
### [Data](https://github.com/lee-michellej/globi_tritrophic_networks/tree/master/Data): Contains CSV file of raw data
### [Model](https://github.com/lee-michellej/globi_tritrophic_networks/tree/master/Model): Contains all material published online
### [ModelOutput](https://github.com/lee-michellej/globi_tritrophic_networks/tree/master/ModelOutput): Contains all material published online
