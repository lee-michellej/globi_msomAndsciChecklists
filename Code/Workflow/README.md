# Code for leveraging local species data, a global database, and an occupancy model to explore bee-plant interactions

## Authors

Michelle J. Lee☨, (michellejlee@ucsb.edu) Ecology & Evolution and Marine Biology, University of California Santa Barbara, USA 

Graziella V. DiRenzo☨, (gdirenzo@umass.edu) U.S. Geological Survey, Massachusetts Cooperative Fish and Wildlife Research Unit, University of Massachusetts Amherst, USA

Yolanda Diao, Cheadle Center for Biodiversity and Ecological Restoration, University of California Santa Barbara, USA 

Katja C. Seltmann, Cheadle Center for Biodiversity and Ecological Restoration, University of California Santa Barbara, USA


☨ Both authors contributed equally to leading this study


## Information

Repository Type: Program R scripts

Year of Origin:  2025

Year of Version: 2025

Version: 1.0.0

Digital Object Identifier (DOI): https://doi.org/10.5066/P9ZMJ9YD

USGS Information Product Data System (IPDS) no.: IP-152618 (internal agency tracking)


## Citation for Software

Lee, M. J., DiRenzo, G. V., Y. Diao, K. C. Seltmann. Code for leveraging local species data, a global database, and an occupancy model to explore bee-plant interactions. Version 1.0.0; U.S. Geological Survey software release. Reston, VA. DOI:https://doi.org/10.5066/P9ZMJ9YD


## Data associated with Software

Seltmann, K. C., & Global Biotic Interaction Community. (2024). Global Bee Interaction Data (Version v3.0) [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.10552937

Lee, M.J., Diao, C. & Seltmann, K.C. (2024). lee-michellej/globi_msomAndsciChecklists: Manuscript Release (Data) (2.0). Zenodo. https://zenodo.org/records/13510872


## Citation for associated Manuscript

Lee, M. J., DiRenzo, G. V., Y. Diao, K. C. Seltmann. In press. Leveraging local species data, a global database, and an occupancy model to explore bee-plant interactions. Ecological Applications.

## Abstract 

This software release contains 10 R scripts, 1 R markdown file, and 1 folder organized according to the objective (e.g., format the data, model fitting, etc.). The overall objective of the project was to explore the hypotheses related to bee and plant characteristics that relate to ecological (i.e., what characteristics relate to a bee species degree of plant generalization?) and detection (i.e., what characteristics relate to species detectability?) processes.

We used plant and bee species checklists, bee phenology and trait data (i.e., body size, coloration, flight timing), plant phenology and trait data (i.e., flower color, bloom time), and species interaction data from Global Biotic Interactions (Globi Community & Seltmann 2023) before embarking on the statistical analysis. 

We took the following steps to prepare the data:
  (1)	Identifying bee-plant communities
  (2)	Normalized species checklists
  (3)	Compiling bee and plant trait data
  (4)	GloBI data normalization
      - Filtering to checklist plant species and aligning names
      - Aligning bee species names
      - Normalizing source citation information
      - Formatting data for analysis
After the data were prepared, we used an occupancy model to estimate the probability of bee-plant interactions and the total number of bee species interacting with each plant species. To understand the difference between the bee-plant interactions predicted from the occupancy model and the normalized GloBI dataset, we compared network metrics calculated from each bipartite matrix. 

The necessary data files to run all three scripts can be found in Globi Community & Seltmann (2022), whereas the accompanying research article can be found in Lee et al. (202x).


## "0 - RemoveDupsInGloBI.R"

**Details**: The purpose of this code is to take the latest download of Globi data for bees and remove duplicates that have been added due to recent publications and SCAN additions. These artificial duplicates could impact the probabilities of detecting these various interactions. These additions were discovered by Katja Seltmann.

**Outputs**:

  - New Globi file to take into analyses
  
    - file name = all_bee_data_unique_01feb24.tsv

## "1 - SourceTargetStandardization.Rmd" 


**Details**: R script that downloads the data from the repository on Zenodo. The Zenodo repository already has the bee-plant interactions trimmed from the entire Globi dataset. Bee and plant names can be in any order in either the "source" or "target" columns.

**Outputs**:

  - interactions_dup.csv.gz
  
  - interactions.csv.gz
  

## "2 - PlantStandardization_02feb24.R"


**Details**: This code is part of the standardization process for matching the santa cruz island plant list and Globi interactions. The goal is to make a standardized name column for each dataset and then merge the files


**Outputs**: 

  - resolvedplantnamesglobi_12feb24.csv
	

## "3 - Format_dat_checklist_2025_01_22.R"

**Details**: This R script formats the Globi data for the model into a 3D array (bee species x plant species x citation) & in 2D. The objective of the analysis is to determine the number of plants that each bee species interacts with while accounting for sampling bias. We will be using an occupancy model for the analysis. We will account for bee-plant interactions that were not observed. We will use citations & collections are the "replicate surveys" (rather than temporal or spatial surveys). This model does not take time or space into account.

**Outputs**: 

 - Subsetted globi data
   - file = final_globi_data.csv

 - 3-D array  (bee species x plant species x citation)
    - file = globi_data_formatted_bee_plant_date_citation_2025_01_22 - short plant list - no apis.rds

 - Information about the data
    - file = dat_info_2025_01_22.rds



## "4 - Format_covariates_2025_01_07.R"

**Details**: This R script formats the covariates for the model. It also explores correlations among the covariates and generates a number of exploratory plots. 

**Outputs**: 

  - Plant and bee covariates
  
    - file = model_covariates - 2025 01 22 - no apis.rds



## "5 - model code"

**Folder contains the following folders and files**:

  - Check-Model-Output
    
    - Review-Model-Convergence.R
  
  - loop_all_models_all_priors
  
    - _loop-shell-script.sh
    
    - globi-job.sh
    
    - model-code-2025-07-02.R
    
    - Multi-sp-occ-mod-vector-NIMBLE-2025-07-02.R
    
  - loop_models

    - _loop-shell-script.sh

    - globi-job.sh

    - model-code-2024-01-07.R

    - Multi-sp-occ-mod-vector-NIMBLE-2025-01-07.R
    
  - loop_priors_bee_model
    
    - _loop-shell-script.sh
    
    - globi-job.sh
    
    - model-code-2025-07-02.R
    
    - Multi-sp-occ-mod-vector-NIMBLE-2025-07-02.R

**Details**: The folder "Check-Model-Output" contains 1 R script that checks the Rhat values of the model outputs. 

The folder "loop_all_models_all_priors" contains the files for running the analyses for all model types (bee species, plant species, plant family, etc.) and prior combinations (standard deviation = 1, 3, 5, etc.). 

The folder "loop_models" contains the files for running the analyses for all model types (bee species, plant species, plant family, etc.). Only 1 prior type set.

The folder "loop_priors_bee_model" contains the files for running the bee species model with all prior types.




**Outputs**: 

  - Outputs all "out" and "result" files for each model type and prior combination
	
  - Creates Table 1
	


## "6 - functions for covariate plots.R"

**Details**: This R script contains the code for 2 functions that are used to generate covariate plots for comparing the output of 6 different model types.

**Outputs**: 
  
  - response_continous_cov_plot()
  
  - response_factor_cov_plot()


## "7 - Model-output-plots-NIMBLE-2024-07-15.R"

**Details**: This R script creates plots from the model output. It sources the code to the file: "6 - functions for covariate plots.R", and creates all of the figures and statistics that are reported in the results section of the manuscript. 

**Outputs**:

  - Figure 1 - covariates on interaction probability
	
  - Figure 2 - covariates on detection probability


## "8 - Model-output-comparison-2025-01-26.R"

**Details**: This R script produces all of the figures in Appendix S2, and Appendix S1, Figure S1. The primary purpose of this code is to compare the model output of the 6 different model types. 

**Outputs**: 

  - Appendix S1, Figure S1
  
  - All figures in Appendix S2


## "9 - Results reported 2024-07-19.R"

**Details**: This R script generates the values reported in the results section of the Globi manuscript.

**Outputs**:

  - None
	
	
## "10 - Bipartite networks 2022-09-23.R"

**Details**: This R script formats the output from the occupancy model for the bipartite network analysis.

**Outputs**: 

  - 2025 08 05 - bee-plant-mod-probabilities.csv
  
  
## "11 - create_bipartite_visAndAnalysis_2024_08_02.R"

**Details**: This R script creates the bipartite figure (Figure 3).

**Outputs**: 

  - Figure 3 - bipartite network figure

