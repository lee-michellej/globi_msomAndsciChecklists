## LEVERAGING LOCAL EFFORTS TO SOLVE REGIONAL-SCALE ECOLOGICAL QUESTIONS: USING MULTIPLE SOURCES OF DATA AND A MULTI-SPECIES OCCUPANCY MODEL TO EXPLORE BEE-PLANT INTERACTIONS

### [Michelle J. Lee](https://michelleleelifesci.weebly.com/), Chengyi Diao & [Katja C. Seltmann](https://orcid.org/0000-0001-5354-6048)

### Note
This repository contains the other dataset files, including phenology, bee traits and plant trait files. We also include a link to the full interaction dataset from Global Biotic Interactions. These files should be downloaded before running the code associated with this manuscript. Please contact either Michelle or Katja for questions about the data contained within this archive.

  
[```Abstract```](#abstract) / [```Methods```](#methods) / [```Interaction Dataset```](#interaction-dataset) / [```Other Datasets```](#other-datasets)
__________________________________________________________________________________________________________________________________________

### Abstract: 
1: Bees are declining globally, imperiling many ecosystem services they provide, such as plant pollination. Unfortunately, many bee-plant interactions are understudied, producing an incomplete picture of system-level losses resulting from bee declines. Today, we have the opportunity to learn more about bee-plant interactions via opportunistic data coalesced across  natural history collection records, published ecological datasets, and community science initiatives in online databases, such as GloBI (Global Biotic Interactions). 

2: Here, we used the GloBI database, curated local checklists of bee and flowering plant species, phenology data, and a multi-species occupancy model (MSOM) alongside stochastic search variable selection to explore hypotheses related to bee-plant interactions and detection processes. Accounting for some forbidden links (unobservable interactions), we hypothesized that leveraging bee and floral traits would impact the number and detection of interactions. We hypothesized that our MSOM approach would increase our understanding of network structure relative to the raw interaction occurrences, resulting in a more complete network and reflecting a more realistic depiction of bee-plant interactions.  

3: We demonstrate the utility of our MSOM approach on an example bee-plant dataset from Santa Cruz Island. We found a strong effect of bee sociality on the probability of bee-plant interaction, where solitary bees had a lower probability of bee-plant interactions than non-solitary (i.e., social) bees. We did not find an effect of bee size, flower color, or flower shape on the probability of bee-plant interaction. We found a strong effect of source citation type and floral traits on bee-plant detection probability, where the probability of detecting a bee-plant interaction was much higher for observational citations (e.g., iNaturalist) than for natural history collections. Our modeled interaction network showed a higher level of evenness, nestedness, and specialization relative to the interaction network of raw GloBI occurrences. 

4: Observations of species interactions dictate our ability to predict and protect ecosystem structure and function. Our study is the first to utilize occupancy modeling to better understand species interactions, leveraging aggregated, open-source databases and expert checklists. Our findings, while largely counterintuitive, stress the importance of investigating the effect of detection and collection biases on our understanding of ecological interactions.  

### Methods: 
We are using an N-mixture model, with bee species as our "sites" and study as out "survey replicates". The data set consist of presence-only data.

## Interaction Dataset: 
Data used from the Global Biotic Interactions website are constantly being updated. The version used for this paper were accessed on 14 September 2021. The data file can be found on https://zenodo.org/record/7753956 and can be downloaded directly. This file is not included in our github workflow as the files are too large. To run the following code, this data should be downloaded first.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7753956.svg)](https://doi.org/10.5281/zenodo.7753956)

## Other Datasets: 
Datasets used in this analysis are listed below.

1. **Bee traits for checklist species - coloration_NEW.csv**  
_A file that includes color patterns of bees coded by Yolanda Diao from bee images, using references 1-81._
  
  
2. **Bee traits for checklist species - size.csv**  
_A file of bee size compiled by Yolanda Diao, using references 82-83._
  
  
3. **Bee traits for checklist species - sociality.csv**  
_A file of bee sociality compiled by Yolanda Diao, using references 84-163._
  
  
4. **Globi-names-not-in-discoverlife - Sheet1 2022 04 01.csv**  
_A list of bees included in the GloBI interaction dataset that were not included in the Discover Life bee name synonym list._
  
  
5. **SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv**  
_A file that includes the published (as of October 17, 2022) Santa Cruz Island bee checklist. Phenology data was compiled using the bee checklist and searching for every occurrence of each species in GBIF. An binary observation/no observation was established for every month of the year based on these occurrence data._

Citations for SCI bee checklist (SCI checklist and phenology - SCI checklists and phenology - Seltmann 2022 04 01.csv)  
a. Seltmann, K. C., Dewey, D., McLaren, L., & Thrift, C. (2022). Native and non-native bees (Anthophila) of Santa Cruz Island: An annotated checklist. Zenodo. https://doi.org/10.5281/zenodo.7216845  
b. GBIF.org (25 November 2020) GBIF Occurrence Download https://doi.org/10.15468/dl.2ebjjd  
  
  
6. **citation-list-type.csv**  
_A file that includes a list of citation type for each interaction._
  
  
7. **discoverlife-Anthophila-2022 04 01.csv**  
_A file that includes bee species names, resolved names, and relevant sources._

Citations for bee names (discoverlife-Anthophila-2022 04 01.csv)  
a. Ascher, J. S., & Pickering, J. (2020). Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). https://www.discoverlife.org/mp/20q?guide=Apoidea_species
  
  
8. **final-globi-citations-unique 2022 01 21.csv**  
_A file that includes the filtered down list of unique citations that have contributed entries to the final interaction dataset used for analyses._
  
  
9. **institutioncodes_2021_12_16.csv**  
_A file that includes all institutions included in the Symbiota Collections of Arthropods Network (SCAN) that can be merged with contributed entries in GloBI dataset. This matches allow us to attribute collections to different organizations._

SCAN website: https://scan-bugs.org/portal/
  
  
10. **short_noapis_resolvedplantsci_041122.csv**  
_A file that includes the published (as of 2022) Santa Cruz Island plant checklist. Additional phenology data was collected from Calflora and addiitonal trait data were coded using photographs on Calflora and the Jepson eFlora by Michelle Lee. The full plant list was then filtered down to only include plant species in genera that had a recorded interaction with the bees in our GloBI dataset._

Citations for plantlist (short_noapis_resolvedplantsci_041122.csv)  
a. Hasenstab-Lehman, K.E., Guilliams, C.M., & Junak, S.A. (2022). Checklist of the Vascular Flora of Santa Cruz Island, California, Version 2  
b. Calflora: Information on California plants for education, research and conservation. (2022). Berkeley, California: The Calflora Database [a Non-Profit Organization]. https://www.calflora.org  
c. Jepson Flora Project (eds.) 2022. Jepson eFlora, https://ucjeps.berkeley.edu/eflora/   
 
**Citations for bee trait datasets: **  
1. LaBerge, W. E. 1989. A revision of the bees of the genus _Andrena_ of the Western Hemisphere. Part XIII. Subgenera Simandrena and Taeniandrena. Transactions of the American Entomological Society 115: 1-56.  
2. LaBerge, W. E. 1985. A revision of the bees of the genus _Andrena_ of the Western Hemisphere. Part XI. Minor subgenera and subgeneric key. Transactions of the American Entomological Society 111: 440-567.  
3. LaBerge, W. E., Ribble, D. W. 1975. A revision of the bees of the genus _Andrena_ of the Western Hemisphere. Part VII. Subgenus Euandrena. Transactions of the American Entomological Society 101: 371-446.  
4. Thorp, R. W., LaBerge, W. E., 2005. A revision of the bees of the genus _Andrena_ of the Western Hemisphere. Part XV. Subgenus Hesperandrena. Illinois Natural History Survey Bulletin 37: 65-93.  
5. LaBerge, W. E. 1977. A revision of the bees of the genus _Andrena_ of the Western Hemisphere. Part VIII. Subgenera _Thysandrena_, _Dasyandrena_, _Psammandrena_, _Rhacandrena_, _Euandrena_, _Oxyandrena_. Transactions of the American Entomological Society 103: 1-144.  
6. Ribble, D. W. 1968. Revisions of two subgenera of Andrena, Micrandrena Ashmead and Derandrena new subgenus (Hymenoptera: Apoidea). Bulletin of the University of Nebraska State Museum 8: 1-124.  
7. Thorp, R. W. 1969. Systematics and ecology of bees of the subgenus Diandrena (Hymenoptera: Andrenidae). University of California Publications in Entomology 52: 1-146.  
8. Bouseman, J. K., LaBerge, W. E. 1978. A revision of the bees of the genus Andrena of the Western Hemisphere. Part IX. Subgenus Melandrena. Transactions of the American Entomological Society 104: 275-390.  
9. LaBerge, W. E., Thorp, R. W., 2005. A revision of the bees of the genus Andrena of the Western Hemisphere. Part XIV. Subgenus Onagrandrena. Illinois Natural History Survey Bulletin 37: 1-63.  
10. Ribble, D. W. 1974. A revision of the bees of the genus Andrena of the Western Hemisphere. Subgenus Scaphandrena. Transactions of the American Entomological Society 100: 101-189.  
11. Cockerell, Theodore D. A. (Theodore Dru Alison). 1896. “XI.—The Bees of the Genus Andrena Found in New Mexico.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 18: 78–92. https://doi.org/10.1080/00222939608680413.  
12. Timberlake, P. H. (Philip Hunter). 1938. “New Species of Andrena from California (Hymenoptera).” The Pan-Pacific Entomologist 14: 24–29.  
13. LaBerge, W. E., Bouseman, J. K. 1970. A revision of the bees of the genus Andrena of the Western Hemisphere. Part III. Tylandrena. Transactions of the American Entomological Society 96: 543-605.  
14. Bees of Maryland: A Field Guide (2017) http://bio2.elmira.edu/fieldbio/beesofmarylandbookversion1.pdf  
15. Cresson, E. T. (Ezra Townsend). 1879. “Descriptions of New North American Hymenoptera in the Collection of the American Entomological Society.” Transactions of the American Entomological Society and Proceedings of the Entomological Section of the Academy of Natural Sciences 7: 61–136.  
16. Cockerell, Theodore D. A. (Theodore Dru Alison). 1916. “Descriptions and Records of Bees. LXXI.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 17: 277–87.  
17. Michener, Charles D. 1935. “SOME PACIFIC COAST PANURGINUS (HYMEN., APOIDEA).” The Canadian Entomologist 67 (12): 275–78. https://doi.org/10.4039/Ent67275-12.  
18. Crawford, J.C. 1926. “North American Bees of the Genus Panurginus.” Proceedings of the Entomological Society of Washington 28: 207-214  
19. “Observations on Plants and Insects in North-Western Baja California, Mexico, with Descriptions of New Bees.” 1941. Transactions of the San Diego Society of Natural History 9: 337–52.  
20. Cresson, E. T. 1869. “A List of the North American Species of the Genus Anthophora, with descriptions of new species.” Transactions of the American Entomological Society and Proceedings of the Entomological Section of the Academy of Natural Sciences 2: 289–293.  
21. “Descriptions of New Species of North American Bees.” 1878. Proceedings of the Academy of Natural Sciences of Philadelphia 30: 181–221.  
22. Franklin, Henry J. n.d. The Bombidae of the New World. [Philadelphia], American Entomological Society, 1913. https://www.biodiversitylibrary.org/item/213978.  
23. Daly, H. V. 1973. Bees of the genus Ceratina in America North of Mexico (Hymenoptera: Apoidea). University of California Publications in Entomology 74: 1-114. The Regents of the University of California. Published by the University of California Press.  
24. Viereck, Henry Lorenz. 1909. “Descriptions of New Hymenoptera.” Proceedings of the Entomological Society of Washington 11: 42–51.  
25. Michener, Charles D. 1937. “XXVIII.—Records and Descriptions of North American Bees.” Annals and Magazine of Natural History 19 (111): 313–29. https://doi.org/10.1080/00222933708655269.  
26. Cockerell, Theodore D. A. (Theodore Dru Alison). 1924. “Anthophorid Bees in the Collection of the California Academy of Sciences.” The Pan-Pacific Entomologist 1(2): 49–55.  
27. Robertson, C. 1901. “Some New Diptera.” The Canadian Entomologist 33: 284–86.  
28. Onuferko, T.M. 2017. Cleptoparasitic Bees of the Genus Epeolus Latreille (Hymenoptera: Apidae) in Canada. Canadian Journal of Arthropod Identification No. 30: March 30, 2017. doi:10.3752/cjai.2017.30  
29. Timberlake P.H., (1969). A Contribution to the Systematics of North America Species of Synhalonia (Hymenoptera, Apoidea). University of California Publications in Entomology Volume 57  
30. Timberlake, P. H. 1947. A Revision of the Species of Exomalopsis Inhabitating the United States. Journal of the New York Entomological Society 55: 85 - 106.  
31. Fowler, Carroll. 1899. “The Habropoda and Didasia of California.” The Canadian Entomologist 31: 283–86.  
32. Cresson, E. T. (Ezra Townsend). 1879. “Descriptions of New North American Hymenoptera in the Collection of the American Entomological Society.” Transactions of the American Entomological Society and Proceedings of the Entomological Section of the Academy of Natural Sciences 7: 201–14.  
33. LaBerge, W. E., 1961. A Revision of the Bees of the Genus Melissodes in North and Central America. Part III (Hymenoptera, Apidae). The University of Kansas Science Bulletin, Vol. 43. 1-107.  
34. Cresson, E.T., 1878. Descriptions of new North American Hymenoptera in the collection of the American Entomological Society. Trans. Am. Entomol. Soc.7:72.  
35. Viereck, H. 1902. "Hymenoptera from Southern California and New Mexico, with Descriptions of New Species."  Proceedings of the Academy of Natural Sciences of Philadelphia 54: 728 - 743.   
36. Cockerell, T.D.A. 1903. "Bees of the Genus Nomada from California."  Proceedings of the Academy of Natural Sciences of Philadelphia 55: 559 - 579.   
37. Cockerell,T.A. 1910. Some bees of the genus Nomada from Washington state. Psyche. 17:92  
38. Cresson, E.T., 1878. Descriptions of new North American Hymenoptera in the collection of the American Entomological Society. Trans. Am. Entomol. Soc.7:74.  
39. Cockerell, Theodore D. A. (Theodore Dru Alison), and Sandhouse Grace Adelbert. 1924. “Parasitic Bees (Epeolinae and Melectinae) in the Collection of the California Academy of Sciences.” Proceedings of the California Academy of Sciences, 4th Series 13: 305–24.  
40. Mitchell, T.B. 1962 Bees of the Eastern United States. North Carolina Agricultural Experiment Station Technical Bulletin No. 152.  
41. Mitchell, T.B. 1960 Bees of the Eastern United States. North Carolina Agricultural Experiment Station Technical Bulletin No. 141.  
42. Stephen, W. P. 1954. A Revison of the Bee Genus Colletes in America North of Mexico. Part I (Hymenoptera, Colletidae). The University of Kansas Science Bulletin, Vol. 36. 149-506.  
43. Snelling, R. 1970. STUDIES ON NORTH AMERICAN BEES OF THE GENUS HYLAEUS. 5. THE SUBGENERA HYLAEUS. S. STR. AND PARAPROSOPIS (HYMENOPTERA: COLLETIDAE) Contributions in Science, No. 180.  
44. Ordway E. (1966). Systematics of the Genus Augochlorella (Hymenoptera, Halictidae) North of Merxico. The University of Kansas Science Bulletin Vol. XLVI, pp. 509-624, No. 16  
45. Janjic, Jessica, and Laurence Packer. 2001. “New Descriptions of Halictus (Seladonia) from the New World (Hymenopterta: Halictidae).” Journal of Hymenoptera Research 10: 55–75.  
46. Robertson, C. (1897). North American Bees - Description and Synonyms. Transactions of the Academy of Science od St. Louis. Vol. 7. No. 14.  
47. Mitchell, T. 1962. Bees of the Eastern United States, I. North Carolina Agricultural Experiment Station. 1-191.  
48. Cockerell, T.D.A. 1895. “XI.—New Bees of the Genus Halictus from New Mexico, U.S.A.” Annals and Magazine of Natural History 16 (91): 63–69. https://doi.org/10.1080/00222939508680230.  
49. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 61-65  
50. Charles Robertson (1900). Some Illinois Bees. Trans. Acad. Sci. Of st. Louis.  
51. Wilson, Joseph S., and Olivia Messinger Carril. 2016. The Bees in Your Backyard: A Guide to North America’s Bees. Princeton University Press. http://www.jstor.org/stable/j.ctt15hvxqg.  
52. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 80-84  
53. Cockerell, Theodore D. A. (Theodore Dru Alison). 1937. “Bees from San Miguel Island, California.” The Pan-Pacific Entomologist 13: 148–57.  
54. Ellis, Marion Durbin. 1913. "Seven New North American Bees of the Genus Halictus (Hym.)." Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 24: 205 - 211.  
55. McGinley, R. J. 1986. Studies of Halictinae (Apoidea: Halictidae), I: Revision of New World Lasioglossum Curtis. https://doi.org/10.5479/si.00810282.429  
56. Ellis, Marion Durbin. 1914. "New American Bees of the Genus Halictus (Hym.)." Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 25: 151 - 155.  
57. The Annals and magazine of natural history; zoology, botany, and geology.  11 (2): 149  
58. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 144-149  
59. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 161-166  
60. Cockerell, T.D.A. 1897. "New and Little-known North American Bees."  Proceedings of the Academy of Natural Sciences of Philadelphia 49: 334 - 355. https://www.biodiversitylibrary.org/page/26284481#page/334/mode/1up  
61. Crawford, J. C. "New Halictinae From the Wwestern United States." Invertebrata Pacifica Vol. 1 (1903-1907) : 190 - 96.  
62. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 198-203  
63. Cockerell, Theodore D. A. (Theodore Dru Alison). 1936. “Bees from Northern California.” The Pan-Pacific Entomologist 12: 133–64.  
64. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 273-276  
65. Crawford, J.C. 1902. “Notes and Descriptions of Bees.” The Canadian Entomologist 34: 234–40.  
66. Grigarick A.A., & Stange L.A., (1968). The Pollen Collecting Bees of the Anthidiini of California (Hymenoptera: Megachilidae) Bulletin of the California Insect Survey Volume 9.  
67. Cockerell, T. D. A. “The Bees of Southern California. II.” Bulletin of the Southern California Academy of Sciences Vol. v. 1-3(1902-1904): 23-4.  
68. "A Revision of the Genus Ashmeadiella (Hymen., Megachilidae) Author(s): Charles D. Michener Source: American Midland Naturalist,Vol. 22, No. 1 (Jul., 1939), pp. 1-84"  
69. Griswold, Terry L. 1985. “A New Ashmeadiella from the California Channel Islands (Hymenoptera: Megachilidae).” Journal of the Kansas Entomological Society 58, no. 3: 555–57. http://www.jstor.org/stable/25084680.  
70. Cockerell, Theodore D. A. (Theodore Dru Alison). 1935. “Some California Bees.” The Pan-Pacific Entomologist 11: 48–54.  
71. Baker J.R., (1975). Taxonomy of Five Nearctic Subgenera of Coelioxys (Hymenoptera; Megachilidae). The University of Kansas Science Bulletin Vol. 50, No. 12, pp.649-730.  
72. Cockerell, Theodore D. A. (Theodore Dru Alison), and Emerson Atkins. 1902. “XXXIX.—Contributions from the New Mexico Biological Station—XII. On Some Genera of Bees.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 9: 230–34.  
73. C. S. Sheffield 2011. Canadian Journal of Arthropod Identification No. 18 (November 2011)  
74. Sheffield C. S., et al (2011). Leaf cutter and Mason Bees of the Genus Megachile Latreille (Hymenoptera; Megachilidae) in Canada and Alaska. Canadian Journal of Arthropod Identification No. 18  
75. Cockerell, T.D.A. "The Bees of Florissant, Colorado." Bulletin of the American Museum of Natural History 22: 420 - 455.  
76. Michener, Charles D., and Sinha, Ranendra N. 1958. A revison of the Genus Osmia, Subgenus Centrosmia (Hymenoptera: Megachilidae). The University of Kansas Science Bulletin, Vol. 39. 275-303.  
77. Cockerell, T.D.A. 1910. "New and Little-known Western Bees." Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 21: 270-73.  
78. Cockerell, Theodore D. A. (Theodore Dru Alison). 1911. “LXXXIX.—Descriptions and Records of Bees—XL.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 8: 763 - 770.  
79. Cockerell, Theodore D. A. (Theodore Dru Alison). 1907. “LXIV.—Descriptions and Records of Bees—XIV.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 19: 531 - 540.  
80. Michener, Charles D. 1937. "The Bees of the Genera Chelostomopsis, Formicapis, Robertsonella and Prochelostoma.(Hymen: Megachilidae)" Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 48: 127 - 32.  
81. "Descriptions of new North American Hymenopters, and observations on some already described." American Entomology : A Description of the Insects of North American, with Illustrations Drawn and Colored after Nature 2: 642 - 766.  
82. Wilson, Joseph S., and Olivia Messinger Carril.  2016. The Bees in Your Backyard: A Guide to North America’s Bees. Princeton University Press. http://www.jstor.org/stable/j.ctt15hvxqg.
83. Griswold, T. 1986. Notes on the nesting biology of Protosmia (Chelostomopsis) rubifloris (Cockerell) (Hymenoptera: Megachilidae). The Pan-Pacific Entomologist 62: 84–87.
84. LaBerge, W. E. 1989. A revision of the bees of the genus Andrena of the Western Hemisphere. Part XIII. Subgenera Simandrena and Taeniandrena. Transactions of the American Entomological Society 115: 1-56.  
85. LaBerge, W. E. 1985. A revision of the bees of the genus Andrena of the Western Hemisphere. Part XI. Minor subgenera and subgeneric key. Transactions of the American Entomological Society 111: 440-567.  
86. LaBerge, W. E., Ribble, D. W. 1975. A revision of the bees of the genus Andrena of the Western Hemisphere. Part VII. Subgenus Euandrena. Transactions of the American Entomological Society 101: 371-446.  
87. Thorp, R. W., LaBerge, W. E., 2005. A revision of the bees of the genus Andrena of the Western Hemisphere. Part XV. Subgenus Hesperandrena. Illinois Natural History Survey Bulletin 37: 65-93.  
88. LaBerge, W. E. 1977. A revision of the bees of the genus Andrena of the Western Hemisphere. Part VIII. Subgenera Thysandrena, Dasyandrena, Psammandrena, Rhacandrena, Euandrena, Oxyandrena. Transactions of the American Entomological Society 103: 1-144.  
89. Ribble, D. W. 1968. Revisions of two subgenera of Andrena, Micrandrena Ashmead and Derandrena new subgenus (Hymenoptera: Apoidea). Bulletin of the University of Nebraska State Museum 8: 1-124.  
90. Thorp, R. W. 1969. Systematics and ecology of bees of the subgenus Diandrena (Hymenoptera: Andrenidae). University of California Publications in Entomology 52: 1-146.  
91. Bouseman, J. K., LaBerge, W. E. 1978. A revision of the bees of the genus Andrena of the Western Hemisphere. Part IX. Subgenus Melandrena. Transactions of the American Entomological Society 104: 275-390.  
92. LaBerge, W. E., Thorp, R. W., 2005. A revision of the bees of the genus Andrena of the Western Hemisphere. Part XIV. Subgenus Onagrandrena. Illinois Natural History Survey Bulletin 37: 1-63.  
93. Ribble, D. W. 1974. A revision of the bees of the genus Andrena of the Western Hemisphere. Subgenus Scaphandrena. Transactions of the American Entomological Society 100: 101-189.  
94. Cockerell, Theodore D. A. (Theodore Dru Alison). 1896. “XI.—The Bees of the Genus Andrena Found in New Mexico.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 18: 78–92. https://doi.org/10.1080/00222939608680413.  
95. LaBerge, W. E., Bouseman, J. K. 1970. A revision of the bees of the genus Andrena of the Western Hemisphere. Part III. Tylandrena. Transactions of the American Entomological Society 96: 543-605.  
96. Michener, Charles Duncan. “The Bees of the World.” (2000).  
97. Cresson, E. T. (Ezra Townsend). 1879. “Descriptions of New North American Hymenoptera in the Collection of the American Entomological Society.” Transactions of the American Entomological Society and Proceedings of the Entomological Section of the Academy of Natural Sciences 7: 61–136.  
98. Cockerell, Theodore D. A. (Theodore Dru Alison). 1916. “Descriptions and Records of Bees. LXXI.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 17: 277–87.  
99. Michener, Charles D. 1935. “SOME PACIFIC COAST PANURGINUS (HYMEN., APOIDEA).” The Canadian Entomologist 67 (12): 275–78. https://doi.org/10.4039/Ent67275-12.  
100. Crawford, J.C. 1926. “North American Bees of the Genus Panurginus.” Proceedings of the Entomological Society of Washington 28: 207-214  
101. “Observations on Plants and Insects in North-Western Baja California, Mexico, with Descriptions of New Bees.” 1941. Transactions of the San Diego Society of Natural History 9: 337–52.  
102. Cresson, E. T. 1869. “A List of the North American Species of the Genus Anthophora, with descriptions of new species.” Transactions of the American Entomological Society and Proceedings of the Entomological Section of the Academy of Natural Sciences 2: 289–293.  
103. “Descriptions of New Species of North American Bees.” 1878. Proceedings of the Academy of Natural Sciences of Philadelphia 30: 181–221.  
104. Wilson, Joseph S., and Olivia Messinger Carril.  2016. The Bees in Your Backyard: A Guide to North America’s Bees. Princeton University Press. http://www.jstor.org/stable/j.ctt15hvxqg.  
105. Franklin, Henry J. n.d. The Bombidae of the New World. [Philadelphia], American Entomological Society, 1913. https://www.biodiversitylibrary.org/item/213978.  
106. Jonathan Koch, James Strange,Paul Williams.2012. Bumble Bees of the Western United States. A product of the U.S. Forest Service and the Pollinator Partnership with funding from the National Fish and Wildlife Foundation  
107. Cockerell, T.D.A. 1897. "New and Little-known North American Bees."  Proceedings of the Academy of Natural Sciences of Philadelphia 49: 334 - 355. https://www.biodiversitylibrary.org/page/26284481#page/334/mode/1up  
108. Viereck, Henry Lorenz. 1909. “Descriptions of New Hymenoptera.” Proceedings of the Entomological Society of Washington 11: 42–51.  
109. Michener, Charles D. 1937. “XXVIII.—Records and Descriptions of North American Bees.” Annals and Magazine of Natural History 19 (111): 313–29. https://doi.org/10.1080/00222933708655269.  
110. Cockerell, Theodore D. A. (Theodore Dru Alison). 1924. “Anthophorid Bees in the Collection of the California Academy of Sciences.” The Pan-Pacific Entomologist 1(2): 49–55.  
111. Robertson, C. 1901. “Some New Diptera.” The Canadian Entomologist 33: 284–86.  
112. Cockerell, Theodore D. A. (Theodore Dru Alison). 1937. “Bees from San Miguel Island, California.” The Pan-Pacific Entomologist 13: 148–57.  
113. Timberlake P.H., (1969). A Contribution to the Systematics of North America Species of Synhalonia (Hymenoptera, Apoidea). University of California Publications in Entomology Volume 57  
114. Timberlake, P. H. 1947. A Revision of the Species of Exomalopsis Inhabitating the United States. Journal of the New York Entomological Society 55: 85 - 106.  
115. Fowler, Carroll. 1899. “The Habropoda and Didasia of California.” The Canadian Entomologist 31: 283–86.  
116. Cresson, E. T. (Ezra Townsend). 1879. “Descriptions of New North American Hymenoptera in the Collection of the American Entomological Society.” Transactions of the American Entomological Society and Proceedings of the Entomological Section of the Academy of Natural Sciences 7: 201–14.  
117. LaBerge, W. E., 1961. A Revision of the Bees of the Genus Melissodes in North and Central America. Part III (Hymenoptera, Apidae). The University of Kansas Science Bulletin, Vol. 43. 1-107.  
118. Cockerell, Theodore D. A. (Theodore Dru Alison). 1903. “New American Hymenoptera, Mostly of the Genus Nomada.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 12: 200–214.  
119. Viereck, H. 1902. "Hymenoptera from Southern California and New Mexico, with Descriptions of New Species."  Proceedings of the Academy of Natural Sciences of Philadelphia 54: 728 - 743.   
120. Cockerell, T.D.A. 1903. "Bees of the Genus Nomada from California."  Proceedings of the Academy of Natural Sciences of Philadelphia 55: 559 - 579.   
121. Cockerell,T.A. 1910. Some bees of the genus Nomada from Washington state. Psyche. 17:92  
122. Cockerell, Theodore D. A. (Theodore Dru Alison). 1910. “Some Bees of the Genus Nomada From Washington State.” Psyche 17: 91–98.  
123. Cockerell, Theodore D. A. (Theodore Dru Alison), and Sandhouse Grace Adelbert. 1924. “Parasitic Bees (Epeolinae and Melectinae) in the Collection of the California Academy of Sciences.” Proceedings of the California Academy of Sciences, 4th Series 13: 305–24.  
124. Mitchell, T.B. 1962 Bees of the Eastern United States. North Carolina Agricultural Experiment Station Technical Bulletin No. 152.  
125. Stephen, W. P. 1954. A Revison of the Bee Genus Colletes in America North of Mexico. Part I (Hymenoptera, Colletidae). The University of Kansas Science Bulletin, Vol. 36. 149-506.  
126. Mitchell, T.B. 1960 Bees of the Eastern United States. North Carolina Agricultural Experiment Station Technical Bulletin No. 141.  
127. Snelling, R. 1970. STUDIES ON NORTH AMERICAN BEES OF THE GENUS HYLAEUS. 5. THE SUBGENERA HYLAEUS. S. STR. AND PARAPROSOPIS (HYMENOPTERA: COLLETIDAE) Contributions in Science, No. 180.  
128. Cockerell, T.D.A. 1901. “Bees from Southern California, Visting Flowers of Eriogonum and Rhus.” The Canadian Entomologist 33: 281–83.  
129. Ordway E. (1966). Systematics of the Genus Augochlorella (Hymenoptera, Halictidae) North of Merxico. The University of Kansas Science Bulletin Vol. XLVI, pp. 509-624, No. 16  
130. Janjic, Jessica, and Laurence Packer. 2001. “New Descriptions of Halictus (Seladonia) from the New World (Hymenopterta: Halictidae).” Journal of Hymenoptera Research 10: 55–75.  
131. Robertson, C. (1897). North American Bees - Description and Synonyms. Transactions of the Academy of Science od St. Louis. Vol. 7. No. 14.  
132. Mitchell, T. 1962. Bees of the Eastern United States, I. North Carolina Agricultural Experiment Station. 1-191.  
133. Cockerell, T.D.A. 1895. “XI.—New Bees of the Genus Halictus from New Mexico, U.S.A.” Annals and Magazine of Natural History 16 (91): 63–69. https://doi.org/10.1080/00222939508680230.  
134. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 61-65  
135. Charles Robertson (1900). Some Illinois Bees. Trans. Acad. Sci. Of st. Louis.  
136. Wilson, Joseph S., and Olivia Messinger Carril. 2016. The Bees in Your Backyard: A Guide to North America’s Bees. Princeton University Press. http://www.jstor.org/stable/j.ctt15hvxqg.  
137. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 80-84  
138. Ellis, Marion Durbin. 1913. "Seven New North American Bees of the Genus Halictus (Hym.)." Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 24: 205 - 211.  
139. McGinley, R. J. 1986. Studies of Halictinae (Apoidea: Halictidae), I: Revision of New World Lasioglossum Curtis. https://doi.org/10.5479/si.00810282.429  
140. Ellis, Marion Durbin. 1914. "New American Bees of the Genus Halictus (Hym.)." Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 25: 151 - 155.  
141. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 144-149  
142. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 161-166  
143. Crawford, J. C. "New Halictinae From the Wwestern United States." Invertebrata Pacifica Vol. 1 (1903-1907) : 190 - 96.  
144. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 198-203  
145. Cockerell, Theodore D. A. (Theodore Dru Alison). 1936. “Bees from Northern California.” The Pan-Pacific Entomologist 12: 133–64.  
146. Gibbs, J. 2010. Revision of the metallic species of Lasioglossum (Dialictus)in Canada (Hymenoptera, Halictidae, Halictini). Zootaxa; 2591, 273-276  
147. Crawford, J.C. 1902. “Notes and Descriptions of Bees.” The Canadian Entomologist 34: 234–40.  
148. Cockerell, T. D. A. 1938. “Bees Collected on the California Islands in the Spring of 1938.” 1938. Transactions of the San Diego Society of Natural History. Vol. 9 (1938-1942): 37 - 8.  
149. Cockerell, T. D. A. “The Bees of Southern California. II.” Bulletin of the Southern California Academy of Sciences Vol. v. 1-3(1902-1904): 23-4.  
150. "A Revision of the Genus Ashmeadiella (Hymen., Megachilidae) Author(s): Charles D. Michener Source: American Midland Naturalist,Vol. 22, No. 1 (Jul., 1939), pp. 1-84"  
151. Griswold, Terry L. 1985. “A New Ashmeadiella from the California Channel Islands (Hymenoptera: Megachilidae).” Journal of the Kansas Entomological Society 58, no. 3: 555–57. http://www.jstor.org/stable/25084680.  
152. Cockerell, Theodore D. A. (Theodore Dru Alison). 1935. “Some California Bees.” The Pan-Pacific Entomologist 11: 48–54.  
153. Baker J.R., (1975). Taxonomy of Five Nearctic Subgenera of Coelioxys (Hymenoptera; Megachilidae). The University of Kansas Science Bulletin Vol. 50, No. 12, pp.649-730.  
154. Cockerell, Theodore D. A. (Theodore Dru Alison), and Emerson Atkins. 1902. “XXXIX.—Contributions from the New Mexico Biological Station—XII. On Some Genera of Bees.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 9: 230–34.  
155. C. S. Sheffield 2011. Canadian Journal of Arthropod Identification No. 18 (November 2011)  
156. Sheffield C. S., et al (2011). Leaf cutter and Mason Bees of the Genus Megachile Latreille (Hymenoptera; Megachilidae) in Canada and Alaska. Canadian Journal of Arthropod Identification No. 18  
157. Cockerell, T.D.A. "The Bees of Florissant, Colorado." Bulletin of the American Museum of Natural History 22: 420 - 455.  
158. Michener, Charles D., and Sinha, Ranendra N. 1958. A revison of the Genus Osmia, Subgenus Centrosmia (Hymenoptera: Megachilidae). The University of Kansas Science Bulletin, Vol. 39. 275-303.  
159. Cockerell, T.D.A. 1910. "New and Little-known Western Bees." Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 21: 270-73.  
160. Cockerell, Theodore D. A. (Theodore Dru Alison). 1911. “LXXXIX.—Descriptions and Records of Bees—XL.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 8: 763 - 770.  
161. Cockerell, Theodore D. A. (Theodore Dru Alison). 1907. “LXIV.—Descriptions and Records of Bees—XIV.” The Annals and Magazine of Natural History; Zoology, Botany, and Geology 19: 531 - 540.  
162. Michener, Charles D. 1937. "The Bees of the Genera Chelostomopsis, Formicapis, Robertsonella and Prochelostoma.(Hymen: Megachilidae)" Entomological News, and Proceedings of the Entomological Section of the Academy of Natural Sciences of Philadelphia 48: 127 - 32.  
163. "Descriptions of new North American Hymenopters, and observations on some already described." American Entomology : A Description of the Insects of North American, with Illustrations Drawn and Colored after Nature 2: 642 - 766.  
  
