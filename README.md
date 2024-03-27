# Balancing the risks of mating: biogeographic evidence of cleistogamy as a bet hedging strategy
[Link to preprint to come]().

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About This Research</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
        <li><a href="#r-biogeo">Constructing and analyzing biogeographic dataset in R</a></li>
        <li><a href="#markov">Latitudinal Markov model</a></li>
      </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## Abstract <a name="about-the-project"></a>

Cleistogamy is a mating system in which plants produce some proportion of closed, autonomously self-pollinating flowers. Cleistogamous flowers differ from chasmogamous flowers, which are open flowers capable of outcrossing. Both dimorphic cleistogamy (cleistogamous and chasmogamous flowers produced on the same plant) and complete cleistogamy occur. Cleistogamy has been hypothesized to be a bet hedging strategy for reducing risk in the face of unpredictable pollinator availability. However, conflicting results across species and challenges connecting theory to data have prevented researchers from proving that cleistogamy is bet hedging. To test the bet hedging hypothesis, we investigated the distribution of over 400 cleistogamous species through biogeographical analyses. We find that cleistogamy is more prevalent in cooler, more variable environments. Additionally, we find that among cleistogamous species, complete cleistogamy is more likely to occur in warmer, more stable, tropical and subtropical environments. We hypothesize that the difference in distribution between complete and dimorphic cleistogamy may be driven by the opposing forces of selection to increase cleistogamy proportion and extinction risk, which we test using a heuristic Markov transition model. We conclude that the distribution of cleistogamy suggests that the strategy has evolved in variable environments, consistent with expectations for bet hedging.

### Built With <a name="built-with"></a>

Code is written using the following languages and packages:
* [R Programming Language](https://www.r-project.org/)
  * [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
  * [tidyr](https://tidyr.tidyverse.org/)
  * [ggplot2](https://ggplot2.tidyverse.org/)
  * [rgbif](https://www.gbif.org/tool/81747/rgbif)
  * [rWCVP](https://matildabrown.github.io/rWCVP/)
  * [rgdal](https://www.rdocumentation.org/packages/rgdal/versions/1.6-7)
  * [sf](https://cran.r-project.org/web/packages/sf/index.html)
  * [raster](https://cran.r-project.org/web/packages/raster/index.html)
  * [rstatix](https://cran.r-project.org/web/packages/rstatix/index.html)
  * [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html)
* [MATLAB Programming Language](https://www.mathworks.com/products/MATLAB/programming-with-MATLAB.html)

Data Sources:
* [Culley 2007](https://www.jstor.org/stable/27571184): list of cleistogamous species
* [GBIF](https://www.gbif.org/): geographic occurrence points
* [Kew Plants of the World Online](https://powo.science.kew.org/): known native ranges of plant species
* [WWF Terrestrial Ecoregions of the World](https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world): ecoregion and biome data
* [WorldCLim2](https://www.worldclim.com/version2): bioclimatic data (e.g. temperature and precipitation)

### Constructing and analyzing biogeographic dataset in R <a name="r-biogeo"></a>
Data files:
* Full occurrence data
* Filtered occurrence data w/ ecoregion and climate data

Code files:
* [create_occurrence_data.R](https://github.com/mweissman97/cleistogamy_biogeography/blob/91a436c32ee6f2f351b4e1e9f211aaa9a72db9bd/r_files/create_occurrence_data.R): creates occurrence dataset
* [cleistogamy_occ_filtering.R](https://github.com/mweissman97/cleistogamy_biogeography/blob/91a436c32ee6f2f351b4e1e9f211aaa9a72db9bd/r_files/cleistogamy_occ_filtering.R): reconcile species names + double check occurrence coordinates using the known native ranges (Kew). Also adds biome/ecoregions (WWF).
* [cleist_presence.R](https://github.com/mweissman97/cleistogamy_biogeography/blob/b30f15d3fdd3259a3b9d6e8ae3c95c850247e4a9/r_files/cleist_presence.R): analyze ecoregions where cleistogamy is present vs. absent.
  * Used to make Fig. 1, Fig. 2, Supplemental Table 1
* [cleist_strategy_differences.R](https://github.com/mweissman97/cleistogamy_biogeography/blob/b30f15d3fdd3259a3b9d6e8ae3c95c850247e4a9/r_files/cleist_strategy_differences.R): used to analyze differences in distribution of species occurrence by strategy type. For both differences between complete and dimorphic cleistogamy, as well as differences by pollinator mode (Poaceae vs. other taxa).
  * Used to make Fig. 3, Supplemental Table 2
* USA climate analysis
  * Used to make Supplemental Fig. 1 

### Latitudinal Markov model <a name="markov"></a>
Code files:
* matlab markov matrix generation
* r matrix analyses
  * Used to make Fig. 4

<!-- CONTACT -->
## Contact <a name="contact"></a>

[Personal Website](https://sciencemaya.com) - [Twitter](https://twitter.com/maya_weissman) - maya_weissman@brown.edu
