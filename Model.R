# Model

probability_of_detection ~ B0 + 
                           B1 * L_CUT +  # Station in recently harvested cutblock (≤30 years) proportion ==> HarvestStateMap > 0
                           B2 * L_YDEC + # Station in young deciduous forest (31–90 years) proportion ==> Age map + LCC05
                           B3 * L_ODEC + # Station in old deciduous forest (>90 years) proportion ==> Age map = LCC05
                           B4 * L_MIX + # Station in mixed deciduous/conifer forest (>30 years) proportion ==> Age map + LCC05
                           B5 * L_CC + # Area weighted mean forest crown closure ==> ???
                           B6 * L_WDIS + # Distance from station centre to nearest river or lake ==> ??? just 0 for all?
                             B7 * N_CUT + # Station in recently harvested cutblock (≤30 years) proportion ==> HarvestStateMap > 0
                             B8 * N_LATE + # Proportion in old forest (90+ year) proportion ==> Age map (independently of class?)
                             B9 * N_DEC + # Proportion in deciduous forest ==> LCC05 (independently of age?)
                             B10 * N_MIX + # Station in mixed deciduous/conifer forest (>30 years) proportion ==> Age map
                             B11 * N_SB + # Presence of black spruce forest ==> Y/N? and LCC doesn't separate the type of conifers...??? 
                             B12 * N_RICH # Number of habitat classes in neighbourhood ==> LCC05

# B... : Get from table Appendix 1
# Values for the variables get from rasters Age Map, Harvest State Map...?

# Questions to Steve:
# 1. How is the fire going to be accounted for in the model? = L_CUT?
# 2. We need landscape classes: LCC05 modules?

# Need to extract from the maps, per year:
# 
# 
# "scfmCrop", "scfmLandcoverInit" --> Load and crop the vegetation map
# 
