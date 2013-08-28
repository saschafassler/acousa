AsaR2FishFrame <-
function(strata_numbers_all,acoustic_data,spp)

### function to convert AsaR output into FishFrame format for the selected species
###
### input: -strata numbers: data frame with species numbers by strata and ICES rectangle information

### load required packages
    require(xlsReadWrite)

