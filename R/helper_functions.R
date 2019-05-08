## Load libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)

## Helper functions

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))



