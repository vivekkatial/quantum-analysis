###############################################################################
# Script to debug JSON file
#
# Author: Vivek Katial
# Created 2021-04-14 13:22:51
###############################################################################

library(igraph)
library(jsonlite)

raw_instance <-read_json("data/vrp-instances/debug-qaoa-vrp.json")

raw_instance
