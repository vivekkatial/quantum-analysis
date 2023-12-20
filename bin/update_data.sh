#!/bin/bash

#----------------------------------------------------
# Script to update experimentation data from MLFlow
#
# Author: Vivek Katial
#----------------------------------------------------

#export AQC_EXPERIMENT="three-sat-usa-inc-gs"
# export QAOA_EXPERIMENT="three-sat-usa-qaoa"
export QAOA_VRP_EXPERIMENT="QAOA-Instance-Based-Parameter-Optimization"

pipenv run python analysis/extraction-scripts/get_mlflow_data.py --experiment=$QAOA_VRP_EXPERIMENT

# Run R Script to enrich data with features
# Rscript analysis/extraction-scripts/build-feature-set.R
# Rscript analysis/combine-exps.R
