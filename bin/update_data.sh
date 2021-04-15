#!/bin/bash

#----------------------------------------------------
# Script to update experimentation data from MLFlow
#
# Author: Vivek Katial
#----------------------------------------------------

#export AQC_EXPERIMENT="three-sat-usa-inc-gs"
# export QAOA_EXPERIMENT="three-sat-usa-qaoa"
export QAOA_VRP_EXPERIMENT="vrp-qaoa"

# Activate venv
pipenv shell

# Run python extraction scripts to download data
# python3 analysis/extraction-scripts/get_mlflow_data.py --experiment=$AQC_EXPERIMENT
python3 analysis/extraction-scripts/get_mlflow_data.py --experiment=$QAOA_VRP_EXPERIMENT

# Run R Script to enrich data with features
# Rscript analysis/extraction-scripts/build-feature-set.R
# Rscript analysis/combine-exps.R
