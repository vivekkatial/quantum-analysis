#!/bin/bash

#----------------------------------------------------
# Script to update experimentation data from MLFlow
#
# Author: Vivek Katial
#----------------------------------------------------

export AQC_EXPERIMENT="three-sat-usa-inc-gs"
export QAOA_EXPERIMENT="three-sat-usa-qaoa"

# Activate venv
source activate qiskit

# Run python extraction scripts to download data
python3 analysis/extraction-scripts/get_mlflow_data.py --experiment=$AQC_EXPERIMENT
python3 analysis/extraction-scripts/get_mlflow_data.py --experiment=$QAOA_EXPERIMENT

# Run R Script to enrich data with features
Rscript analysis/extraction-scripts/build-feature-set.R
