"""
Script to collect data from MLFlow

Author: Vivek Katial
"""

import argparse
import mlflow
import yaml
import logging


def main():
    # Initialise logging
    logging.basicConfig(format="%(asctime)s - %(message)s", level=logging.INFO)

    # Parsing arguments from CLI
    parser = argparse.ArgumentParser()

    # Adding command line argument
    parser.add_argument("-e", "--experiment", type=str, help="Name of the experiment")

    args = parser.parse_args()
    experiment_name = args.experiment

    # Read data from configuration file
    with open("config/mlflow-tracking-server.yml") as file:
        mlflow_config = yaml.load(file, Loader=yaml.FullLoader)

    # Connect to mlflow
    logging.info(
        'Connecting to MLFlow Tracking Server at URI: "%s" on Experiment:"%s"'
        % (mlflow_config["mlflow"]["tracking_server_uri"], experiment_name)
    )

    mlflow.set_tracking_uri(mlflow_config["mlflow"]["tracking_server_uri"])

    # Find experiments
    experiment = mlflow.get_experiment_by_name(experiment_name)

    logging.info("Downloading data from Experiment")
    d_results = mlflow.search_runs(
        experiment_ids=experiment.experiment_id, run_view_type=1,max_results=50000
    )
    d_results_filename = "data/d_%s.csv" % experiment_name
    d_results.to_csv(d_results_filename, index=False)
    logging.info('Writing runs data to "%s"' % d_results_filename)


if __name__ == "__main__":
    main()
