import pandas as pd
import os

from mlflow.tracking import MlflowClient
from typing import Dict, Union
from concurrent.futures import ThreadPoolExecutor, as_completed
from tqdm import tqdm


def create_performance_metric(data: pd.DataFrame, massive_number: Union[int, float] = 1e5) -> Dict[str, Union[int, float]]:
    """
    Computes the minimum number of function evaluations required to reach a good approximation ratio for each 'init_type'.
    
    This function processes a DataFrame containing optimization data. It identifies the minimum number of function 
    evaluations (eval_count) required to reach a good approximation ratio for each initialization type (init_type). 
    The approximation ratio is considered good if it's at least 95% of the maximum approximation ratio found in the data.
    For any init_type not meeting this criterion, a default large number (massive_number) is assigned.
    
    Parameters:
    data (pd.DataFrame): The input data. Must contain columns 'init_type', 'approximation_ratio', and 'eval_count'.
    massive_number (int or float, optional): The default number to assign for init_types that do not meet the criterion. 
                                             Defaults to 1e5.
    
    Returns:
    Dict[str, Union[int, float]]: A dictionary where keys are init_types and values are the minimum eval_count or 
                                  massive_number for each init_type.
    
    Raises:
    ValueError: If the input is not a DataFrame or does not contain the necessary columns.
    """
    
    # Ensure that the input is a DataFrame
    if not isinstance(data, pd.DataFrame):
        raise ValueError("Input data must be a DataFrame.")
    
    # Check for necessary columns
    required_columns = ["init_type", "approximation_ratio", "eval_count"]
    if not all(col in data.columns for col in required_columns):
        raise ValueError("Data must contain 'init_type', 'approximation_ratio', and 'eval_count' columns.")
    
    # Compute the maximum approximation ratio
    max_approximation_ratio = data['approximation_ratio'].max(skipna=True)
    
    # Filter and summarize for groups that meet the criterion
    filtered_data = data[data['approximation_ratio'] >= 0.95 * max_approximation_ratio]
    summary = filtered_data.groupby('init_type')['eval_count'].min().reset_index()
    summary.rename(columns={'eval_count': 'min_fevals_to_reach_good_ar'}, inplace=True)
    
    # Identify the excluded groups and impute values
    all_init_types = data['init_type'].unique()
    excluded_init_types = set(all_init_types) - set(summary['init_type'])
    excluded_data = pd.DataFrame({'init_type': list(excluded_init_types),
                                  'min_fevals_to_reach_good_ar': [massive_number] * len(excluded_init_types)})
    
    # Combine the results
    final_data = pd.concat([summary, excluded_data], ignore_index=True)
    
    # Convert to a key-value list
    key_value_list = final_data.set_index('init_type').to_dict()['min_fevals_to_reach_good_ar']
    
    return key_value_list

def download_artifacts(run_id, destination_path):
    """
    Download 'results.csv' artifact for a given MLflow run ID.

    Parameters:
    run_id (str): The run ID for the MLflow experiment.
    destination_path (str): Local path to download artifacts.

    Returns:
    str: Path to the downloaded 'results.csv' file, or None if not found.
    """
    client = MlflowClient()

    # Specify the artifact path for 'results.csv'
    artifact_path = "results.csv"

    # Build the full path for the artifact
    full_artifact_path = os.path.join(destination_path, artifact_path)

    # Create destination directory if it does not exist
    if not os.path.exists(destination_path):
        os.makedirs(destination_path)

    # Download 'results.csv'
    try:
        client.download_artifacts(run_id, artifact_path, destination_path)
        print(f"Downloaded 'results.csv' for run_id {run_id} to {full_artifact_path}")
        return full_artifact_path
    except Exception as e:
        print(f"Error downloading 'results.csv' for run_id {run_id}: {e}")
        return None

def process_single_run(run_id):
    """
    Process a single run: download the artifact, process it, 
    and return the results from create_performance_metric.

    Parameters:
    run_id (str): The run_id for processing.

    Returns:
    Dict[str, Union[int, float]]: Performance metrics for the run_id.
    """
    destination_path = f"temp_artifacts/{run_id}"
    artifact_file = download_artifacts(run_id, destination_path)

    if artifact_file and os.path.exists(artifact_file):
        artifact_data = pd.read_csv(artifact_file)
        metrics = create_performance_metric(artifact_data)

        # Cleanup the downloaded file
        os.remove(artifact_file)
        if os.path.exists(destination_path):
            os.rmdir(destination_path)

        return run_id, metrics
    else:
        print(f"No artifact file found for run_id: {run_id}")
        return run_id, {}

def process_data_parallel(df, max_workers=10):
    """
    Process each run_id in the DataFrame in parallel and update the DataFrame with new metrics.

    Parameters:
    df (pd.DataFrame): The DataFrame containing the run_id column.
    max_workers (int): Maximum number of parallel workers.

    Returns:
    pd.DataFrame: Updated DataFrame with new columns for performance metrics for each run_id.
    """
    run_ids = df['run_id'].tolist()

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = {executor.submit(process_single_run, run_id): run_id for run_id in run_ids}
        
        # Use tqdm for progress updates
        for future in tqdm(as_completed(futures), total=len(run_ids)):
            run_id = futures[future]
            try:
                run_id, metrics = future.result()
                for key, value in metrics.items():
                    df.loc[df['run_id'] == run_id, f"algo_{key}"] = value
            except Exception as exc:
                print(f'{run_id} generated an exception: {exc}')

    return df


# Main execution
if __name__ == "__main__":
    system_size = 12

    # Read the CSV file
    d_runs = pd.read_csv("data/d_QAOA-Instance-Based-Parameter-Optimization_all_runs.csv")
    
    # Filter data for custom graph True
    d_runs = d_runs[d_runs['params.custom_graph'] == True]
    
    for col in d_runs.columns:
        print(col)

    # Filter data
    # filtered_d_runs = d_runs[d_runs['params.instance_size'] == system_size]

    # Select specific columns and rename
    # selected_d_runs = filtered_d_runs.loc[:, ['run_id', 'params.instance_class'] + 
    #                                       [col for col in filtered_d_runs.columns if 'params' in col or 'metric' in col]]
    # selected_d_runs.rename(columns={'params.instance_class': 'Source'}, inplace=True)

    # Process each run in parallel to get performance metrics
    d_runs_with_metrics = process_data_parallel(d_runs, max_workers=24)  # Adjust max_workers as needed
    print(d_runs_with_metrics)

    # Write the results to a CSV file
    d_runs_with_metrics.to_csv("data/new-custom-instances-runs-with-Metrics.csv", index=False)
