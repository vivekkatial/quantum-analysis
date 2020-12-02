load_instance = function(run_id, experiment_name, uri = "http://115.146.95.176:5000/"){
  # Connect to MLFlow Server
  mlflow::mlflow_set_tracking_uri(uri = uri)
  # Download Artifacts
  mlflow::mlflow_download_artifacts("tmp/d_clauses.rds")
  # Get clauses
  d_clauses <- read_rds("tmp/d_clauses.rds")
  # Delete temp file
  file.remove("tmp/d_clauses.rds")
  # Get number of qubits
  n_qubits <- mlflow::mlflow_get_run(run_id = run_id) %>% 
    pull(params.n_qubits)
  
  # Return instance object
  list(
    n_qubits = n_qubits,
    clauses = d_clauses
  )
}