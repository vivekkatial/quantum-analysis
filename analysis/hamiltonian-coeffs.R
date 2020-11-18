###############################################################################
# Script to generate value for ratios
#
# Author: Vivek Katial
# Created 2020-09-07 19:16:17
###############################################################################


#' Function to generate permutations
permutations = function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) {
  if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 
      0) 
    stop("bad value of n")
  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 
      0) 
    stop("bad value of r")
  if (!is.atomic(v) || length(v) < n) 
    stop("v is either non-atomic or too short")
  if ((r > n) & repeats.allowed == FALSE) 
    stop("r > n and repeats.allowed=FALSE")
  if (set) {
    v <- unique(sort(v))
    if (length(v) < n) 
      stop("too few different elements")
  }
  v0 <- vector(mode(v), 0)
  if (repeats.allowed) 
    sub <- function(n, r, v) {
      if (r == 1) 
        matrix(v, n, 1)
      else if (n == 1) 
        matrix(v, 1, r)
      else {
        inner <- Recall(n, r - 1, v)
        cbind(rep(v, rep(nrow(inner), n)), matrix(t(inner), 
                                                  ncol = ncol(inner), nrow = nrow(inner) * n, 
                                                  byrow = TRUE))
      }
    }
  else sub <- function(n, r, v) {
    if (r == 1) 
      matrix(v, n, 1)
    else if (n == 1) 
      matrix(v, 1, r)
    else {
      X <- NULL
      for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n - 
                                                        1, r - 1, v[-i])))
      X
    }
  }
  sub(n, r, v[1:n])
}

#' Function to check if matrices are equal
matequal <- function(x, y){
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y) 
}
 

#' Function to generate r_i for an instance of 3SAT
#' @param n_qubits The number of qubits, n
#' @param d_clauses The set of clauses as a list
#' @return dataframe for each configuration of Hamiltonian and their corresponding ratio
compute_pauli_coefficients <- function(n_qubits, d_clauses){
  
  # Define Pauli Matrix
  Z = matrix(c(1,0,0,-1),   nrow = 2, ncol = 2)
  I = matrix(c(1,0,0,1),    nrow = 2, ncol = 2)

  # Create Problem Hamiltonian
  H_p <- create_ham_problem(n_qubits, d_clauses)
  
  # Generate different sets of Permutations for Pauli matrix I and matrix Z
  pauli_hams <- permutations(2, n_qubits, c("I","Z"), repeats.allowed = T)
  
  # Data frame of Pauli Entries
  pauli_df <- pauli_hams %>% 
    as_tibble() %>% 
    mutate(coeff_r = as.double(0L))
  
  # Construct base Hamiltonian
  base_ham <- matrix(0L, nrow = 2^n_qubits, ncol = 2^n_qubits)
  
  for (i in 1:nrow(pauli_hams)) {
    
    # Construct Pauli String
    pauli_str <- pauli_hams[i,] %>% 
      paste0(collapse = " %x% ")
    
    # Build Hamiltonian for Pauli String
    pauli_ham <- eval(rlang::parse_expr(pauli_str))
    
    # Compute r_i
    r_i <- sum(diag(H_p %*% pauli_ham))/(2^n_qubits)
    
    # Label matrix
    pauli_df[i,n_qubits+1] = r_i
    
    # Add pauli ham to base_ham
    base_ham <- r_i*pauli_ham + base_ham
    
  }
  
  testthat::expect_equal(
    matequal(base_ham, H_p),
    TRUE
  )
  
  pauli_df
}

#' Function to convert coefficients to JSON string for QAOA scripts
#' @param n_qubits The number of qubits, n
#' @param d_coeffs Dataframe consisting of JSON 
#' @param run_id run_id for AQC run
#' @param sat_assgn The satisfying assignment
#' @return JSON String
generate_instance_json = function(n_qubits, d_coeffs, run_id, sat_assgn){
  d_json <- d_coeffs %>% 
    # Remove non-needed interaction terms
    filter(coeff_r != 0) %>% 
    # Combine terms
    unite("z_terms", remove = F, sep = "", -coeff_r) %>% 
    # Remove III..I term
    slice(2:n()) %>% 
    # Identify qubits
    mutate(qubits = map(z_terms, find_qubit_interaction)) %>% 
    # Rename to coefficient
    select(qubits, coefficient = coeff_r) %>% 
    mutate(terms = map_chr(qubits, function(qubits){
      if (length(qubits) == 1) {
        "single_qubit"
      } else if (length(qubits) == 2){
        "double_qubit"
      } else if (length(qubits) == 3){
        "triple_qubit"
      } else {
        NA
      }
    }))
  
  z_terms <- d_json$terms %>% unique()
  
  instance_json <- lapply(z_terms, function(term){
    circuit_rots = d_json %>% 
      filter(terms == term) %>% 
      select(qubits, coefficient)
    
    list(
      rotations = list(
        circuit_rots
      )
    )
  }) %>% 
    setNames(z_terms)
  
  # Add instance json
  instance_json$n_qubits <- jsonlite::unbox(n_qubits)
  
  # Add run_id
  instance_json$run_id <- jsonlite::unbox(run_id)
  
  # Add satisfying assignment
  instance_json$sat_assgn <- jsonlite::unbox(sat_assgn)
  
  instance_json %>% 
    jsonlite::toJSON(pretty = T, auto_unbox = F, flatten=T)
}


#' Function to identify qubits on which Z terms are interacting based on a string
#' @param z_term A string with Z term
#' @return A vector representing qubits with interaction terms e.g. c(1,5) corresponds to Z interactions on 1 and 5
find_qubit_interaction = function(z_term){
  
  qubits <- str_locate_all(pattern = "Z", stringi::stri_reverse(z_term)) %>% 
    unlist() %>%
    unique()
  
  if (is_empty(z_term)) {
    qubits <- NA
  }
  
  qubits - 1
  
}
