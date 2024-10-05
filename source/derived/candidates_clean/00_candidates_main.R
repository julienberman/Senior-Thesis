# import scripts
source("source/derived/candidates_clean/01_candidates_imports.R")
source("source/derived/candidates_clean/02_candidates_helper_functions.R")
source("source/derived/candidates_clean/03_candidates_clean_n1.R")

# Main pipeline function
run_pipeline <- function(
    input_path = "data/raw/candidates.csv",
    output_path = "data/processed/candidates_clean.csv"
) {
  log_info("Starting candidates data cleaning pipeline")
  
  # Load raw data
  raw_df <- load_raw_data(input_path)
  log_info(sprintf("Loaded %d rows from %s", nrow(raw_df), input_path))
  
  # Run cleaning steps
  clean_n1 <- clean_candidates_n1(raw_df)
  #clean_n2 <- clean_candidates_n2(clean_n1)
  #clean_n3 <- clean_candidates_n3(clean_n2)
  
  # Save output
  write_csv(clean_n3, output_path)
  log_info(sprintf("Pipeline completed. Output saved to %s", output_path))
}

# Run pipeline
run_pipeline()