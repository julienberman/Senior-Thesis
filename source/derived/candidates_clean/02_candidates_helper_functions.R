# function to generate table
generate_table <- function(models, 
                           title = NULL,
                           var_labels = NULL,
                           output_file = NULL
                           ) {
  
  # Get the number of models
  num_models <- length(models)
  
  # Prepare arguments for texreg
  texreg_args <- list(
    l = models,
    custom.model.names = paste0("(", 1:num_models, ")"),
    custom.coef.map = setNames(var_labels, names(var_labels)),
    booktabs = TRUE,
    digits = 3,
    dcolumn = TRUE,
    use.packages = FALSE,
    include.adjrs = TRUE,
    include.nobs = TRUE,
    include.aic = FALSE,    # Exclude AIC
    include.bic = FALSE,    # Exclude BIC
    include.loglik = FALSE,  # Exclude log-likelihood
    include.deviance = FALSE,  # Exclude deviance
    include.rmse = FALSE,  # Exclude RMSE
    custom.note = "$^{*}p<0.1; ^{**}p<0.05; ^{***}p<0.01$"
  )
  
  # Create the LaTeX table
  latex_table <- do.call(texreg, texreg_args)
  
  # Add title and adjust table formatting
  latex_table <- str_replace(latex_table, "\\\\begin\\{table\\}", 
                             paste0("\\\\begin{table}[!htbp]\n",
                                    ifelse(!is.null(title), paste0("\\\\caption{", title, "}\n"), ""),
                                    "\\\\centering\n",
                                    "\\\\begin{adjustbox}{width=\\\\textwidth}"))
  
  latex_table <- str_replace(latex_table, "\\\\end\\{tabular\\}", 
                             "\\\\end{tabular}\n\\\\end{adjustbox}")
  
  
  # Remove annoying formatting for p values
  latex_table <- str_replace_all(
    latex_table, 
    "\\\\multicolumn\\{9\\}\\{l\\}\\{\\\\scriptsize\\{\\$\\^\\{\\*\\}p<0\\.1; \\^\\{\\*\\*\\}p<0\\.05; \\^\\{\\*\\*\\*\\}p<0\\.01\\$\\}\\}", 
    "\\\\multicolumn{9}{l}{\\\\textit{Note:} $^{*}p<0.1; ^{**}p<0.05; ^{***}p<0.01$}"
  )
  
  # Remove caption
  latex_table <- str_replace_all(latex_table, "\\\\caption\\{Statistical models\\}", "")
  
  # Remove \begin{center} and \end{center}
  latex_table <- str_replace_all(latex_table, "\\\\begin\\{center\\}", "")
  latex_table <- str_replace_all(latex_table, "\\\\end\\{center\\}", "")
  
  # Remove extra whitespace in LaTeX code
  latex_table <- gsub("(\\s{2,})(?=\\\\\\\\)", " ", latex_table, perl = TRUE)
  latex_table <- gsub("(\\s{2,})(?=&)", " ", latex_table, perl = TRUE)
  
  # If output_file is specified, write the LaTeX code to a file
  if (!is.null(output_file)) {
    writeLines(latex_table, output_file)
    cat("LaTeX table has been written to", output_file, "\n")
  }
  
  return(latex_table)
}


train_models <- function(formulas, df) {
  # Initialize an empty list to store the models
  models <- list()
  
  # Iterate over the formulas list and train a model for each
  for (i in seq_along(formulas)) {
    model <- lm(formulas[[i]], data = df)  # Train the model using lm
    models[[i]] <- model  # Store the model in the list
  }
  
  # Return the list of models
  return(models)
}
