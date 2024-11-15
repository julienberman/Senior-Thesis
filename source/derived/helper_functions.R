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

# function to set up dplyr namespace conflicts
set_dplyr_defaults <- function() {
  summarize = dplyr::summarise
  select = dplyr::select
}

configure_fixest <- function(
    lean = TRUE,
    mem.clean = FALSE,
    fixef.iter = 10000,
    fixef.tol = 1e-8,
    collin.tol = 1e-9,
    verbose = 2
) {
  setFixest_fml(
    ..econ_local = ~unemp_local + lfpr_local + log_incom_per_capita,
    ..econ_nat = ~jobs_nat + pce_nat + rdpi_nat + cpi_nat + 
      ics_nat + sp500_nat + unemp_nat,
    ..county_demo = ~share_black + share_hisp + share_other + 
      share_u18 + share_65plus,
    ..cpr = ~cpr_likely_r + cpr_lean_r + cpr_toss_up + 
      cpr_lean_d + cpr_likely_d + cpr_solid_d,
    reset = TRUE
  )
  
  setFixest_estimation(
    lean = lean,
    mem.clean = mem.clean,
    fixef.iter = fixef.iter,
    fixef.tol = fixef.tol,
    collin.tol = collin.tol,
    verbose = verbose
  )
  
  return(TRUE)
}



# function to create scatter plot
scatter <- function(data, x_var, y_var, 
                                 title = NULL,
                                 subtitle = NULL,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 color = "#2171B5",
                                 add_correlation = TRUE,
                                 add_regression = TRUE,
                                 point_alpha = 0.6,
                                 point_size = 2) {
  
  # Calculate correlation
  cor_val <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    # Add points
    geom_point(color = color, 
               alpha = point_alpha, 
               size = point_size) +
    # Add regression line if requested
    {if(add_regression) 
      geom_smooth(method = "lm", 
                  color = "#EF3B2C", 
                  fill = "#FEE0D2", 
                  alpha = 0.2)} +
    # Add correlation annotation if requested
    {if(add_correlation)
      annotate("text", 
               x = -Inf, y = Inf,
               label = sprintf("r = %.3f", cor_val),
               hjust = -0.2, 
               vjust = 2,
               size = 4,
               fontface = "italic")} +
    # Customize theme
    theme_minimal() +
    theme(
      # Title and subtitle formatting
      plot.title = element_text(size = 16, face = "bold", 
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "gray30",
                                   margin = margin(b = 20)),
      # Axis formatting
      axis.title = element_text(size = 12, color = "gray30"),
      axis.text = element_text(size = 10),
      # Grid formatting
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      # Add padding
      plot.margin = margin(30, 30, 30, 30)
    ) +
    # Add custom labels if provided
    labs(
      title = title %||% paste("Relationship between", x_var, "and", y_var),
      subtitle = subtitle,
      x = x_lab %||% gsub("_", " ", x_var),
      y = y_lab %||% gsub("_", " ", y_var)
    )
  
  return(p)
}
