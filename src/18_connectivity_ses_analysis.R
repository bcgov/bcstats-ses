# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# =============================================================================
# SCRIPT: 18_connectivity_ses_analysis.R
#
# PURPOSE:
#   Analyze the correlation between internet connectivity (PHH/NBD and CITZ data)
#   and socio-economic status (SES) indices at the CSD level using 2023 data.
#
# DATA SOURCES:
#   1. Connectivity data from scripts 16/17:
#      - csd_comparison.csv: CSD-level connectivity comparison
#   2. SES data (CSD level, 2023):
#      - robust-csd-weighted-scores-2025-07-21_masked.csv: Detailed model (2023 only)
#
# WHAT THIS SCRIPT DOES:
#   1. Load connectivity data from scripts 16/17 outputs
#   2. Load SES indices from robust-csd file (2023)
#   3. Join connectivity and SES data by csd_code
#   4. Compute correlations between connectivity metrics and SES indices
#   5. Visualize relationships
#   6. Export results for PowerBI
#
# OUTPUTS:
#   - connectivity_ses_csd.csv: CSD-level joined dataset
#   - correlation_results.csv: Correlation analysis results
#   - correlation_summary.csv: Summary statistics
#   - plots/: Visualization plots
# =============================================================================

# =============================================================================
# SECTION 1: SETUP AND CONFIGURATION
# =============================================================================

# ---- Packages ----
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(corrplot)
library(stats)
library(logger)
library(glue)

# ---- Configuration ----
config <- config::get()

lan_path <- config$lan_path
connectivity_data_path <- config$conectivity_data_path
ses_data_path <- file.path(
  lan_path,
  "2024 SES Index/exports/2025-07-31-initial-index-results"
)

output_path <- file.path(lan_path, connectivity_data_path, "outputs")
dir.create(output_path, showWarnings = FALSE)

# ---- Logger setup ----
log_threshold(INFO)
log_dir <- file.path(output_path, "logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(
  log_dir,
  paste0("18_connectivity_ses_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)
log_appender(appender_tee(log_file))
log_info("Starting 18_connectivity_ses_analysis.R")
log_info("Log file: {log_file}")
log_info("Output path: {output_path}")

# =============================================================================
# SECTION 2: LOAD DATA
# =============================================================================

# ---- 2.1) Load Connectivity Data ----
log_info("==== Loading Connectivity Data ====")

conn_output_path <- output_path

# Load CSD-level connectivity comparison
csd_comp_path <- file.path(conn_output_path, "csd_comparison.csv")
if (file.exists(csd_comp_path)) {
  csd_conn <- read_csv(csd_comp_path, show_col_types = FALSE)
  log_info(
    "CSD connectivity loaded: {nrow(csd_conn)} rows, {ncol(csd_conn)} cols"
  )
} else {
  log_warn("CSD comparison file not found: {csd_comp_path}")
  csd_conn <- NULL
}

# ---- 2.2) Load SES Data (CSD Level, 2023) ----
log_info("==== Loading SES Data (CSD Level, 2023) ====")

# Load Robust (Detailed) model - 2023 only
ses_robust_path <- file.path(
  ses_data_path,
  "robust-csd-weighted-scores-2025-07-21_masked.csv"
)
log_info("Loading Robust (Detailed) SES from: {ses_robust_path}")
ses_robust <- read_csv(ses_robust_path, show_col_types = FALSE)
log_info("Robust SES loaded: {nrow(ses_robust)} rows, {ncol(ses_robust)} cols")

# Use Robust (Detailed) model as primary - it's the most current and detailed
ses_latest <- ses_robust %>%
  filter(CALENDAR_YEAR == 2023) %>%
  rename(
    csd_code = CSD_UID,
    csd_name = MUN_NAME_2021
  ) %>%
  select(
    csd_code,
    csd_name,
    pop_estimate = POPULATION_ESTIMATE,
    total_index = TOTAL_INDEX_0_100,
    sei_index = SEI_INDEX_0_100,
    econ_index = ECON_0_100,
    educ_index = EDUC_0_100,
    health_index = HEALTH_0_100,
    community_index = COMMUNITY_0_100
  )

log_info("SES CSD 2023 data: {nrow(ses_latest)} CSDs")
log_info("SES columns: {paste(names(ses_latest), collapse = ', ')}")

# =============================================================================
# SECTION 3: GEOGRAPHIC MAPPING
# =============================================================================
log_info("==== Creating Geographic Mapping ====")

# Verify CSD codes overlap between connectivity and SES data
csd_conn_codes <- unique(csd_conn$csd_code)
ses_csd_codes <- unique(ses_latest$csd_code)

csd_overlap <- intersect(
  na.omit(as.integer(csd_conn_codes)),
  na.omit(as.integer(ses_csd_codes))
)
log_info("Overlapping CSD codes (connectivity ↔ SES): {length(csd_overlap)}")

# =============================================================================
# SECTION 4: JOIN CONNECTIVITY AND SES DATA
# =============================================================================
log_info("==== Joining Connectivity and SES Data ====")

# CSD-Level Join
if (!is.null(csd_conn) && nrow(ses_latest) > 0) {
  csd_joined <- csd_conn %>%
    left_join(ses_latest, by = "csd_code")

  log_info("CSD join by csd_code: {nrow(csd_joined)} rows")

  # Filter to rows with both connectivity and SES data
  csd_analysis <- csd_joined %>%
    filter(!is.na(total_index) | !is.na(sei_index))

  log_info("CSD analysis-ready rows: {nrow(csd_analysis)}")
} else {
  csd_analysis <- NULL
  log_warn("Cannot create CSD analysis dataset")
}

# =============================================================================
# SECTION 5: CORRELATION ANALYSIS
# =============================================================================
log_info("==== Computing Correlations ====")

# ---- 5.1) Define Connectivity Metrics for Correlation ----
# Using mean_delta columns from CSD data (CITZ - PHH difference)
conn_vars <- c(
  "mean_delta_combined_50_10",
  "mean_delta_combined_25_5",
  "mean_delta_combined_10_2",
  "mean_delta_combined_5_1",
  "mean_delta_wired_50_10",
  "mean_delta_wired_25_5",
  "mean_delta_wired_10_2",
  "mean_delta_wired_5_1",
  "mean_delta_wireless_50_10",
  "mean_delta_wireless_25_5",
  "mean_delta_wireless_10_2",
  "mean_delta_wireless_5_1"
)

# ---- 5.2) Define SES Indices ----
ses_vars <- c(
  "total_index",
  "sei_index",
  "econ_index",
  "educ_index",
  "health_index",
  "community_index"
)

# ---- 5.3) Compute Correlations ----
compute_correlations <- function(df, conn_vars, ses_vars, geo_level = "CSD") {
  if (is.null(df) || nrow(df) < 10) {
    log_warn("Insufficient data for correlation analysis")
    return(NULL)
  }

  # Ensure SES columns are numeric
  for (sv in ses_vars) {
    if (sv %in% names(df)) {
      df[[sv]] <- as.numeric(df[[sv]])
    }
  }

  results <- data.frame()

  for (conn_var in conn_vars) {
    if (!conn_var %in% names(df)) next

    for (ses_var in ses_vars) {
      if (!ses_var %in% names(df)) next

      # Get complete cases
      test_data <- df %>%
        select(all_of(c(conn_var, ses_var))) %>%
        filter(complete.cases(.))

      if (nrow(test_data) < 30) {
        log_debug(
          "Skipping {conn_var} vs {ses_var}: only {nrow(test_data)} complete cases"
        )
        next
      }

      # Compute Pearson correlation
      test_result <- cor.test(
        test_data[[conn_var]],
        test_data[[ses_var]],
        method = "pearson",
        use = "complete.obs"
      )

      # Compute Spearman correlation
      spearman_result <- cor.test(
        test_data[[conn_var]],
        test_data[[ses_var]],
        method = "spearman",
        use = "complete.obs"
      )

      results <- bind_rows(
        results,
        data.frame(
          geo_level = geo_level,
          connectivity_metric = conn_var,
          ses_index = ses_var,
          n = nrow(test_data),
          pearson_r = test_result$estimate,
          pearson_p = test_result$p.value,
          spearman_r = spearman_result$estimate,
          spearman_p = spearman_result$p.value,
          significance = ifelse(
            test_result$p.value < 0.001,
            "***",
            ifelse(
              test_result$p.value < 0.01,
              "**",
              ifelse(test_result$p.value < 0.05, "*", "")
            )
          )
        )
      )
    }
  }

  return(results)
}

# Compute correlations at CSD level
if (!is.null(csd_analysis)) {
  csd_correlations <- compute_correlations(
    csd_analysis,
    conn_vars,
    ses_vars,
    "CSD"
  )
  log_info("CSD correlations computed: {nrow(csd_correlations)} pairs")

  # Print significant correlations
  sig_corr <- csd_correlations %>%
    filter(pearson_p < 0.05) %>%
    arrange(desc(abs(pearson_r)))

  if (nrow(sig_corr) > 0) {
    log_info("Significant correlations (p < 0.05):")
    for (i in 1:min(10, nrow(sig_corr))) {
      log_info(
        "  {sig_corr$connectivity_metric[i]} ↔ {sig_corr$ses_index[i]}: r={round(sig_corr$pearson_r[i], 3)} {sig_corr$significance[i]}"
      )
    }
  }
} else {
  csd_correlations <- NULL
}

# =============================================================================
# SECTION 6: VISUALIZATIONS
# =============================================================================
log_info("==== Creating Visualizations ====")

plot_dir <- file.path(output_path, "plots")
dir.create(plot_dir, showWarnings = FALSE)

# ---- 6.1) Correlation Heatmap ----
if (!is.null(csd_correlations) && nrow(csd_correlations) > 0) {
  # Create matrix, handling NA values
  corr_wide <- csd_correlations %>%
    select(connectivity_metric, ses_index, pearson_r) %>%
    pivot_wider(names_from = ses_index, values_from = pearson_r, values_fill = NA)
  
  if (nrow(corr_wide) > 0 && ncol(corr_wide) > 1) {
    corr_matrix <- corr_wide %>%
      column_to_rownames("connectivity_metric") %>%
      as.matrix()
    
    # Remove rows/cols that are all NA
    valid_rows <- rowSums(!is.na(corr_matrix)) > 0
    valid_cols <- colSums(!is.na(corr_matrix)) > 0
    corr_matrix <- corr_matrix[valid_rows, valid_cols, drop = FALSE]
    
    if (nrow(corr_matrix) > 1 && ncol(corr_matrix) > 1) {
      # Debug: log matrix info
      log_info("Correlation matrix: {nrow(corr_matrix)} x {ncol(corr_matrix)}")
      log_info("Row names: {paste(rownames(corr_matrix), collapse = ', ')}")
      
      png(
        file.path(plot_dir, "correlation_heatmap.png"),
        width = 1200,
        height = 800
      )
      corrplot(
        corr_matrix,
        method = "color",
        type = "upper",
        order = "original",
        addCoef.col = "black",
        tl.col = "black",
        tl.srt = 45,
        diag = FALSE,
        title = "Correlation: Connectivity vs SES Indices (CSD Level)",
        mar = c(0, 0, 2, 0)
      )
      dev.off()
      log_info("Correlation heatmap saved")
    } else {
      log_warn("Not enough data for correlation heatmap")
    }
  }
}

# ---- 6.2) Scatter Plots for Top Correlations ----
if (!is.null(csd_analysis) && nrow(csd_analysis) > 0) {
  if (!is.null(csd_correlations) && nrow(csd_correlations) > 0) {
    top_pairs <- csd_correlations %>%
      filter(pearson_p < 0.05) %>%
      arrange(desc(abs(pearson_r))) %>%
      head(6)

    for (i in 1:nrow(top_pairs)) {
      conn_var <- top_pairs$connectivity_metric[i]
      ses_var <- top_pairs$ses_index[i]

      p <- ggplot(csd_analysis, aes_string(x = conn_var, y = ses_var)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        labs(
          title = paste("Connectivity vs SES:", conn_var, "vs", ses_var),
          subtitle = paste(
            "r =",
            round(top_pairs$pearson_r[i], 3),
            ifelse(
              top_pairs$significance[i] != "",
              paste("(", top_pairs$significance[i], ")", sep = ""),
              ""
            )
          ),
          x = conn_var,
          y = ses_var
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))

      ggsave(
        file.path(
          plot_dir,
          paste0("scatter_", conn_var, "_vs_", ses_var, ".png")
        ),
        plot = p,
        width = 8,
        height = 6,
        dpi = 150
      )
    }
    log_info("Scatter plots saved: {nrow(top_pairs)} plots")
  }
}

# ---- 6.3) Distribution Plots ----
if (!is.null(csd_analysis) && nrow(csd_analysis) > 0) {
  for (ses_var in ses_vars) {
    if (ses_var %in% names(csd_analysis)) {
      p <- ggplot(csd_analysis, aes_string(x = ses_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        labs(
          title = paste("Distribution of", ses_var, "at CSD Level"),
          x = ses_var,
          y = "Count"
        ) +
        theme_minimal()

      ggsave(
        file.path(plot_dir, paste0("distribution_", ses_var, ".png")),
        plot = p,
        width = 8,
        height = 6,
        dpi = 150
      )
    }
  }
  log_info("Distribution plots saved")
}

# =============================================================================
# SECTION 7: EXPORT RESULTS
# =============================================================================
log_info("==== Exporting Results ====")

# ---- 7.1) Export Joined Dataset ----
if (!is.null(csd_analysis)) {
  csd_out_path <- file.path(output_path, "connectivity_ses_csd.csv")
  write_csv(csd_analysis, csd_out_path)
  log_info(
    "CSD connectivity + SES saved: {csd_out_path} ({nrow(csd_analysis)} rows)"
  )
}

# ---- 7.2) Export Correlation Results ----
if (!is.null(csd_correlations)) {
  corr_out_path <- file.path(output_path, "correlation_results.csv")
  write_csv(csd_correlations, corr_out_path)
  log_info(
    "Correlation results saved: {corr_out_path} ({nrow(csd_correlations)} rows)"
  )
}

# ---- 7.3) Export Summary Statistics ----
if (!is.null(csd_analysis)) {
  summary_stats <- csd_analysis %>%
    summarise(
      n_csd = n(),
      mean_delta_50_10 = mean(mean_delta_combined_50_10, na.rm = TRUE),
      median_delta_50_10 = median(mean_delta_combined_50_10, na.rm = TRUE),
      mean_sei = mean(sei_index, na.rm = TRUE),
      median_sei = median(sei_index, na.rm = TRUE),
      mean_total = mean(total_index, na.rm = TRUE),
      median_total = median(total_index, na.rm = TRUE)
    )

  summary_out_path <- file.path(output_path, "correlation_summary.csv")
  write_csv(summary_stats, summary_out_path)
  log_info("Summary statistics saved: {summary_out_path}")
}

# =============================================================================
# SECTION 8: FINAL SUMMARY
# =============================================================================
log_info("==== FINAL SUMMARY ====")
log_info("Analysis Complete!")
log_info("")
log_info("Files created:")
log_info("  1. connectivity_ses_csd.csv - CSD-level joined dataset")
log_info("  2. correlation_results.csv - Detailed correlation results")
log_info("  3. correlation_summary.csv - Summary statistics")
log_info("  4. plots/ - Visualization plots")
log_info("")

if (!is.null(csd_correlations)) {
  log_info("Key Findings:")
  sig_strong <- csd_correlations %>%
    filter(pearson_p < 0.05, abs(pearson_r) > 0.3) %>%
    arrange(desc(abs(pearson_r)))

  if (nrow(sig_strong) > 0) {
    for (i in 1:min(5, nrow(sig_strong))) {
      log_info(
        "  - {sig_strong$connectivity_metric[i]} ↔ {sig_strong$ses_index[i]}: r={round(sig_strong$pearson_r[i], 3)} ({sig_strong$significance[i]})"
      )
    }
  } else {
    log_info("  No strong correlations found (|r| > 0.3)")
  }
}

log_info("")
log_info("Done: 18_connectivity_ses_analysis.R")
