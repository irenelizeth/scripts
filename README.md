# scripts for organizing and preparing energy usage data for analysis

Order of usage:

1. Command line: merge_files_dir_dir
2. Command line: create_csv_file
3. R: analyze_statsig
4. R: analyze_tradeoff_alternatives (use top_par = -1 for exhaustive exploration)
5. R: analyze_alloc_sites
6. compute_costs --> compute_cost_alternatives, compute_cost_sites, compute_cost_combined
