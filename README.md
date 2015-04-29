# scripts for preparing and doing energy usage data analysis

Order of usage:

1. Command line: merge_files_dir_dir
2. Command line: create_csv_file
3. R: analyze_statsig
4. R: analyze_tradeoff_alternatives (use top_par = -1 for exhaustive exploration)
5. R: analyze_alloc_sites
6. R: count_implementations --> generate a file with the number of implementations per subject per site, counting them in total and the ones included in the top most frequent ones (selected as energy-efficient implementations)
7. R: compute_cost --> compute_cost_alternatives, compute_cost_sites, compute_cost_combined
