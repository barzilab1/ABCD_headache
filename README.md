##########
# ABCD_headache project


#### This project uses the following ABCD instruments [version 4.0]:
1. pdem02
2. abcd_lpds01
3. abcd_ydmes01
4. abcd_nsc01
5. abcd_pnsc01
6. abcd_fhxssp01
7. abcd_yle01
8. acspsw03
9. abcd_cb01
10. abcd_rhds01
11. abcd_sscey01
12. abcd_sscep01
13. medsy01
14. abcd_mx01
15. abcd_lpmh01
16. abcd_otbi01
17. abcd_medhxss01
18. abcd_lssmh01
19. abcd_tbi01
20. abcd_lsstbi01
21. abcd_mhy02
22. abcd_mhp02
23. abcd_lt01



#### How to run the code:

1. Update the [config.R](config.R) to reflect the location of the instruments above.
2. In the data-scripts folder, run scripts in any order. These scripts go over the abcd instruments and create new variables and datasets that are placed in the “outputs” folder.
3. Run the [merge.R](scripts/merge.R) and [create_wide_data.Rmd](scripts/create_wide_data.Rmd) script to create the long and wide format dataset.
4. Run the [create_descriptive_tables.Rmd](scripts/create_descriptive_tables.Rmd) to generate tables of the main paper and supplement.
5. Run the [main_analyses.Rmd](scripts/main_analyses.Rmd) to generate main results.
6. Run the [create_forest_plot.Rmd](scripts/create_forest_plot.Rmd) to generate the forest plot (figure 2).
7. Run the [sensitivity analysis.Rmd](scripts/sensitivity analysis.Rmd) to generate eTable 7 and 8.
