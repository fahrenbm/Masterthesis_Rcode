# Masterthesis_Rcode
Here are my R codes which I used to create my data and analysis for my master thesis. 
You should run the script in the order of the number to contain all calculations and results. The scripts contain the plots can be skippet. 03 contain the important ones


First: 0_packages_data_functions.R
This script contains everything which is necessary for all the other scripts:
  - all needed packages
  - Input datasets and the pre-processing of them
  - creation of function and variables

Second: 1_event_stat.R
In this script the calculation of the event-based statistics is done. The resulting data frame is needed for the following scripts

Third: 1_1_event_stat_plot.R
This script contains the code for the analysis. Therefore, calculations and plots are done based on the data frame from the previous script

Fourth: 2_monthly_tendency.R
Here monthly tendencies are calculated on basis of the raster averages for two different outcomes:
  - average over area and month (to see tendencies over the month)
  - average over month (to see spatial patterns, differences)

Fifth: 3_final_graphs.R
Here the final graphs for my thesis are created

The scripts 4_sec_approach contains the second approach which was at the end not used for my thesis.
Basically, I searched for the spatiotemporal point of each event where the highest reduction in GPP was calculated and calculated therefore as well statistics
In script 4_01_sec_approach_plots these results are plotted
