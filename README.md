# texas-voting-access

Use these scripts to reproduce the analysis contained in the paper [*Access to Secure Ballot Drop-off Locations in Texas*](https://findingspress.org/article/24080-access-to-secure-ballot-drop-off-locations-in-texas) authored by Alex Karner (UT-Austin) and Dana Rowangould (Univ. Vermont). 

The scripts generate all required travel times, clean and prepare all demographic data, calculate required scores, and report results. Each is described below:
1. *01-transit-analysis.R*: Use r5r to calculate travel times from every origin block group in Texas to the closest ballot drop-off location. 
2. *02-auto-analysis.R*: Process automobile travel times previously calculated using ESRI Streetmaps Premium. 
3. *03-demographic-analysis.R*: Retrieve relevant census data 
4. *04-results.R*: Generate figures and tables to include in the paper. 

![Harris County results](/output/Fig1_HarrisTimes.png)