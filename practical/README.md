# Loading and Plotting Data associated with Publication

The two R scripts are meant to reproduce Fig. 5 in the publication Waldrop et al. 2016 (included as 20160615.pdf). 
 * data-entry.R: loads data from each replicate set (folders set1, set2, set3). This script should be run first.
 * totalhairs_calculate_plot.R: loads some data and does some calculations. Only use the lines in this script that are relevant to the next script.
 * calculate-plot2.R: calculates the mean and confidence intervals of the graphs and creates graphs using the ggplot2 package. This script should be run second.

The code works on my original machine (either the home or work location) to reproduce the figures. However, because of the hard-coding of locations, it will not work for you without edits. 

Your jobs are to: 
 1. Reorganize the code so it adheres to best practices (including creation of an R project file).
 2. Refactor the code so that it is easier to read and more versatile. 
 3. Optimize the code for speed and memory usage (if possible). 
 4. Improve the documentation so that an average user will understand it. 
 5. Add additional functionality: 
    * separate functions into a separate, dedicated script file. 
    * create an RMarkdown file that plots the figure for the user with some information about the figure.
    * remove any unused code. 
    
Be sure to separate code improvements in git commits. 

Also, __include a list of improvements in a markdown file__ and turn this in as part of the project submission. 