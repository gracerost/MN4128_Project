# In order to submit your application assignment, you need to capture your R codes, annotations, and output in one file
# This R script will accomplish such task by saving everything into a html file, you should always run this submission script when you are ready to submit assignment
# IMPORTANT REMINDER: make sure 
# (a) you have no errors in the R script you supply before you call the submission R script; 
# (b) your directory is set correctly (same directory as the file you supply to render function in line 17);
# (c) you supply the correct file name to the render function.
# (d) DO NOT copy/paste the render function code (i.e., line 18 in this file) directly at the end of your homework R script. Homework R file is the input file for the render function. 

# Author: Yu-Chu Shen
# Date: Dec 2025

# make sure you have installed the following package: rmarkdown
library(rmarkdown)

# For each assignment submission, make sure you change the path/filename to the corresponding assignment according to submission instruction

render("C:/Users/rostg/OneDrive/Documents/NPS-G-Laptop/FY26 Q2/MN4128/MN4128_Project/project.R", output_format="html_document")


