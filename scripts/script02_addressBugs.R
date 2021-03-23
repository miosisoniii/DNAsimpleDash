#-------------------------------------------------------------------------------------#
# Project: DNASimpleDash
# Purpose: Correct any Shiny application errors
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# ERROR 01 - output$grouped_topethnicity
# Text to be written must be a length-one character vector
#-------------------------------------------------------------------------------------#
## solved 22Mar21
## adjusted top_n to "head(n = 1)" to select the top ethnicty. found in server.R, 

#-------------------------------------------------------------------------------------#
# ERROR 02 - parsingfailures
#-------------------------------------------------------------------------------------#

