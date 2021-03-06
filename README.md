# WSA Analysis
Scripts for organizing data towards the analysis of submissions gained as part of the consultation process for British Columbia's Water Sustainability Act . 

This is the read me file to accompany scripts used in analysis of BC Water Sustainability Act Modernization process. All files here were used to analyze data contained within the upcoming publication:

The aim of this paper was to examine consultation process and policy outcomes associated with BC's new Water Sustainability Act. Thus, it provides qualitative and quantitative analysis of both consultation submissions to the process, as well as outcomes contained within the Act.

Two scrips were used to parse and organize individual submissions to the consultation process, which are all available through the BC Ministry of Environment's Website:

1. "WaterAct_splitfiles.R" 

This function takes a specified, random percentage of files within a folder and saves the selected files to a new folder. 

In the case of our specific analysis,  we used this function  to choose a random 10% of individual submissions from all stages for analysis. All of the submission files for individuals were were downloaded from the Ministry of Enviornment's website, with full attribution to the exhaustive work they did in collecting and compiling these documents (https://engage.gov.bc.ca/watersustainabilityact/whatweheard/). All individual submissions were saved within three folders, divided according to submission stage. This function  selected a random percentage (which the user can control - 10% in our case) from each of the defined folders according to defined file type (in this case, .pdf), and save into a new folder for further analysis. 

2. "WaterAct_analysis_wordanalysis.R"
This script will take a number of documents, and clusters them into groups according to similarities in word count and word frequencies.

In our analysis, a large percentage of individual submissions came from form letters that were quite similar. We wanted to identify these submissions and group ones that were the same in order to streamline qualtitative analysis (coding) in NVivo. Form submissions were clustered using this script on the basis of similar word counts and similarities in word frequencies between different submissions. 

It also performs optional basic language processing functions, specifically assocation of words of interest (such as 'protect', 'public, etc) that the user can specify.
