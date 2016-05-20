This is the read me file to accompany scripts used in analysis of BC Water Sustainability Act Modernization process. All files here were used to analyze data contained within the upcoming publication:

The function of the three script files are:

1.  "WaterAct\_split files.R" This script takes a random percentage of files within a folder and saves to a new folder. This was done to choose a random 10% of individual submissions from all stages for analysis. In the case of our specific analysis, all of the submission files for individuals were saved within three folders, divided according to submission stage. The script will choose a random percentage (which the user can conrol) from each of the defined folders=, according to defined file type (in this case, .pdf), and save into a new folder for further analysis.

All files were downloaded from the Ministry of Enviornment's website, with full attribution to the exhaustive work they did in collecting and compiling these documents.

<https://engage.gov.bc.ca/watersustainabilityact/whatweheard/>

PDF files were also split into individual pages, noting that individual files were bundled into several PDF files. Split PDF files were saved into three directories according to submission stage.

``` r
# Possible user directed changes to script for selecting 
# directories for PDFs
directory_1 <-"" # location of PDF files for
directory_2 <-"" #where all of the split PDFs are located
directory_3 <-"" #where all of the split PDFs are located

#path for selected files - place where selected files will be saved
Selected_individual <- ""
```

This script allows for selection of the percentage of files to be randomly sampled, as well as the type of file

``` r
##
per = 10          # Percent you are selecting. Change if necessary. This was 10% in our case
filetype =".pdf$" # extension that the files are saved as. PDF in this case (default from MoE).
```

1.  "WaterAct\_analysis\_wordanalysis.R"

A large percentage of individual submissions came from form letters that were quite similar. We wanted to identify these submissions and group ones that were the same in order to streamline qualtitative analysis (coding) in NVivo.

This script identifies and clusters submissions on the basis of similarities in both word frequency and count.

It also performs optional basic natural language processing functions, specifically assocation of words of interest (such as 'protect', 'public, etc)

1.
