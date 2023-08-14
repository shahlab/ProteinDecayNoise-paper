# Evaluating noise in protein degradation
This repo is related to the data that went into the paper titled "Evaluating noise in protein decay."<br>
Folder descriptions: 
## data
The "**data**" folder contains 4 files in CSV format which contain all the data points that were used to create the figures.
The CSVs "_paper_pup1_param_cell.csv_" and "_paper_pup1_timeseries.csv_" were used to generate figures 2, 3, 5 and supplemental figures S2, and S5. <br>
The CSVs "_paper_pup1_prtinh_param_cell.csv_" and "_paperr_pup1_prtinh_timeseries.csv_" were used to generate figure 4 and supplemental Figure S3. 

## code
The "**code**" folder contains all the code that was used to generate the data. The description of each sub-directories in the "code" folder is as follows:

**Figures**: 

 has the code that generates the figures in the paper.

**generatee_df**: 
see Methods/Data analysis/Single-cell fluorescent intensity calculations. 

  Sub-dir "generate_dfs" contains code that utilizes the data from the folder "single_cell_GFP_analysis.tar.gz" on DryAd to create CSVs with background and autofluorescence subtracted single-cell GFP intensity information from each timepoint and a CSV that contains  all the other cellular attributes like area, DNA staining, sphericity, etc. of single cells at the first timepoint. 
 
  Dead cells and blurry cells were filtered using DAPI stain and by removing cells with an abrupt increase in GFP intensity throughout the course of the timelapse. 

**r_functions**: 

Has the source file to be used with the Rmds in the "_generate_dfs_" folder.  <br> 

**modelling**: 

"all_exp_data1":  file combines the CVS with single-cell GFP timelapse information from all the experiments into one CSV.<br>
It also filters out cells with GFP information for less than 31 time points.<br>
"all_exp_cell_attr": file combines the CSVs with single-cell attributes from all the experiments into one CSV. <br>
"mechanistic_all": Takes the CVS generated from "all_exp_data1.Rmd" and estimates the single-cell parameters for the mechanistic model of decay. See section methods/parameter estimation and methods/Modelling.<br>

**segmentation**: 

YS_array_jobs_1.sh : the shell script used to parallelize the image segmentation using the YeastSpotter tool. <br>
See: https://github.com/alexxijielu/yeast_segmentation for dependencies. 
 
  *useful-code*: 
  
  "mv_images_to_folder.pl": The code to reorganize each image into it's own single folder to facilitate the segmentation with YeastSpotter and where the segmented masks will be saved. <br>
  "mv_imagej.pl": code to move all the segmented masks into one folder.<br>
  
**imageJ**:

The code in the folder pertains to the Methods/Data analysis/Image processing. <br>
The macros for background intensity estimation, getting the exact time of acquisition of the images, and converting all the images (fluorescent intensities and the single-cell mask) to 16bit (for analysis with Imaris) are in the folder named "imageJ." <br>

In all the macros scripts, replace the first two lines in the code with <br>
1.input_dir: the path to where the raw images are downloaded from DryAd.<br>
2. output_dir: The path to where the results are to be stored.<br>

Create folders named <br>
    "background/bg_gfp", <br>
    "background/iimage_bg_roi",<br>
     "background/bg_dapi_mcherry",<br> 
    "background/image_bg_roi_dapi_mcherry"<br>
    "image_info_imageJ" <br>
in the folder with raw images. <br>

File Description: 

  "saving_image_info_nestedfolders.ijm": strips the image acquisition information for each image in the time lapse experiment. <br>
  "convert_8bit_to_16bit.ijm" : Converts all the images to 16bit for image analysis witth Imaris. <br>
  "bg_intensity.ijm": get the background intensity of the GFP fluorescent images.<br>
  "bg_intensity_nuc-dapi-pup1-mcherry.ijm": gets the background intensity of the DAPI (NucBlue) and mCherry fluorescent images.<br>

## fig_pdfs
The "**fig_pdfs**" folder contains the figures in pdf format. 
