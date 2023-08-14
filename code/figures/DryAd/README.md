#  GFP-timelapse:
---

The zipped files contain raw image files from timelapse experiments described in the paper "".

## Description of the Data and file structure
### Raw images:
TIF files from metamorph software version 7.10.3.279.
Following is the description of the file names in the zipped folders *06-28-22-modc-pup1.tar.gz*,*7-20-22-cln2.tar.gz*,*7-20-22-yeGFP.tar.gz*,*10-7-22-yeGFP.tar.gz*,*10-7-22-cln2.tar.gz*:


    [GFP]_[experiment type]_[timelapse duration]_[attempt]_[channel]_[field of view]_[timelapse series no.].TIF
        [GFP] = type of GFP either:
                8hr = yeGFP, 
                30mgfp = yeGFP-CLN2, 
                20mgfp = yeGFP-mODC.
        [experiment type] = tl => timelapse 
        [timelapse duration] == 20mins
        [attempt] == 1,2,3
        [channel] = "w1BF-PH" == DIC images
                    "w2TxRed" == fluorescent (fl) images of Pup1-tDimer in Texas red channel 
                    "w3GFP "== GFP fl images, 
                    "w4UV FL"== Hoechst 333xx fl images for DNA stain 
        [field of view] == s1:s8 correspond to individual fields of view.
                            s1,s2: Autofluorescent control (SDY010 strain) 
                            s3,s4: two fields of view from biological replicate 1. 
                            s5,s6: two fields of view from biological replicate 2. 
                            s7,s8: two fields of view from biological replicate 3.

        [timelpase series no.] = t1:t31: describes timelapse image no. 
        from 1 through 31. 
Following is the description of the files contained in the zipped folders:
*06-28-22-modc-pup1.tar.gz*: raw timelapse images for cells expressing yeGFP-mODC in SDY011 strain. 

*7-20-22-cln2.tar.gz*: raw timelapse images for cells expressing yeGFP-CLN2 in SDY011 strain.

*7-20-22-yeGFP.tar.gz*: raw timelapse images for cells expressing yeGFP in SDY011 strain.

*10-7-22-yeGFP.tar.gz* : raw timelapse images for cells expressing yeGFP in SDY011 strain repeat experiment.

*10-7-22-cln2.tar.gz*: raw timelapse images for cells expressing yeGFP-CLN2 in SDY011 strain repeat experiment.

*4-1-22-PI-modc-pup1.tar.gz*: raw timelapse images for cells expressing yeGFP-mODC in SDY011 strain treated with eithere control (DMSO) or with 1 $\mu$M, $2.5\mu$ and $5\mu$M of proteasome inhibitor MG132.  

### Restructured raw images to be analyzed with Imaris: 
Imaris version used: Imaris suite (version 9.8)
Zipped folder with **for_imaris_** in the name contains directories corresponding to each experimental data mentioned above. Images from each field of veiw are clubbed together in individual folders. Each of these folders contains a subfolder with fluorescent images from the first and last timepoint as fluorescent images of pup1-tdimer and DNA stain were taken only at the first and last timepoint. The images are stuctured in a way to facilitate opening of the timelapse series direrctly in Imaris as a stack. 
The files were renamed to have following structure: 
```
[GFP]_tl_20min_[1]_channel_[s1]_[t1]_[fluorescent_channel].TIF
Where [fluorescent_channel] is one of: 
    C0 : single-cell segmented mask (binary, 0 or 255)
    C1: DIC images
    C2 : GFP images
    C3 : TxRed images (Pup1-tDimer)
    C4 : UV (DNA stain)
```
Each folder contains the raw ".TIF" files and a ".ims" file which corresponds to the final imaris file containing the segmentation of cells as surfaces which are tracked in each image and contains statistics associated with each cell. 

The folder named "batch_programs" contains files which have parameters used for the creation of surface objects to identify and track each cell. 
"pup1.icx" = parameters to identify punctas as surface oobjects using TxRed images. 
"YS_SD_GFP.icx" = parameters to identify single cells as surface objects using the binary mask of the segmented cells, to track each cell in every timelapse  image, and to exclude cells on the boundary of the image.  
"cells.icx" = same as YS_SD_GFP.icx but without tracking of cells (to be used with pup1.icx on images in the folder with just the first and last timepoint). 

### Data structure for single cell GFP analysis
The directories in the zipped folder named *single_cell_GFP_analysis.tar.gz* have the following directory structure:
```
.
└── Imaris-data
    └── 2022
        └── pup1_rfp
            ├── 10-7-22
            │   ├── cln2
            │   │   ├── background
            │   │   ├── image_info_imageJ
            │   │   └── imaris
            │   └── stable
            │       ├── background
            │       ├── image_imfo_imageJ
            │       └── imaris
            ├── 6-28-22
            │   ├── (A Document Being Saved By AUHelperService)
            │   └── modc-pup1
            │       ├── background
            │       ├── image_info_imageJ
            │       └── imaris
            └── 7-20-22-pup1
                ├── cln2
                │   ├── background
                │   ├── image_info_imageJ
                │   └── imaris
                └── stable
                    ├── background
                    ├── image_info_imageJ
                    └── imaris
```
*single_cell_GFP_analyssis-PI.tar.gz* directory structure:

```                    
    imaris-data-PI
    └── 2022
    └── proteasome_inhibition
        └── 4-1-22-PI
            ├── 1uM
            │   ├── background
            │   ├── bg_roi
            │   ├── image_info_imageJ
            │   └── imaris
            ├── 2.5uM
            │   ├── background
            │   ├── bg_roi
            │   ├── image_info_imageJ
            │   └── imaris
            ├── 5uM
            │   ├── background
            │   ├── bg_roi
            │   ├── image_info_imageJ
            │   └── imaris
            └── DMSO
                ├── background
                ├── bg_roi
                ├── image_info_imageJ
                └── imaris


These should be used with RMDs in github: https://github.com/shahlab/ProteinDecayNoise-paper to generate the data frames that was used to estimate the paramters of the models in the paper Estimating single-cell variability in proteasomal decay. 
   