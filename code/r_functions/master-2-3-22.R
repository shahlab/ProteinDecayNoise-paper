

#-------Getting Image info------------------------------------
#Process the imageJ image info files to get the real time difference between the images takens

##Function 1 : get.image.info()

####this function is to exract the real time differences between each image that was taken in the time lapse series 
get.image.info.OnlyGFP <- function(list.image.info){
  #extracting the info of file, experiment, field of view, timepoint and the channel from the image_info text. 
  image.info <- lapply(list.image.info, function(x){
    data.table(date = readLines(x)[grepl(pattern = "Date", readLines(x))] ,    #read the info txt from imageJ using data.table, reading lines uisng read.line,and only                                                                                  take the line with date.
               
               experiment = str_split(x, "_", simplify = TRUE)[,6],            #extract the information of the experiment,field of view, timepoint and channel from                                                                                   the name of the file. 
               field = str_split(x, "_", simplify = TRUE)[,9],
               timepoint = str_split(x, "_", simplify = TRUE)[,10],
               channel = str_split(x, "_", simplify = TRUE)[,11])
  }) %>% 
    bind_rows() %>% 
    filter(channel == "C2.txt") %>% 
    
    mutate(time = str_split(date , " ", simplify = TRUE)[,3],                  #using the date column, split the date line                                                                                 by a space and get the time stamp of when                                                                                  the image was taken name the new column as                                                                                 time.  
           hours = str_split(time, ":", simplify = TRUE)[,1],                  #breakdown the timestamp into hour, mins and secs. 
           mins = str_split(time, ":", simplify = TRUE)[,2],                   #min
           sec =  str_split(time, ":", simplify = TRUE)[,3],                   #sec
           timepoint = str_sub(timepoint, start = 2L),                           
           channel = str_remove(channel, pattern = ".txt")) %>%                  
    mutate(tot.time.sec = as.numeric(hours)*60*60 + as.numeric(mins)*60 + as.numeric(sec )) %>%  #converting the entire time stamp into seconds and name that col. as tot.time.sec
    group_by(experiment, field) %>% 
    arrange(.,as.numeric(timepoint), .by_group = TRUE)                                            #reaarange the entire table by timepoint. 
  
  list.time.info <- image.info %>% 
    mutate(exp.field = paste0(experiment,"_",field )) %>%                   #Here you're creating a new name for each field of view to reflect which experiment that field of view belong to. 
    select(., experiment,
           field,
           exp.field,
           timepoint, 
           time,
           hours,
           mins,
           sec,
           tot.time.sec,
           channel) %>% 
    pivot_wider(names_from = channel , values_from = 5:9) %>%          #you're going to make the df wide from long, where the times of (c1=dic , c2=gfp, c3=mcherry) diff channels are side by                                                                        side, and name the new col. of the wider df as tot.time.sec_C1 , tot.time.sec_C2, tot.time.sec_C3. 
    arrange(.,as.numeric(timepoint)) %>%
    split(.$exp.field)
  
  real.time.interval.df<- lapply(list.time.info, function(x){ 
    x %>% 
      mutate(.,timepoint = as.numeric(timepoint),
             #get the difference between the time using diff function in data.table, and add 0 to the first row since t0 = 0. "diff" gives the iterated differences between rows in a column
             time.dif.c2 = c(0 ,diff(.$tot.time.sec_C2))) %>% 
      mutate(.,         #creating a new column where the real timepoint for each image is created starting from 0 to total time of timelapse by                                                                       adding the (time.dif.C1)n + (time.dif.C1)n-1 using diffinv function. "diffinv" is the inverse of "diff"
             real.time.c2 = diffinv(.$time.dif.c2)[2:(nrow(x)+1)])
  }) %>% 
    bind_rows()
  
  return(real.time.interval.df)
} #this is for the pup1-rfp experiments where there is no timelapse in the red channel 

#----------------------------------------------------------------------

#------------------------

#Function 2.1 : get.imaris.info()

#creating a df with all the area, time, trackID, ID, intensity (mean, median, sum, center) of gfp and mcherry, channel information, volume, no. of voxels of all the cells in all the samples using data from Imaris. 

##Function 2.2.1 : get.imaris.info.onlyGFP()  
get.imaris.info.onlyGFP <- function(dir.names , real.time.interval.df, files_to_read){
  
  #local function to be used only within this function
  readingImarisFiles <- function(files , dir_name){
    
    listImarisFiles <- list.files(dir_name, full.names = TRUE) #for one sample as of now
    
    ImarisFiles <- lapply(files, function(x){
      listImarisFiles %>%
        as_tibble() %>%
        filter(grepl(x, value))
    }) %>% bind_rows()
    
    return(ImarisFiles)
  }
  
  #A list, where each df of the list= directory corresponding to each sample and rows for each df = full file path to each file from imaris
  list_fullpaths <- lapply(dir.names, function(x){
    readingImarisFiles(dir_name = x, files = FilesToRead)
  })
  
  #this will read the csv 
  list.of.files <- lapply(list_fullpaths, function(x){
    
    depth <- str_split(x, "/", simplify = T) %>% 
      length() %>% as.numeric()
    
    lapply(x[[1]], function(y){
      read.csv((y), header = TRUE, skip = 3) %>% 
        select(., -contains(c("Category" ,"Image")))
    }) %>% bind_cols(.name_repair = "universal") %>% 
      select(.,Area,
             Time...3,
             TrackID...4,
             #BoundingBoxOO.Length.B,
            # BoundingBoxOO.Length.C,
             Ellipsoid.Axis.Length.B,
             Ellipsoid.Axis.Length.C,
             Intensity.Center,
             Intensity.Mean,
             Intensity.Median,
             Intensity.Sum,
             Number.of.Voxels,
             Number.of.Triangles,
             Sphericity,
             Volume,
             Position.X,
             Position.Y) %>% 
      mutate(.,experiment = str_split(str_split(x, "/" , simplify = TRUE)[,depth], "_", simplify = TRUE)[,3],
             sample = str_split(str_split(x, "/" , simplify = TRUE)[,depth], "_", simplify = TRUE)[,6])  %>% 
      unite(.,"trackID", c("TrackID...4", "experiment","sample") , remove = FALSE)  
  }) %>% bind_rows() %>% 
    split(.$experiment)
  
  #renaming the columns 
  list.of.files <-lapply(list.of.files , function(x){
    names(x) <- c("area" ,
                  "timepoint",
                  "unique.trackID" ,
                  "trackID",
                  #"BB_B",
                  #"BB_C",
                  "Elip_B",
                  "Elip_C",
                  "gfp.intensity.center",
                  "gfp.int.mean",
                  "gfp.int.median",
                  "gfp.int.sum" , 
                  "no.of.voxels",
                  "no.of.triangles",
                  "spheracity",
                  "volume",
                  "pos.x",
                  "pos.y",
                  "experiment", 
                  "field")
    return(x)
  }) %>% 
    bind_rows()
  
  #add the info of the real time interval to the df of intensities and cell properties. 
  list.of.files <- real.time.interval.df %>% 
    select(experiment, 
           field,
           exp.field,
           timepoint,
           time.dif.c2,
           real.time.c2) %>% 
    left_join(list.of.files ,., by = c("field", "experiment","timepoint")) %>%             #left_joining the real time interval df with the list of files df to add the time interval and real                                                                                            time to the experiment, field of view and the fake timepoint. 
    rename(.,
            "time.dif.gfp"="time.dif.c2" ,
           "real.time.gfp" = "real.time.c2") %>% 
    split(.$experiment)
}

#----------------------------------------------------------------------

#function 2.3.a get.imaris.info.dapi_pup1()
get.imaris.info.dapi_pup1 <- function(dir.names , files_to_read){
  
  #local function to be used only within this function
  readingImarisFiles <- function(files , dir_name){
    
    listImarisFiles <- list.files(dir_name, full.names = TRUE) #for one sample as of now
    
    ImarisFiles <- lapply(files, function(x){
      listImarisFiles %>%
        as_tibble() %>%
        filter(grepl(x, value))
    }) %>% bind_rows()
    
    return(ImarisFiles)
  }
  
  #A list, where each df of the list= directory corresponding to each sample and rows for each df = full file path to each file from imaris
  list_fullpaths <- lapply(dir.names, function(x){
    readingImarisFiles(dir_name = x, files = FilesToReadDAPIRFP)
  })
  
  #this will read the csv 
  list.of.files <- lapply(list_fullpaths, function(x){
    
    depth <- str_split(x, "/", simplify = T) %>% 
      length() %>% as.numeric()
    
    lapply(x[[1]], function(y){
      read.csv((y), header = TRUE, skip = 3) %>% 
        select(., -contains(c("Category" ,"Image")))
    }) %>% 
      bind_cols(.name_repair = "universal") %>% 
      select(.,-contains(c("X..", "Unit"))) %>% 
      select(.,contains("Intensity"),
             Area,
             Time...3,
             TrackID...4,
             BoundingBoxOO.Length.B,
             BoundingBoxOO.Length.C,
             Ellipsoid.Axis.Length.B,
             Ellipsoid.Axis.Length.C,
             #Intensity.Center,
             #Intensity.Mean,
             #Intensity.Median,
             #Intensity.Sum,
             Number.of.Voxels,
             Number.of.Triangles,
             Sphericity,
             Volume,
             Position.X,
             Position.Y) %>% 
      mutate(.,experiment = str_split(str_split(x, "/" , simplify = TRUE)[,depth], "_", simplify = TRUE)[,3],
             sample = str_split(str_split(x, "/" , simplify = TRUE)[,depth], "_", simplify = TRUE)[,6])  %>% 
      unite(.,"trackID", c("TrackID...4", "experiment","sample") , remove = FALSE)  
  }) %>% bind_rows() %>% 
    split(.$experiment)
  
  #renaming the columns 
  list.of.files <-lapply(list.of.files , function(x){
    names(x) <- c( "rfp.int.center",
                   "rfp.int.mean",
                   "rfp.int.median",
                   "rfp.int.sum",
                   "dapi.int.center",
                   "dapi.int.mean",
                   "dapi.int.median",
                   "dapi.int.sum",
                   "area" ,
                   "timepoint",
                   "unique.trackID" ,
                   "trackID",
                   "BB_B",
                   "BB_C",
                   "Elip_B",
                   "Elip_C",
                   "no.of.voxels",
                   "no.of.triangles",
                   "spheracity",
                   "volume",
                   "pos.x",
                   "pos.y",
                   "experiment", 
                   "field")
    return(x)
  }) %>% 
    bind_rows()
  
  list.of.files.dapi.pup1 <- list.of.files %>% 
    mutate(timepoint = ifelse(timepoint == 1,1,31)) 
  
  return(list.of.files.dapi.pup1)
}

##########################################--9.1.21---###########################################
#Problem: had to find a way to use the RFP intensity from the localized expression of pup1-rfp. 
#Used RFP channel to threshold on, and only the selection the localized spots of expression. Now we have another mask of rfp surface. One way to identify the rfp surface with the cell's surface is by making a common mask which will now be a new channel. The max intensity of this channel is the same for a given cell's surface and the rfp surface inside that cell.

#Below is a modified function of the get.imaris.info.dapi_pup1() function. The new function involves the information of the new mask's channel's max intensity and intensity center. 

get.imaris.info.dapi_pup1.surface <- function(dir.names , files_to_read){
  
  #local function to be used only within this function
  readingImarisFiles <- function(files , dir_name){
    
    listImarisFiles <- list.files(dir_name, full.names = TRUE) #for one sample as of now
    
    ImarisFiles <- lapply(files, function(x){
      listImarisFiles %>%
        as_tibble() %>%
        filter(grepl(x, value))
    }) %>% bind_rows()
    
    return(ImarisFiles)
  }
  
  #A list, where each df of the list= directory corresponding to each sample and rows for each df = full file path to each file from imaris
  list_fullpaths <- lapply(dir.names, function(x){
    readingImarisFiles(dir_name = x, files = FilesToReadDAPIRFP)
  })
  
  #this will read the csv 
  list.of.files <- lapply(list_fullpaths, function(x){
    
    depth <- str_split(x,"/",simplify = T) %>% 
      length() %>% as.numeric()
    
    lapply(x[[1]], function(y){
      read.csv((y), header = TRUE, skip = 3) %>% 
        select(., -contains(c("Category" ,"Image")))
    }) %>% 
      bind_cols(.name_repair = "universal") %>% 
      select(.,-contains(c("X..", "Unit"))) %>% 
      select(.,contains("Intensity"),
             Area,
             Time...3,
             #TrackID...4, dont need trackIDs for the pup1 and cells surface data
             BoundingBoxOO.Length.B,
             BoundingBoxOO.Length.C,
             Ellipsoid.Axis.Length.B,
             Ellipsoid.Axis.Length.C,
             #Intensity.Center,
             #Intensity.Mean,
             #Intensity.Median,
             #Intensity.Sum,
             Number.of.Voxels,
             Number.of.Triangles,
             Sphericity,
             Volume,
             Position.X,
             Position.Y) %>% 
      mutate(.,experiment = str_split(str_split(x, "/" , simplify = TRUE)[,depth], "_", simplify = TRUE)[,3],
             sample = str_split(str_split(x, "/" , simplify = TRUE)[,depth], "_", simplify = TRUE)[,6]) 
      # unite(.,"trackID", c("TrackID...4", "experiment","sample") , remove = FALSE)  
  }) %>% bind_rows() %>% 
    split(.$experiment)
  
  #renaming the columns 
  list.of.files <-lapply(list.of.files , function(x){
    names(x) <- c( "rfp.int.center",
                   "rfp.int.mean",
                   "rfp.int.median",
                   "rfp.int.sum",
                   "dapi.int.center",
                   "dapi.int.mean",
                   "dapi.int.median",
                   "dapi.int.sum",
                   "all.mask.int.max",
                   "all.mask.int.center",
                   "area" ,
                   "timepoint",
                   # "unique.trackID" ,
                   # "trackID",
                   "BB_B",
                   "BB_C",
                   "Elip_B",
                   "Elip_C",
                   "no.of.voxels",
                   "no.of.triangles",
                   "spheracity",
                   "volume",
                   "pos.x",
                   "pos.y",
                   "experiment", 
                   "field")
    return(x)
  }) %>% 
    bind_rows()
  
  # list.of.files.dapi.pup1 <- list.of.files %>% 
  #   mutate(timepoint = ifelse(timepoint == 1,1,31)) 
  
  return(list.of.files)
}
##########################################--9.1.21---###########################################

#-------Background intensity calculation---------------------------
##Function 3: bg.intensity()

#This function is to get the background image intensity for GFP and mcherry images. third step in the pipeline of analysis.

bg.intensity <- function(bg.files){
  #to get the files with background information for gfp and mcherry
  #getting a df of the background intensities of gfp and mcherry 
  bg.means <- lapply(bg.files, read.csv) %>% 
    bind_rows() %>% 
    select(Label,
           Area,
           Mean,
           StdDev,
           Min,
           Max) %>% 
    mutate(.,channel = str_split(str_split(Label, "_", simplify = TRUE)[,8], ".TIF", simplify = TRUE)[,1] , 
           channel.name = ifelse(channel == "C2", "gfp","mcherry"),
           experiment = str_split(Label, "_", simplify = TRUE)[,3],
           field = str_split(Label, "_", simplify = TRUE)[,6],
           timepoint = as.numeric(str_sub(str_split(Label, "_", simplify = TRUE)[,7] , start = 2))) %>% 
    select(experiment,
           field,
           timepoint,
           Mean,
           channel.name) %>% 
    pivot_wider( names_from = channel.name, 
                 values_from = Mean) %>% 
    rename("avg.gfp.bg" = "gfp",
           "avg.mcherry.bg" = "mcherry") %>% 
    split(.$experiment)
}

bg.intensity.rfp.dapi <- function(bg.files){
  #to get the files with background information for gfp and mcherry
  #getting a df of the background intensities of gfp and mcherry 
  bg.means <- lapply(bg.files, read.csv) %>% 
    bind_rows() %>% 
    select(Label,
           Area,
           Mean,
           StdDev,
           Min,
           Max) %>% 
    mutate(.,channel = str_split(str_split(Label, "_", simplify = TRUE)[,8], ".TIF", simplify = TRUE)[,1] , 
           channel.name = ifelse(channel == "C3", "rfp","dapi"),
           experiment = str_split(Label, "_", simplify = TRUE)[,3],
           field = str_split(Label, "_", simplify = TRUE)[,6],
           timepoint = as.numeric(str_sub(str_split(Label, "_", simplify = TRUE)[,7] , start = 2))) %>% 
    select(experiment,
           field,
           timepoint,
           Mean,
           channel.name) %>% 
    pivot_wider( names_from = channel.name, 
                 values_from = Mean) %>% 
    rename("avg.rfp.bg" = "rfp",
           "avg.dapi.bg" = "dapi") %>% 
    split(.$experiment)
} #this is to get the bg int of pup1-rfp and nuc-blue dapi images 

bg.intensity.dapi <- function(bg.files){
  #to get the files with background information for gfp and mcherry
  #getting a df of the background intensities of gfp and mcherry 
  bg.means <- lapply(bg.files, read.csv) %>% 
    bind_rows() %>% 
    select(Label,
           Area,
           Mean,
           StdDev,
           Min,
           Max) %>% 
    mutate(.,channel = str_split(str_split(Label, "_", simplify = TRUE)[,8], ".TIF", simplify = TRUE)[,1] , 
           channel.name = "dapi",
           experiment = str_split(Label, "_", simplify = TRUE)[,3],
           field = str_split(Label, "_", simplify = TRUE)[,6],
           timepoint = as.numeric(str_sub(str_split(Label, "_", simplify = TRUE)[,7] , start = 2))) %>% 
    select(experiment,
           field,
           timepoint,
           Mean,
           channel.name) %>% 
    pivot_wider( names_from = channel.name, 
                 values_from = Mean) %>% 
    rename("avg.dapi.bg" = "dapi") %>% 
    split(.$experiment)
} #this is just for dapi files 

bg.intensity.gfp <- function(bg.files){
  #to get the files with background information for gfp and mcherry
  #getting a df of the background intensities of gfp and mcherry 
  bg.means <- lapply(bg.files, read.csv) %>% 
    bind_rows() %>% 
    select(Label,
           Area,
           Mean,
           StdDev,
           Min,
           Max) %>% 
    mutate(.,channel = str_split(str_split(Label, "_", simplify = TRUE)[,8], ".TIF", simplify = TRUE)[,1] , 
           channel.name = "gfp",
           experiment = str_split(Label, "_", simplify = TRUE)[,3],
           field = str_split(Label, "_", simplify = TRUE)[,6],
           timepoint = as.numeric(str_sub(str_split(Label, "_", simplify = TRUE)[,7] , start = 2))) %>% 
    select(experiment,
           field,
           timepoint,
           Mean,
           Min,
           channel.name) %>% 
    pivot_wider( names_from = channel.name, 
                 values_from = c(Mean, Min)) %>% 
    # rename("avg.gfp.bg" = "gfp") %>% 
    split(.$experiment)
} #this is just for gfp 
#----------------------------------------------------------------------

#Subtract background intensity ---------------------------------------------------------------------
#Function 4: sub.bg.intensity()
##Use this function to subtract the background intensities from the mean gfp and mcherry intensities. 

sub.bg.intensity <- function(list.of.files, bg.means){
  #subtracting the background gfp and mcherry from signal
  list.of.files.bg.sub <- left_join(bind_rows(list.of.files) ,                                        
                                    bind_rows(bg.means), by = c("timepoint", "experiment", "field")) %>%  #adding the average background gfp and mcherry intensity to the df of cells info wrt each                                                                                                       time point, experiment, and sample 
    mutate(.,  gfp.mean.bg.sub = gfp.int.mean - avg.gfp.bg,                               #subtracting the overall average gfp background intensity from the cell's gfp mean intensity
           mcherry.mean.bg.sub = mcherry.int.mean - avg.mcherry.bg,                   #subtracting the overall average mcherry background intensity from the cell's mcherry mean intensity
           gfp.sum.bg.sub = gfp.int.sum - (no.of.voxels*avg.gfp.bg) ,                 #subtracting the total background gfp intensity (multiplying the cell's no.of voxels to the avg.                                                                                                                  background intensity) from the cell's total gfp mean intensity. 
           mcherry.sum.bg.sub = mcherry.int.sum - (no.of.voxels*avg.mcherry.bg)) %>%  #subtracting the total background mcherry intensity (multiplying the cell's no.of voxels to the avg.                                                                                                              background intensity) from the cell's total mcherry mean intensity. 
    split(.$experiment)
}

sub.bg.intensity.dapi <- function(list.of.files, bg.means){
  #subtracting the background gfp and mcherry from signal
  list.of.files.bg.sub <- left_join(bind_rows(list.of.files) ,                                        
                                    bind_rows(bg.means), by = c("timepoint", "experiment", "field")) %>%  #adding the average background gfp and mcherry intensity to the df of cells info wrt each                                                                                                       time point, experiment, and sample 
    mutate(.,  dapi.mean.bg.sub = dapi.int.mean - avg.dapi.bg,                 #subtracting the overall average mcherry background intensity from the cell's mcherry mean intensity
           dapi.sum.bg.sub = dapi.int.sum - (no.of.voxels*avg.dapi.bg)) %>%  #subtracting the total background mcherry intensity (multiplying the cell's no.of voxels to the avg.                                                                                                              background intensity) from the cell's total mcherry mean intensity. 
    split(.$experiment)
}

sub.bg.intensity.gfp <- function(list.of.files, bg.means){
  #subtracting the background gfp and mcherry from signal
  list.of.files.bg.sub <- left_join(bind_rows(list.of.files) ,                                        
                                    bind_rows(bg.means), by = c("timepoint", "experiment", "field")) %>%  #adding the average background gfp and mcherry intensity to the df of cells info wrt each                                                                                                       time point, experiment, and sample 
    mutate(.,  gfp.mean.bg.sub = gfp.int.mean - avg.gfp.bg,                               #subtracting the overall average gfp background intensity from the cell's gfp mean intensity
           gfp.sum.bg.sub = gfp.int.sum - (no.of.voxels*avg.gfp.bg)) %>%  
    split(.$experiment)
}

sub.bg.intensity.dapi.rfp <- function(list.of.files, bg.means){
  #subtracting the background gfp and mcherry from signal
  list.of.files.bg.sub <- left_join(bind_rows(list.of.files) ,                                        
                                    bind_rows(bg.means), by = c("timepoint", "experiment", "field")) %>%  #adding the average background dapi and mcherry intensity to the df of cells info wrt each                                                                                                       time point, experiment, and sample 
    mutate(.,  dapi.mean.bg.sub = dapi.int.mean - avg.dapi.bg , #subtracting the overall average mcherry background intensity from the cell's mcherry mean intensity
           dapi.mean.bg.sub.puncta = dapi.mean.puncta - avg.dapi.bg , 
           dapi.sum.bg.sub = dapi.int.sum - (no.of.voxels*avg.dapi.bg) ,
           dapi.sum.bg.sub.puncta = dapi.sum.puncta - (no.of.voxels.puncta*avg.dapi.bg),
           
           rfp.mean.bg.sub = rfp.int.mean - avg.rfp.bg , 
           rfp.sum.bg.sub = rfp.int.sum - (no.of.voxels*avg.rfp.bg),
           rfp.mean.bg.sub.puncta = rfp.mean.puncta - avg.rfp.bg,
           rfp.sum.bg.sub.puncta = rfp.sum.puncta - (no.of.voxels.puncta*avg.rfp.bg)
           ) %>%  #subtracting the total background mcherry intensity (multiplying the cell's no.of voxels to the avg.                                                                                                              background intensity) from the cell's total mcherry mean intensity. 
    split(.$experiment)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 5: trackID.abv.zero()

trackID.abv.zero <- function(list.of.files.bg.sub){
  #getting trackID of cells with more than background intensity at the first time point
  intensity.above.zero.trackID <- lapply(list.of.files.bg.sub, function(x){
    x %>% 
      filter(timepoint == 1) %>%                                       #For cells in the first image or at t=0
      filter(gfp.mean.bg.sub >0 & mcherry.mean.bg.sub >0) %>%     #filtering the cells with higher mean gfp and mcherry intensities than the average background gfp and mcherry intensities
      filter(gfp.sum.bg.sub >0 & mcherry.sum.bg.sub >0) %>%       #filtering the cells with higher sum (total) gfp and mcherry intensities than the sum of the background gfp and mcherry                                                                         intensities
      filter(!is.na(trackID)) %>%                                 #filtering out cells which were not identified in the first image (i.e at timepoint = 0)
      pull(unique.trackID) %>%                                    #getting the trackIDs of the cells which pass all the above filtering
      as.data.frame() 
  }) %>% 
    bind_rows(., .id = "names")
  names(intensity.above.zero.trackID) <- c("experiment", "unique.trackID")
  
  #making a new df of cells with gfp and mcherry intensities above the background intensity. 
  list.of.files.bg.sub.above.0 <- bind_rows(list.of.files.bg.sub) %>% 
    filter(unique.trackID %in% intensity.above.zero.trackID$unique.trackID)
}

#removed the mcherrry filtering line here
trackID.abv.zero.gfp.rfp.dapi <- function(bg_subtracted_dftp1,list_gfp_all_tps ){
  #getting trackID of cells with more than background intensity at the first time point
  intensity.above.zero.trackID <- lapply(bg_subtracted_dftp1, function(x){
    x %>% 
      filter(timepoint == 1) %>%                                       #For cells in the first image or at t=0
      filter(gfp.mean.bg.sub >0 ) %>%     
      filter(gfp.sum.bg.sub >0 ) %>% 
      filter(rfp.mean.bg.sub >0 ) %>%  
      # filter(dapi.mean.bg.sub >0) %>%  removing this filter becausee of poor staining in MG132 cells with DAPI
      filter(rfp.sum.bg.sub >0) %>%  
      # filter(dapi.sum.bg.sub >0) %>%        
      filter(!is.na(trackID)) %>%                                
      pull(unique.trackID) %>%                                    
      as.data.frame() 
  }) %>% 
    bind_rows(., .id = "names")
  
  names(intensity.above.zero.trackID) <- c("experiment", "unique.trackID")
  
  #making a new df of cells with gfp and mcherry intensities above the background intensity. 
  list.of.files.bg.sub.above.0 <- bind_rows(list_gfp_all_tps) %>% 
    filter(unique.trackID %in% intensity.above.zero.trackID$unique.trackID)
  
  return(list(intensity.above.zero.trackID, list.of.files.bg.sub.above.0))
}

#filtering cells with less than 0 rfp and dapi internsity
trackID.abv.zero.rfp.dapi <- function(list.of.files.bg.sub){
  #getting trackID of cells with more than background intensity at the first time point
  intensity.above.zero.trackID <- lapply(list.of.files.bg.sub, function(x){
    x %>% 
      filter(timepoint == 1) %>%                                       
      filter(rfp.mean.bg.sub >0 ) %>%  
      filter(dapi.mean.bg.sub >0) %>% 
      filter(rfp.sum.bg.sub >0) %>%  
      filter(dapi.sum.bg.sub >0) %>%                                                                     
      filter(!is.na(trackID)) %>%                                 
      pull(unique.trackID) %>%                                    
      as.data.frame() 
  }) %>% 
    bind_rows(., .id = "names")
  names(intensity.above.zero.trackID) <- c("experiment", "unique.trackID")
  
  #making a new df of cells with gfp and mcherry intensities above the background intensity. 
  list.of.files.bg.sub.above.0 <- bind_rows(list.of.files.bg.sub) %>% 
    filter(unique.trackID %in% intensity.above.zero.trackID$unique.trackID)
}
#----------------------------------------------------------------------

#----------Get a GFP autofluor threshold----------------------------------
#Function 6.b: gfp.threshold()
#This function filters out cells based on a threshold based on the first timepoint 
#old function, dont use it anymore
gfp.thershold <- function(list.of.files.bg.sub.above.0){
  #split the df into the experiments so we can decide the gfp threshold for each esperiment. 
  temp.list <- list.of.files.bg.sub.above.0 %>% 
    split(.$experiment)
  
  #graphing the density plots of the mrg cells and the gfp cells so we can use data from this plot to find the point where the neg and pos cells overlap at the first timepoint. 
  neg.pos.density.plots <- lapply(temp.list, function(x){
    x %>% 
      filter(timepoint == 1) %>% 
      select(sample,
             gfp.mean.bg.sub) %>% 
      ggdensity(.,x = "gfp.mean.bg.sub" , 
                color = "sample", 
                y = "..scaled..",
                ggtheme = theme_pubr())+
      scale_x_log10()
  })
  
  #use the y values from the density plot data to find where the neg and pos cells overlap 
  threshold <- lapply(neg.pos.density.plots, function(a){
    ggplot_build(a)$data[[1]] %>% 
      select(colour, y,x) %>%
      pivot_wider(names_from = colour, values_from = y) %>%       #this line is extracting the x, y and the colors of each graph in the density plot. Neg and pos samples are colored diff. 
      filter(x >log10(50), x <3) %>%                              #we're interested in the region between 50 and 1000 where the true signal lies. Note: log10(1000) = 3
      mutate(diff = abs(`#F8766D` - `#00BFC4`),                   #subtracting the y values from the density plot because the point where the curves overlap will have a value closer to 0.
             real.x = 10^x) %>%                                 #subtracting the y values from the density plot because the point where the curves overlap will have a value closer to 0.
      arrange(diff) %>%
      pull(x) %>% .[1]
  }) %>% bind_rows( ) %>% 
    pivot_longer(cols = 1:3, names_to = c("experiment")) %>% 
    mutate(.,gfp.threshold = 10^value)
  
  return(threshold)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Thresholding based on 1. Hoechst intensity to remove dead cells. 1. based on autoflourescence in GFP 2. and in mcherry 
#This function filters out cells based on a threshold based on every timepoint 
#function 6.a : dapi.threshold()
dapi.threshold <- function(df_for_threshold , lower_lim, uppr_lim){
  df_for_threshold <- bind_rows(df_for_threshold) %>% split(.$exp.field)
  dna.dist <- lapply(df_for_threshold, function(x){ #this df needs to be a list which is split by experiment
    x %>% 
      filter(timepoint == 1) %>% 
      ggplot(.,aes(dapi.mean.bg.sub))+
      geom_density(aes(y = ..scaled..))+
      scale_x_log10()
  })
  
  dapiIntCutOff <- lapply(dna.dist, function(a){
    ggplot_build(a)$data %>% 
      as.data.frame() %>% 
      select(y,x) %>% 
      mutate(new.x = 10^x) %>% 
      filter(new.x >lower_lim & new.x < uppr_lim) %>% 
      arrange(y) %>% 
      pull(new.x) %>% 
      .[1]
  }) %>% 
    bind_rows() %>% 
    pivot_longer(cols = 1:ncol(.)) %>% 
    rename("experiment" = "name", "dapi.threshold" = "value")
  
  return(dapiIntCutOff)
} #dapi thresholding for t=0

#function 6.b.1 : mcherry.threshold.31tp()
mcherry.thershold.31tp <- function(list.of.files.bg.sub.above.0){
  #This is getting threshold of mcherry intensities for the 20min experiment  
  temp.20 <- list.of.files.bg.sub.above.0 %>% 
    filter(experiment == "20min") %>%         #for each experimet 
    split(.$timepoint)                        #This is done because we want the overlap of autoflour and positive signal for each timepoint 
  
  mcherry.dens.20 <- lapply(temp.20,function(x){   #this will create a density plot of mcherry background subtracted intensities for each timepoint and color them based on if they're coming from Autoflour or positive samples
    x %>%
      select(sample,
             mcherry.mean.bg.sub) %>%
      ggdensity(.,x = "mcherry.mean.bg.sub" ,
                color = "sample",
                y = "..scaled..",
                ggtheme = theme_pubr())+
      scale_x_log10()
  })
  
  mcherry.threshold.20 <- lapply(mcherry.dens.20, function(a){
    ggplot_build(a)$data[[1]]%>%
      select(colour, y,x) %>%
      filter(y >0 , y < 1) %>% 
      pivot_wider(names_from = colour, values_from = y) %>%
      filter(x >log10(10), x < log10(100)) %>%
      mutate(diff = abs(`#F8766D` - `#00BFC4`),
             real.x = 10^x) %>%
      arrange(diff) %>%
      pull(x) %>% .[1]
  }) %>% bind_rows( ) %>%
    pivot_longer(cols = 1:31, names_to = c("timepoint")) %>%
    mutate(.,mcherry.threshold = 10^value)
  
  #This is getting threshold of mcherry intensities for the 40min experiment 
  temp.40 <- list.of.files.bg.sub.above.0 %>% 
    filter(experiment == "40min") %>% 
    split(.$timepoint)
  
  mcherry.dens.40 <- lapply(temp.40,function(x){
    x %>%
      select(sample,
             mcherry.mean.bg.sub) %>%
      ggdensity(.,x = "mcherry.mean.bg.sub" ,
                color = "sample",
                y = "..scaled..",
                ggtheme = theme_pubr())+
      scale_x_log10()
  })
  
  mcherry.threshold.40 <- lapply(mcherry.dens.40, function(a){
    ggplot_build(a)$data[[1]]%>%
      select(colour, y,x) %>%
      filter(y >0 , y < 1) %>% 
      pivot_wider(names_from = colour, values_from = y) %>%
      filter(x >log10(10), x < log10(100)) %>%
      mutate(diff = abs(`#F8766D` - `#00BFC4`),
             real.x = 10^x) %>%
      arrange(diff) %>%
      pull(x) %>% .[1]
  }) %>% bind_rows( ) %>%
    pivot_longer(cols = 1:31, names_to = c("timepoint")) %>%
    mutate(.,mcherry.threshold = 10^value)
  
  #This is getting threshold of mcherry intensities for the 60min experiment 
  temp.60 <- list.of.files.bg.sub.above.0 %>% 
    filter(experiment == "60min") %>% 
    split(.$timepoint)
  
  mcherry.dens.60 <- lapply(temp.60,function(x){
    x %>%
      select(sample,
             mcherry.mean.bg.sub) %>%
      ggdensity(.,x = "mcherry.mean.bg.sub" ,
                color = "sample",
                y = "..scaled..",
                ggtheme = theme_pubr())+
      scale_x_log10()
  })
  
  mcherry.threshold.60 <- lapply(mcherry.dens.60, function(a){
    ggplot_build(a)$data[[1]]%>%
      select(colour, y,x) %>%
      filter(y >0 , y < 1) %>% 
      pivot_wider(names_from = colour, values_from = y) %>%
      filter(x >log10(10), x < log10(100)) %>%
      mutate(diff = abs(`#F8766D` - `#00BFC4`),
             real.x = 10^x) %>%
      arrange(diff) %>%
      pull(x) %>% .[1]
  }) %>% bind_rows( ) %>%
    pivot_longer(cols = 1:31, names_to = c("timepoint")) %>%
    mutate(.,mcherry.threshold = 10^value)
  
  mcherry.threshold.all <- list(mcherry.threshold.20,
                        mcherry.threshold.40,
                        mcherry.threshold.60)
  
  names(mcherry.threshold.all) <- c("20min", "40min", "60min")
  
  mcherry.threshold.all <- mcherry.threshold.all %>% bind_rows(.,.id = "experiment") 
  
  mcherry.threshold.all$timepoint <- as.numeric(mcherry.threshold.all$timepoint)
  
  
  
  mcherry.autofluor_threshold <- list.of.files.bg.sub.above.0 %>% 
    filter(sample == "neg.signal") %>% 
    group_by(timepoint, experiment) %>%
    summarise(t80 = quantile(mcherry.mean.bg.sub, 0.8) , 
              t95 = quantile(mcherry.mean.bg.sub, 0.95)) %>% 
    left_join(mcherry.threshold.all , ., by = c("experiment", "timepoint")) %>% 
    mutate(threshold_80 = ifelse(mcherry.threshold>t80, mcherry.threshold, t80) , 
           threshold_95 = ifelse(mcherry.threshold>t95, mcherry.threshold, t95))
  
  return(mcherry.autofluor_threshold)
}

rfp.thershold.31tp.temp <- function(list.of.files.bg.sub.above.0 ){
  
  temp.list <-  list.of.files.bg.sub.above.0 %>% split(.$experiment)
  timepoint <- c(1)
  temp.plots <- list()
  
  for (i in timepoint){
    temp.plots[[i]] <- lapply(temp.list, function(df){
      a <- df$experiment[1]
      # print(a)
      df %>%
        filter(timepoint == i) %>%
        select(.,sample, rfp.mean.bg.sub) %>%
        ggplot(.,aes(x = rfp.mean.bg.sub , color = sample))+
        geom_density(aes(y = ..scaled..))+
        scale_x_log10()
    })
  }
  
  temp.plots <- temp.plots %>% discard(is.null)
  
  
  rfp.af.threshold <- lapply(temp.plots,function(a){
    lapply(a,function(b){
      ggplot_build(b)$data[[1]]%>%
        select(colour, y,x) %>%
        filter(y >0 , y < 1) %>% 
        pivot_wider(names_from = colour, values_from = y) %>%
        filter(x >log10(10), x < log10(100)) %>%
        mutate(diff = abs(`#F8766D` - `#00BFC4`),
               real.x = 10^x) %>%
        arrange(diff) %>%
        pull(real.x) %>% .[1]
    })
  }) %>% bind_rows() %>% 
    mutate(timepoint = c(1)) %>% 
    pivot_longer(cols = c("20min"), names_to = "experiment")
  
  
  rfp.autofluor_threshold <- list.of.files.bg.sub.above.0 %>% 
    filter(sample == "neg.signal") %>% 
    group_by(timepoint, experiment) %>%
    summarise(t80 = quantile(rfp.mean.bg.sub, 0.8) , 
              t95 = quantile(rfp.mean.bg.sub, 0.95)) %>% 
    left_join( rfp.af.threshold, ., by = c("experiment", "timepoint")) %>% 
    mutate(threshold_80 = ifelse(value>t80, value, t80) , 
           threshold_95 = ifelse(value>t95, value, t95))
  
  return(rfp.autofluor_threshold)
}

#Function 6.b.2 : gfp.threshold.31tp(). 
gfp.thershold.31tp <- function(list.of.files.bg.sub.above.0){
  
  timepoint.1 <- unique(list.of.files.bg.sub.above.0[[1]]$timepoint)
  temp.plots <- list()
  
  #this will plot a distribution plot of neg and pos gfp for every timepoint
  for (i in timepoint.1){
    temp.plots[[i]] <- lapply(list.of.files.bg.sub.above.0, function(df){  #input is a list of dfs where each list is df for exp. 
      df %>%
        filter(timepoint == i) %>%
        select(.,sample, gfp.mean.bg.sub) %>%
        ggplot(.,aes(x = gfp.mean.bg.sub , color = sample))+
        geom_density(aes(y = ..scaled..))+
        scale_x_log10()
    })
  }
  
  #remove any null elements from the list 
  temp.plots <- temp.plots %>% discard(is.null)
  
  #getting the overlap point of the neg and pos distribution: 
  gfp.af.threshold <- lapply(temp.plots,function(a){
    lapply(a,function(b){
      ggplot_build(b)$data[[1]]%>%
        select(colour, y,x) %>%
        filter(y >0.125 , y < 1) %>%   
        pivot_wider(names_from = colour, values_from = y) %>%
        filter(x >log10(30), x < log10(300)) %>%
        mutate(diff = abs(`#F8766D` - `#00BFC4`),
               real.x = 10^x) %>%
        arrange(diff) %>%
        pull(real.x) %>% .[1]
    })
  }) %>% bind_rows() %>% 
    mutate(timepoint = timepoint.1) %>% 
    pivot_longer(cols = c("20min"), names_to = "experiment")
  
  gfp.af.threshold$timepoint <- as.numeric(gfp.af.threshold$timepoint)
  
  #getting the 80th and 95th quantile points for the negative GFP cells 
  autofluor_threshold <- bind_rows(list.of.files.bg.sub.above.0) %>% 
    filter(sample == "neg.signal") %>% 
    group_by(timepoint, experiment) %>%
    summarise(t80 = quantile(gfp.mean.bg.sub, 0.8) , 
              t95 = quantile(gfp.mean.bg.sub, 0.95)) %>% 
    left_join(gfp.af.threshold , ., by = c("experiment", "timepoint")) %>% 
    mutate(threshold_80 = ifelse(value>t80, value, t80) , 
           threshold_95 = ifelse(value>t95, value, t95))
  
  return(autofluor_threshold)
}

#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 7: filter.cells.below.autofluor()

#filtering the cells :
#1. filter cells which are below the gfp threshold based on the autofluorescence for only t = 0 and then making subsequent timepoint intensities NA if less than threshold. (we don't use this anymore. use the function filter.cells.below.autofluor.31tp)
filter.cells.below.autofluor <- function(list.of.files.bg.sub.above.0 , min.intensity){
  
  #getting the trackID of the cells which have higher mcherrry mean intensity than the threshold set for each field of view and experiment. 
  unique.trackid.above.min.mcherry <- left_join(list.of.files.bg.sub.above.0  , min.intensity , by = "experiment") %>%
    filter(timepoint == 1) %>% 
    filter(mcherry.mean.bg.sub > mcherry.threshold) %>%
    pull(unique.trackID)
  
  #getting the trackID of the cells which have higher gfp mean intensity than the threshold set for each field of view and experiment. 
  unique.trackid.above.min.gfp <- left_join(list.of.files.bg.sub.above.0  , min.intensity , by = "experiment") %>%
    filter(timepoint == 1) %>% 
    filter(unique.trackID %in% unique.trackid.above.min.mcherry) %>% 
    filter(gfp.mean.bg.sub > gfp.threshold) %>%
    pull(unique.trackID)
  
  #creating a new df with cells expressing gfp above the threshold set above. 
  filtered.df <- left_join(list.of.files.bg.sub.above.0 , min.intensity , by = c("experiment")) %>%
    filter(unique.trackID %in% unique.trackid.above.min.gfp) %>% 
    split(.$experiment)
  
  #making a new df where the mean gfp intensity less than or equal to the median gfp intensity at the last timepoint is made NA to remove background noise 
  filtered.df.new <- lapply(filtered.df, function(x){
    x %>% 
      mutate(gfp.mean.bg.sub.new = ifelse(gfp.mean.bg.sub <= gfp.threshold, NA, gfp.mean.bg.sub))
  })
  
  cells.filtered.df <- list(filtered.df, 
                            filtered.df.new)  
  names(cells.filtered.df) <- c("filtered.df", "filtered.df.new")
  
  return(cells.filtered.df)                 #fiiltered.df.new is the df with NAs for cells with intensities < gfp.threshold. filtered.df is the df without cells wiith less that gfp.thresold at t=0. 
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 7.a: filter.cells.below.autofluor.31tp()
#filtering the cells :
#0. Filter out dead cells based on nuclear staining (Hoechst): 
FilterDeadCells <- function(dna.list, threshold , GfpMcherryAbv0.df){
  dapi.filtered.cells <- bind_rows(df.list) %>% 
    left_join(.,threshold , by = "experiment") %>% 
    filter(dapi.mean.bg.sub < dapi.threshold)
  
  ids.liveCells <- dapi.filtered.cells  %>% pull(unique.trackID)
  
  onlyLiveCells <- GfpMcherryAbv0.df %>% filter(unique.trackID %in% ids.liveCells)
  
  return(list(dapi.filtered.cells, 
              onlyLiveCells) %>% names("dapi.live.cells", "onlyLiveCells"))
}

#maybe remove the rfp negative cells? 
#1. filter cells which are below the gfp threshold based on the autofluorescence for t = 0, subtracting the autofluor threshold value for gfp and mcherry for every timepoint, and then making the autofluor subtracted intensity NA if the subtracted value is less than 1 
#this has gfp and mcherry
filter.cells.below.autofluor.31tp <- function(list.of.files.bg.sub.above.0 , min.intensity){
  
  #getting the trackID of the cells which have higher gfp mean intensity than the threshold set for each field of view and experiment.
  #first remove cells with mcherry intensity less than the set threshold (set an arbitrary threshold of 100 based on the overlap of autofluorescence and positive signal)
  #second remove cells with gfp intensity less than the set threshold 
  unique.trackid.above.min.gfp <- left_join(list.of.files.bg.sub.above.0  , min.intensity , by = c("experiment", "timepoint")) %>%
    filter(timepoint == 1) %>% 
    filter(mcherry.mean.bg.sub > mcherry.threshold) %>%
    filter(gfp.mean.bg.sub > gfp.threshold) %>%
    pull(unique.trackID)
  
  #creating a new df with cells expressing gfp above the threshold set above. 
  filtered.df <- left_join(list.of.files.bg.sub.above.0 , min.intensity , by = c("experiment", "timepoint")) %>%
    filter(unique.trackID %in% unique.trackid.above.min.gfp) %>% 
    split(.$experiment)
  
  #making a new df where the mean gfp and mcherry intensities are subtracted by the autofluorescence value at each time point 
  filtered.df.new <- lapply(filtered.df, function(x){
    x %>% 
      mutate(gfp.mean.bg.sub.new = gfp.mean.bg.sub - gfp.threshold,
             mcherry.mean.bg.sub.new = mcherry.mean.bg.sub - mcherry.threshold) %>%   #subtract the autofluorescence of the cells from the mean intensity of the cell
      mutate(gfp.mean.bg.sub.new = ifelse(gfp.mean.bg.sub.new <1 , NA, gfp.mean.bg.sub.new),
             mcherry.mean.bg.sub.new = ifelse(mcherry.mean.bg.sub.new <1 , NA, mcherry.mean.bg.sub.new)) 
  })
  
  cells.filtered.df <- list(filtered.df, 
                            filtered.df.new)  
  names(cells.filtered.df) <- c("filtered.df", "filtered.df.new")
  
  return(cells.filtered.df)                 #fiiltered.df.new is the df with NAs for cells with intensities < gfp.threshold. filtered.df is the df without cells wiith less that gfp.thresold at t=0. 
}

#only for GFP
filter.cells.below.autofluor.31tp.gfp <- function(list.of.files.bg.sub.above.0 , min.intensity){
  
  #getting the trackID of the cells which have higher gfp mean intensity than the threshold set for each field of view and experiment.
  #first remove cells with mcherry intensity less than the set threshold (set an arbitrary threshold of 100 based on the overlap of autofluorescence and positive signal)
  #second remove cells with gfp intensity less than the set threshold 
  unique.trackid.above.min.gfp <- left_join(list.of.files.bg.sub.above.0  , min.intensity , by = c("experiment", "timepoint")) %>%
    filter(timepoint == 1) %>% 
    filter(gfp.mean.bg.sub > gfp.threshold) %>%
    pull(unique.trackID)
  
  #creating a new df with cells expressing gfp above the threshold set above. 
  filtered.df <- left_join(list.of.files.bg.sub.above.0 , min.intensity , by = c("experiment", "timepoint")) %>%
    filter(unique.trackID %in% unique.trackid.above.min.gfp) %>% 
    split(.$experiment)
  
  #making a new df where the mean gfp and mcherry intensities are subtracted by the autofluorescence value at each time point 
  filtered.df.new <- lapply(filtered.df, function(x){
    x %>% 
      mutate(gfp.mean.bg.sub.new = gfp.mean.bg.sub - gfp.threshold) %>%   #subtract the autofluorescence of the cells from the mean intensity of the cell
      mutate(gfp.mean.bg.sub.new = ifelse(gfp.mean.bg.sub.new <1 , NA, gfp.mean.bg.sub.new)) 
  })
  
  cells.filtered.df <- list(filtered.df, 
                            filtered.df.new)  
  names(cells.filtered.df) <- c("filtered.df", "filtered.df.new")
  
  return(cells.filtered.df)                 #fiiltered.df.new is the df with NAs for cells with intensities < gfp.threshold. filtered.df is the df without cells wiith less that gfp.thresold at t=0. 
}

#this function filters out cells which do not have any pup1-rfp and then gets the cells which are above autofluor at tp=1 and then subtracts the autofluor value from cells at every time point and every experiment. 
#This is different from the above function in that is one subtracts the autofluor threshold from the gfp intensity sum of each cell. 
filter.cells.below.autofluor.31tp.gfpV2 <- function(df_rfp_dapi_gfp, rfp_min_int, gfp_live_cells_list , gfp_min_int){
  #first remove cells with pup1-rfp intensity less than the set threshold 
  #pup1 filtering 
  pup1.rfp_abvThreshold <- df_rfp_dapi_gfp %>% 
    left_join(.,rfp_min_int, by = c("timepoint", "experiment")) %>% 
    filter(timepoint == 1 & sample == "pos.signal") %>% 
    filter(rfp.mean.bg.sub > threshold_95) %>% pull(unique.trackID)
  
  #gfp filter 
  #getting the trackID of the cells which have higher gfp mean intensity than the threshold set for each field of view and experiment.
  #second remove cells with gfp intensity less than the set threshold at tp =1
  gfpCells_abvThreshold <- gfp_live_cells_list %>% 
    bind_rows() %>% 
    filter(unique.trackID %in% pup1.rfp_abvThreshold) %>% 
    left_join(.,gfp_min_int, by = c("timepoint", "experiment")) %>% 
    filter(timepoint == 1 & sample == "pos.signal") %>% 
    filter(gfp.mean.bg.sub > threshold) %>% 
    pull(unique.trackID)
  
  #creating a new df with cells expressing gfp above the threshold set above. 
  filtered.df <- left_join(bind_rows(gfp_live_cells_list) , 
                           gfp_min_int , by = c("experiment", "timepoint")) %>%
    filter(unique.trackID %in% gfpCells_abvThreshold) %>% 
    split(.$experiment)
  
  #making a new df where the mean gfp and mcherry intensities are subtracted by the autofluorescence value at each time point 
  filtered.df.new <- lapply(filtered.df, function(x){
    x %>% 
      mutate(gfp.mean.bg.af.sub = gfp.mean.bg.sub - threshold , 
             gfp.sum.bg.af.sub = gfp.sum.bg.sub - no.of.voxels*threshold) %>%
      filter(gfp.sum.bg.af.sub > 0) %>% #subtract the autofluorescence of the cells from the mean intensity of the cell
      mutate(gfp.mean.bg.af.sub.new = ifelse(gfp.mean.bg.af.sub <1 , NA, gfp.mean.bg.af.sub)) 
  })
  
  cells.filtered.df <- list(filtered.df, 
                            filtered.df.new)  
  names(cells.filtered.df) <- c("filtered.df", "filtered.df.new")
  
  return(cells.filtered.df)                 #fiiltered.df.new is the df with NAs for cells with intensities < gfp.threshold. filtered.df is the df without cells wiith less that gfp.thresold at t=0. 
}

#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 8: plot.ln.line.graphs()
#This function generates 4 line plots for gfp and mcherry intensities

x <- function(data, x, y, xlabel, ylabel){
  bind_rows(data$`20min`, 
            data$`40min`,
            data$`60min`) %>% 
    ggplot(.,aes(.[x], .[y], group = unique.trackID, color = experiment ))+
    geom_line(alpha = 0.2)+
    facet_wrap(~experiment)+
    #  guides(color = FALSE)+
    ylab(ylabel)+
    xlab(xlabel)+
    scale_y_log10()
}

plot.ln.line.graphs.temp <- function(data){
  xlabels <- c("Time (secs)", "Image #","Time (secs)", "Image #")
  ylabels <- c("ln(GFP)", "ln(GFP)", "ln(mCherry)", "ln(mcherry)")
  xcolid <- c("real.time.gfp", "timepoint", "real.time.gfp", "timepoint")
  ycolid <- c("gfp.mean.bg.sub.new", "gfp.mean.bg.sub.new" , "mcherry.mean.bg.sub" , "mcherry.mean.bg.sub" )
  
  plots <- lapply(1:4, function(i) {
  x(data, 
    x = xcolid[i], 
    y = ycolid[i], 
    xlabel = xlabels[i] , 
    ylabel = ylabels[i])
  })
  
  names(plots) <- c("gfp_time", "gfp_image" , "mcherry_time" , "mcherry_image")
  return(plots)
}

plot.ln.line.graphs <- function(filtered.df){
  
  #line plot of ln(gfp mean) vs the real time.
  ln.gfp.time <-  bind_rows(filtered.df$`20min`, 
                            filtered.df$`40min`,
                            filtered.df$`60min`) %>% 
    ggplot(.,aes(real.time.gfp, log(gfp.mean.bg.sub.new), group = unique.trackID, color = experiment ))+
    geom_line(alpha = 0.2)+
    facet_wrap(~experiment , scales =  "free_x")+
    guides(color = FALSE)+
    ylab("ln(GFP)")+
    xlab("Time (secs)")+
    scale_color_manual(values = c("grey", "grey" , "grey"))+
    theme_pubr()
  
  # ggsave(filename="semi-log_plot_gfp.png", plot= ln.gfp.time , path = "~/plots/4-9-10-30mgfp/test/" , width = 13 , height = 8, bg = "transparent")
  
  #line plot of ln(gfp mean) vs the image no.
  
  ln.gfp.image <- bind_rows(filtered.df$`20min`, 
                            filtered.df$`40min`,
                            filtered.df$`60min`) %>% 
    ggplot(.,aes(timepoint, log(gfp.mean.bg.sub.new), group = unique.trackID, color = experiment ))+
    geom_line(alpha = 0.2)+
    facet_wrap(~experiment)+
    guides(color = FALSE)+
    ylab("ln(GFP)")+
    xlab("Image #")+
    scale_color_manual(values = c("grey", "grey" , "grey"))+
    theme_pubr()
  
  # ggsave(filename="semi-log_plot_gfp_image.png", plot= ln.gfp.image , path = "~/plots/4-9-10-30mgfp/test/" , width = 13 , height = 8, bg = "transparent")
  
  ######################################################## mcherry plots
  #line plot of ln(mcherry mean) vs the real time.
  
  ln.mcherry.time <- bind_rows(filtered.df$`20min`, 
                               filtered.df$`40min`,
                               filtered.df$`60min`) %>% 
    ggplot(.,aes(real.time.mcherry, log(mcherry.mean.bg.sub.new), group = unique.trackID, color = experiment ))+
    geom_line(alpha = 0.2)+
    facet_wrap(~experiment , scales =  "free_x")+
    guides(color = FALSE)+
    xlab("Time (sec)")+
    ylab("ln(mcherry)")+
    scale_color_manual(values = c("grey", "grey" , "grey"))+
    theme_pubr()
  # ggsave(filename="semi-log_plot_mcherry.png", plot= ln.mcherry.time , path = "~/plots/4-9-10-30mgfp/test/" , width = 13 , height = 8, bg = "transparent")
  
  #line plot of ln(mcherry mean) vs image no.
  ln.mcherry.image <- bind_rows(filtered.df$`20min`, 
                                filtered.df$`40min`,
                                filtered.df$`60min`) %>% 
    ggplot(.,aes(timepoint, log(mcherry.mean.bg.sub.new), group = unique.trackID, color = experiment ))+
    geom_line(alpha = 0.2)+
    facet_wrap(~experiment)+
    #  guides(color = FALSE)+
    xlab("Image #")+
    ylab("ln(mcherry)")+
    scale_color_manual(values = c("grey", "grey" , "grey"))+
    theme_pubr() 
  # ggsave(filename="semi-log_plot_mcherry_image.png", plot= last_plot() , path = "~/plots/4-9-10-30mgfp/" , width = 13 , height = 8, bg = "transparent")
  
  graphs <- list(ln.gfp.time, 
                 ln.gfp.image,
                 ln.mcherry.time,
                 ln.mcherry.image)
  
  names(graphs) <- c("ln.gfp.time", 
                     "ln.gfp.image",
                     "ln.mcherry.time",
                     "ln.mcherry.image")
  return(graphs)
  # return(ln.gfp.image)
  # return(ln.mcherry.time)
  # return(ln.mcherry.image)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 9: df.model()

#Creating a data frame to use with the modeling: 
#selecting uniquetrackID, gfp, mcherry bg subtracted mean intensities , timepoint no. (which is the image no.) , experiment corresponding to the timelapse (20min, 40min and 60min timelape exp), unique field of views. 
#transform the intensity values on natural log. 
df.model <- function(filtered.df.new){               #  
  data.gfp <- bind_rows(filtered.df.new) %>%
    dplyr::select(unique.trackID,
                  gfp.mean.bg.sub.new, 
                  real.time.gfp, 
                  timepoint,
                  experiment,
                  exp.field) %>% 
    mutate(image.no = timepoint,
           nat.log.gfp = log(gfp.mean.bg.sub.new))
  
  data.rfp <- bind_rows(filtered.df.new) %>% 
    dplyr::select(unique.trackID,
                  mcherry.mean.bg.sub.new, 
                  real.time.mcherry, 
                  timepoint,
                  experiment,
                  exp.field) %>% 
    mutate(image.no = timepoint,
           nat.log.mcherry = log(mcherry.mean.bg.sub.new))
  df.for.modelling <- list(data.gfp,
                           data.rfp)
  names(df.for.modelling) <- c("data.gfp",
                               "data.rfp")
  return(df.for.modelling)
}

#making df for modelling only of gfp 
df.model.gfp <- function(filtered.df.new){               
  data.gfp <- bind_rows(filtered.df.new) %>%
    dplyr::select(unique.trackID,
                  gfp.mean.bg.af.sub.new, 
                  real.time.gfp, 
                  timepoint,
                  experiment,
                  exp.field) %>% 
    mutate(image.no = timepoint,
           nat.log.gfp = log(gfp.mean.bg.af.sub.new))
  return(data.gfp)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 10: clean.df.modelling.gfp()
#Clean the df from the creat_df_modelling function to use for modelling 
#subtracting the ln(intensity @ t=0) from ln(intensity @ t = t) 
clean.df.modeling.gfp <- function(data.gfp){
  
  #split the df based on the unique field of views. Need to the time of t=0 images by the rest of the times (this needs to be done when the first few images were blurry, but otherwise it doesn't matter since you're subtracting time at t =0 ie 0 from the rest of the times)
  data.split <- data.gfp %>% split(.$exp.field)
  data <- lapply(data.split, function(x){
    x <- x %>% 
      dplyr::mutate(., image =  image.no - 1 ) %>%
      mutate(.,real.time.gfp = real.time.gfp - real.time.gfp[1] ) %>% 
      dplyr::rename("time" = "real.time.gfp", 
                    "ln.gfp" = "nat.log.gfp") %>% 
      na.omit()
  }) %>% 
    bind_rows()
  
  #split the data frame by unique track IDs (basically splitting the df by single cells)
  # data1 <- data %>% 
  #   split(.$unique.trackID)
  # 
  # 
  # wider.df <- lapply(data1, function(x){
  #   x %>% 
  #     # filter(experiment != "60min") %>% 
  #     dplyr::select(unique.trackID, timepoint, ln.gfp) %>% 
  #     pivot_wider(., names_from = timepoint , values_from = ln.gfp) 
  # }) %>% 
  #   bind_rows()
  # 
  # wider.df <- column_to_rownames(wider.df, var = "unique.trackID")
  # wider.df <- wider.df[,1:31] - wider.df[,1] 
  # wider.df <- rownames_to_column(wider.df, var = "unique.trackID")
  # 
  # wider.df
  # 
  # longer.df <- wider.df %>% 
  #   pivot_longer(., cols = c(2:32) , names_to = "timepoint" , values_to = "ln.gfp") %>% 
  #   mutate(.,timepoint = as.numeric(timepoint))
  
###group by the cell ID and subtract the first timepoint log(int) with the rest for a given cell
  
  data <- data %>% 
    group_by(unique.trackID) %>% 
    mutate(new.ln.gfp = ln.gfp - ln.gfp[1]) %>% 
    ungroup() %>% 
    dplyr::rename(.,"ln.gfp.dif" = "new.ln.gfp")
  
  # data <- left_join(data, longer.df, by = c("unique.trackID", "timepoint")) %>% 
  #   dplyr::rename(.,"ln.gfp.old" = "ln.gfp.x", "intensity" = "ln.gfp.y")
  
  data <- data %>% 
    na.omit()
  return(data)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#Function 11: clean.df.modelling.mcherry()

#Clean the df from the creat_df_modelling function to use for modelling 


clean.df.modeling.mcherrry <- function(data.rfp){
  
  data.split <- data.rfp %>% split(.$exp.field)
  
  data <- lapply(data.split, function(x){
    x <- x %>% 
      dplyr::mutate(., image = image.no - 1 ) %>%
      mutate(.,real.time.mcherry = real.time.mcherry - real.time.mcherry[1] ) %>% 
      dplyr::rename("time" = "real.time.mcherry", 
                    "ln.rfp" = "nat.log.mcherry") %>% 
      na.omit()
  }) %>% 
    bind_rows()
  
  data1 <- data %>% 
    split(.$unique.trackID)
  
  wider.df <- lapply(data1, function(x){
    x %>% 
      # filter(experiment != "60min") %>% 
      dplyr::select(unique.trackID, timepoint, ln.rfp) %>% 
      pivot_wider(., names_from = timepoint , values_from = ln.rfp) 
  }) %>% 
    bind_rows()
  
  wider.df <- column_to_rownames(wider.df, var = "unique.trackID")
  wider.df <- wider.df[,1:31] - wider.df[,1] 
  wider.df <- rownames_to_column(wider.df, var = "unique.trackID")
  
  wider.df
  
  longer.df <- wider.df %>% 
    pivot_longer(., cols = c(2:32) , names_to = "timepoint" , values_to = "ln.rfp") %>% 
    mutate(.,timepoint = as.numeric(timepoint))
  
  data <- left_join(data, longer.df, by = c("unique.trackID", "timepoint")) %>% 
    dplyr::rename(.,"ln.rfp.old" = "ln.rfp.x", "intensity" = "ln.rfp.y")
  
  data <- data %>% 
    na.omit()
  return(data)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#function 12: eval.24models()
eval.24models <- function(data){
  library(minpack.lm)
  library(broom)
  model.out <- list()
  model.est <- list()
  # pred.val <- list()
  # intensity <- ln.gfp
  ####
  list.model <- NULL
  list.model[["linear.time.image"]] <- NULL
  list.model[["exp.time.neg"]] <- NULL
  list.model[["exp.image.neg"]] <- NULL
  list.model[["exp.time.image.neg"]] <- NULL
  list.model[["quadratic.time"]] <- NULL
  list.model[["quadratic.image"]] <- NULL
  list.model[["quadratic.image.time"]] <- NULL
  list.model[["pow.time"]] <- NULL
  list.model[["pow.image"]] <- NULL
  list.model[["pow.time.image"]] <- NULL
  list.model[["exp.time.neg_quad.image"]]<- NULL
  list.model[["exp.time.neg_pow.image"]]<- NULL
  list.model[["quad.time_exp.image"]]<- NULL
  list.model[["quad.time_pow.image"]]<- NULL
  list.model[["pow.time_quad.image"]]<- NULL
  list.model[["pow.time_exp.image.neg"]]<- NULL
  list.model[["only.lin.time"]]<- NULL
  list.model[["only.exp.time"]]<- NULL
  list.model[["only.quad.time"]] <- NULL
  list.model[["only.pow.time"]] <- NULL
  list.model[["only.lin.image"]] <- NULL
  list.model[["only.exp.image"]] <- NULL
  list.model[["only.quad.image"]] <- NULL
  list.model[["only.pow.image"]] <- NULL
  
  
  ####### 1. LINEAR TIME AND IMAGE
  # try(list.model[["linear.time.image"]] <- lm(data$intensity ~ data$time + data$image + 0))
  try(list.model[["linear.time.image"]] <- nls(data$intensity ~ a*data$time + b*data$image ,
                                               start = list(a = -1 , b = -1 ) , 
                                               lower = c(a = -Inf , b = -Inf) , 
                                               upper = c(a = 0 , b = 0 ), 
                                               algorithm = "port",
                                               data = data))
  
  ##### 2. Exponential Time Linear Image 
  try(list.model[["exp.time.neg"]] <- nls(data$intensity ~ a*(1-exp(c*(data$time)/60)) + b*data$image , 
                                          start = list(a = -1 , c = -0.5 , b = -1 ) , 
                                          lower = c(a = -Inf , c = -Inf , b = -Inf) , ###change the limits of c to 0-INF
                                          upper = c(a = 0, c = 0 , b = 0 ), 
                                          algorithm = "port",
                                          data = data))
  
  ##### 3. Exponential Image Linear Time
  try(list.model[["exp.image.neg"]] <- nls(data$intensity ~ a*data$time + b*(1-exp(c*data$image)) , 
                                           start = list(a = -1 , b = -1 , c = -1 ) ,
                                           lower = c(a = -Inf, b = -Inf , c = -Inf ), 
                                           upper = c(a = 0 , b = 0 , c = 0),
                                           data = data,
                                           algorithm = "port"))
  
  
  ##### 4. Exponential Time Exponential Image 
  try(list.model[["exp.time.image.neg"]] <- nls(data$intensity ~ a*(1-exp(c*(data$time)/60)) + b*(1-exp(d*data$image)) ,
                                                start = list(a = -1 , c = -1 , b = -1, d = -1 ) ,
                                                lower = c(a = -Inf, c = -Inf, b = -Inf , d = -Inf ),
                                                upper = c(a = 0, c = 0, b = 0 , d = 0 ),
                                                data = data,
                                                algorithm = "port"))
  
  #########QUADRATIC FUNCTIONS
  ##### 5. Quadratic Time Linear Image 
  try(list.model[["quadratic.time"]] <- nls(data$intensity ~ a1*data$time + a2*(data$time)^2 + b*data$image, 
                                            start = list(a1 = -1 , a2 = -1 , b = -1 ),
                                            lower = c(a1 = -Inf , a2 = -Inf , b = -Inf ),
                                            upper = c(a1 = 0 , a2 = Inf , b = 0 ),
                                            algorithm = "port",
                                            data = data))
  
  ##### 6. Quadratic Image Linear Time
  # try(list.model[["quadratic.image"]] <-  lm(intensity ~ time + image + I(image^2) + 0 , data = data))
  try(list.model[["quadratic.image"]] <-  nls(data$intensity ~ a*data$time +  b1*data$image +b2*(data$image)^2 ,
                                              start = list(a = -1 , b1 = -1 , b2 = -1 ),
                                              lower = c(a = -Inf , b1 = -Inf , b2 = -Inf ),
                                              upper = c(a = 0 , b1 = 0 , b2 = Inf),
                                              algorithm = "port",
                                              data = data))
  
  ##### 7. Quadratic Time quadratic Image
  # try(list.model[["quadratic.image.time"]] <-  lm(intensity ~ time + I(time^2)+ image + I(image^2) + 0, data = data))
  try(list.model[["quadratic.image.time"]] <- nls(data$intensity ~ a1*data$time + a2*(data$time)^2 + b1*data$image +b2*(data$image)^2,
                                                  start = list(a1 = -1, a2 = -1 , b1 = -1 , b2 = -1 ),
                                                  lower = c(a1 = -Inf , a2 = -Inf , b1 = -Inf , b2 = -Inf),
                                                  upper = c(a1 = 0 , a2 = Inf, b1 = 0 , b2 = Inf),
                                                  algorithm = "port",
                                                  data = data))
  
  ########power law function: 
  ##### 8. Power Time Linear Image
  try(list.model[["pow.time"]] <- nls(intensity ~ a*(time)^c + b*image, 
                                      start = list(a = -1 , c = 0.5, b = -0.5 ) , 
                                      lower = c(a = -Inf , c = 0 , b = -Inf ),
                                      upper = c(a = 0 , c = 1 , b = 0 ),
                                      algorithm = "port",
                                      data = data)) 
  
  ##### 9. Power Image Linear Time
  try(list.model[["pow.image"]] <- nls(intensity ~ a*data$time + b*(data$image)^c , 
                                       start = list(a = -1 , b = -1 , c = 1 ), 
                                       lower = c(a = -Inf , b = -Inf , c = 0 ),
                                       upper = c(a = 0 , b = 0 , c = 1 ),
                                       algorithm = "port",
                                       data = data))
  
  ##### 10. Power Time Power Image
  try(list.model[["pow.time.image"]] <- nls(intensity ~ a*(data$time)^c + b*(data$image)^d , 
                                            start = list(a = -1 , c = 1, b = -1, d = 1) ,
                                            lower = c(a = -Inf, c = 0 , b = -Inf , d = 0),
                                            upper = c(a = 0 , c = 1 , b = 0 , d = 1 ),
                                            algorithm = "port",
                                            data = data,
                                            control = nls.control(maxiter = 10000)))
  
  #########Combination functions Pow VS Exp ; Quad VS Exp ; Quad VS pow 
  
  ###### 11. exponential time + quadratic image 
  try(list.model[["exp.time.neg_quad.image"]] <- nls(intensity ~ a*(1-exp(c*time/60)) + b1*image + b2*(image)^2, 
                                                     start = list(a = -1, c = -1, b1 = -1 , b2 = -1 ) ,
                                                     lower = c(a = -Inf , c = -Inf, b1 = -Inf , b2 = -Inf),
                                                     upper =  c(a = 0 ,   c = 0 ,   b1 = 0 ,    b2 = Inf ),
                                                     algorithm = "port",
                                                     data = data , 
                                                     control = nls.lm.control(maxiter = 10000)))
  
  ###### 12. exponential time + power image 
  #exp.time with neg bounds
  try(list.model[["exp.time.neg_pow.image"]] <- nls(intensity ~ a*(1-exp(c*data$time/60)) + b*(data$image)^d, 
                                                    start = list(a = -1, c = -0.5, b = -1 , d = 1 ) , 
                                                    lower = c(a = -Inf , c = -Inf , b = -Inf , d =0 ),
                                                    upper = c(a = 0, c = 0 , b = 0 , d = 1 ),
                                                    algorithm = "port",
                                                    control = nls.lm.control(maxiter = 10000),
                                                    data = data))  
  
  ####### 13. quadratic time + exponential image 
  try(list.model[["quad.time_exp.image"]] <- nls(intensity ~ a1*data$time + a2*(data$time)^2 + b*(1-exp(c*data$image)), 
                                                 start = list(a1 = -1, a2 = -1, b = -0.5 , c = -0.5 ) , 
                                                 lower = c(a1 = -Inf, a2 = -Inf, b = -Inf , c = -Inf),
                                                 upper = c(a1 = 0 , a2 = Inf , b = 0 , c = 0 ),
                                                 algorithm = "port",
                                                 data = data))
  
  ####### 14. quadratic time + power image 
  try(list.model[["quad.time_pow.image"]] <- nls(intensity ~ a1*data$time + a2*(data$time)^2 + b*(data$image)^c , 
                                                 start = list(a1 = -1, a2 = -1, b = -1 , c = 1) , 
                                                 lower = c(a1 = -Inf, a2 = -Inf , b = -Inf , c = 0 ),
                                                 upper = c(a1 = 0, a2 = Inf, b = 0 , c = 1 ),
                                                 algorithm = "port",
                                                 data = data) , silent = TRUE)
  
  ####### 15.  power time + quadtratic image 
  try(list.model[["pow.time_quad.image"]] <- nls(intensity ~ a*(data$time)^c + b1*data$image + b2*(data$image)^2, 
                                                 start = list(a = -1, c = 0.5, b1 = -1 , b2 = -1 ) , 
                                                 lower = c(a = -Inf , c = 0, b1 = -Inf , b2 = -Inf ),
                                                 upper = c(a = 0, c = 1, b1 = 0, b2 = Inf ),
                                                 algorithm = "port",
                                                 data = data))
  
  ####### 16. power time + exponential image 
  #exp.image with neg bounds
  try(list.model[["pow.time_exp.image.neg"]] <- nls(intensity ~ a*(data$time)^c + b*(1-exp(d*data$image)), 
                                                    start = list(a = -1, c = 1, b = -1 , d = -0.5 ) , 
                                                    lower = c(a = -Inf, c = 0 , b = -Inf, d = -Inf ),
                                                    upper = c(a = 0 , c = 1, b = 0, d = 0 ),
                                                    algorithm = "port",
                                                    data = data)) 
  
  #############singular models 
  #####TIME
  #### 17. Linear time
  try(list.model[["only.lin.time"]] <- nls(data$intensity ~ a*data$time,
                                           start = list(a = -1) , 
                                           lower = c(a = -Inf ) , 
                                           upper = c(a = 0), 
                                           algorithm = "port",
                                           data = data))
  
  ##### 18. exponential time
  try(list.model[["only.exp.time"]] <-nls(intensity ~ a*(1-exp(b*(data$time)/60)), 
                                          start = list(a = -1 , b = -0.5  ) , 
                                          lower = c(a = -Inf , b = -Inf) , 
                                          upper = c(a = 0, b = 0), 
                                          algorithm = "port",
                                          data = data) )
  
  ##### 19. quadratic time 
  # try(list.model[["only.quad.time"]] <- lm(intensity ~ time + I(time^2) +0 , data = data) ) 
  try(list.model[["only.quad.time"]] <-nls(data$intensity ~ a1*data$time + a2*(data$time)^2, 
                                           start = list(a1 = -1 , a2 = -1  ),
                                           lower = c(a1 = -Inf , a2 = -Inf),
                                           upper = c(a1 = 0 , a2 = Inf  ),
                                           algorithm = "port",
                                           data = data))
  
  
  ##### 20. power time 
  try(list.model[["only.pow.time"]] <- nls(intensity ~ a*(time)^c , 
                                           start = list(a = -1 , c = 0.5 ) , 
                                           lower = c(a = -Inf , c = 0  ),
                                           upper = c(a = 0 , c = 1 ),
                                           algorithm = "port",
                                           data = data) )
  
  #####IMAGE
  #### 21. Linear Image 
  try(list.model[["only.lin.image"]] <-  nls(data$intensity ~ a*data$image,
                                             start = list(a = -1) , 
                                             lower = c(a = -Inf ) , 
                                             upper = c(a = 0), 
                                             algorithm = "port",
                                             data = data))
  
  ##### 22. exponential image 
  try(list.model[["only.exp.image"]] <-nls(intensity ~ a*(1-exp(b*(data$image)/60)), 
                                           start = list(a = -1 , b = -0.5  ) , 
                                           lower = c(a = -Inf , b = -Inf) , 
                                           upper = c(a = 0, b = 0), 
                                           algorithm = "port",
                                           data = data) )
  
  ##### 23. quadratic image 
  # try(list.model[["only.quad.image"]] <-  lm(intensity ~ image + I(image^2) + 0 , data = data) )
  try(list.model[["only.quad.image"]] <-nls(intensity~ a1*data$image + a2*(data$image)^2, 
                                            start = list(a1 = -1 , a2 = -1  ),
                                            lower = c(a1 = -Inf , a2 = -Inf),
                                            upper = c(a1 = 0 , a2 = Inf ),
                                            algorithm = "port",
                                            data = data))
  
  ##### 24. power image 
  try(list.model[["only.pow.image"]] <- nls(intensity ~ a*(image)^c , 
                                            start = list(a = -1 , c = 0.5 ) , 
                                            lower = c(a = -Inf , c = 0  ),
                                            upper = c(a = 0 , c = 1 ),
                                            algorithm = "port",
                                            data = data) )
  
  ###extracting the model estimates and statistical inferences 
  func.glance <- function(model){
    tryCatch({
      df.glance <-  model %>% glance()
      return(df.glance)},
      error = function(err){return(as.data.frame(NA))})
  }
  
  model.out <- lapply(list.model, func.glance)
  
  func.tidy <- function(model){
    tryCatch({
      df.tidy <-  model %>% tidy()
      return(df.tidy)},
      error = function(err){return(as.data.frame(NA))})
  }
  
  model.est <- lapply(list.model, func.tidy)
  
  
  models <- list(bind_rows(model.out , .id = "func") , #have bind rows in here
                 bind_rows(model.est , .id = "func" ), 
                 list.model)
  
  names(models) <- c("model.stats" , "model.est" , "list.model")
  
  return(models)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#function 13: pred.val.models()

pred.val.models <- function(list.model, df , param){ #takes the list of models , the dataframe with the time and image values as X1 and X2 and the df with all the model estimates. 
  
  pred.val <- list()
  
  #1
  if(!is.null(list.model[["linear.time.image"]])){
    est <- param %>% filter(func == "linear.time.image") %>% select(term, estimate) 
    pred.val[["linear.time.image"]] <- df %>%
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*image ,
             pred.time = as.numeric(est[1,2])*time,
             pred.image = as.numeric(est[2,2])*image,
             func = "linear_time_linear_image")}
  
  #2
  if(!is.null(list.model[["exp.time.neg"]])){
    est <- param %>% filter(func == "exp.time.neg") %>% select(term, estimate) 
    pred.val[["exp.time.linear.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*(time)/60)) + as.numeric(tidy(list.model[["exp.time.neg"]])[3,2])*image,
             pred.time = as.numeric(tidy(list.model[["exp.time.neg"]])[1,2])*(1-exp(as.numeric(tidy(list.model[["exp.time.neg"]])[2,2])*(time)/60)),
             pred.image =  as.numeric(tidy(list.model[["exp.time.neg"]])[3,2])*image  ,
             func = "exponential_time_linear_image")
  }
  
  #3
  if(!is.null(list.model$exp.image.neg)){
    est <- param %>% filter(func == "exp.image.neg") %>% select(term, estimate) 
    pred.val[["exp.image.linear.time"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(1-exp(as.numeric(est[3,2])*image)) ,
             pred.time = as.numeric(est[1,2])*time,
             pred.image = as.numeric(est[2,2])*(1-exp(as.numeric(est[3,2])*image)),
             func = "exponential_image_linear_time")
  }
  
  #4
  if(!is.null(list.model$exp.time.image.neg)){
    est <- param %>% filter(func == "exp.time.image.neg") %>% select(term, estimate) 
    pred.val[["exp.time.exp.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*(time)/60)) +as.numeric(est[3,2])*(1-exp(as.numeric(est[4,2])*image)),
             pred.time = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*(time)/60)),
             pred.image = as.numeric(est[3,2])*(1-exp(as.numeric(est[4,2])*image)),
             func = "exponential_time_exponential_image")
  }
  
  #5
  if(!is.null(list.model$quadratic.time)){
    est <- param %>% filter(func == "quadratic.time") %>% select(term, estimate)
    pred.val[["quadratic.time.linear.image"]] <-df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 + as.numeric(est[3,2])*image,
             pred.time = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 ,
             pred.image =   as.numeric(est[3,2])*image , 
             func = "quad_time_lin_image") 
  }
  
  #6
  if (!is.null(list.model$quadratic.image)) {
    est <- param %>% filter(func == "quadratic.image") %>% select(term, estimate)
    pred.val[["quadratic.image.linear.time"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time +  as.numeric(est[2,2])*image +as.numeric(est[3,2])*(image)^2,
             pred.time = as.numeric(est[1,2])*time ,
             pred.image = as.numeric(est[2,2])*image +as.numeric(est[3,2])*(image)^2 , 
             func = "linear_time_quadratic_image") 
  }
  
  #7
  if (!is.null(list.model$quadratic.image.time )) {
    est <- param %>% filter(func == "quadratic.image.time") %>% select(term, estimate)
    pred.val[["quadratic.time.quad.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 + 
               as.numeric(est[3,2])*image +as.numeric(est[4,2])*(image)^2 ,
             pred.time = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 ,
             pred.image = as.numeric(est[3,2])*image +as.numeric(est[4,2])*(image)^2 , 
             func = "quadratic_time_quadratic_image")
  }
  
  #8
  if (!is.null(list.model$pow.time)) {
    est <- param %>% filter(func == "pow.time") %>% select(term, estimate)
    pred.val[["pow.time.lin.image"]] <- df %>% 
      mutate(pred.all =  as.numeric(est[1,2])*(time)^as.numeric(est[2,2]) + as.numeric(est[3,2])*image ,
             pred.time = as.numeric(est[1,2])*(time)^as.numeric(est[2,2])  ,
             pred.image =  as.numeric(est[3,2])*image ,
             func = "power_time_linear_image")
  }
  
  #9
  if (!is.null(list.model$pow.image)) {
    est <- param %>% filter(func == "pow.image") %>% select(term, estimate)
    pred.val[["pow.image.lin.time"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(image)^as.numeric(est[3,2])  ,
             pred.time = as.numeric(est[1,2])*time  ,
             pred.image = as.numeric(est[2,2])*(image)^as.numeric(est[3,2])  ,
             func = "linear_time_power_image")
    
  }
  
  #10
  if (!is.null(list.model$pow.time.image)) {
    est <- param %>% filter(func == "pow.time.image") %>% select(term, estimate)
    pred.val[["pow.time.pow.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(time)^as.numeric(est[2,2]) +  as.numeric(est[3,2])*(image)^as.numeric(est[4,2]) ,
             pred.time = as.numeric(est[1,2])*(time)^as.numeric(est[2,2]),
             pred.image = as.numeric(est[3,2])*(image)^as.numeric(est[4,2]),
             func = "pow_time_pow_image")
  }
  
  #11
  if (!is.null(list.model$exp.time.neg_quad.image)) {
    est <- param %>% filter(func == "exp.time.neg_quad.image") %>% select(term, estimate)
    pred.val[["exp.time.neg_quad.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*time/60)) +
               as.numeric(est[3,2])*image + as.numeric(est[4,2])*(image)^2 ,
             pred.time =  as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*time/60)) ,
             pred.image =  as.numeric(est[3,2])*image + as.numeric(est[4,2])*(image)^2 ,
             func = "exp_time_quad_image")
  }
  
  #12
  if (!is.null(list.model$exp.time.neg_pow.image)) {
    est <- param %>% filter(func == "exp.time.neg_pow.image") %>% select(term, estimate)
    pred.val[["exp.time.neg_pow.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*time/60)) +
               as.numeric(est[3,2])*(image)^as.numeric(est[4,2]) ,
             pred.time =  as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*time/60)) ,
             pred.image =  as.numeric(est[3,2])*(image)^as.numeric(est[4,2]),
             func = "exp.time.neg_pow.image")
  }
  
  #13
  if (!is.null(list.model$quad.time_exp.image)) {
    est <- param %>% filter(func == "quad.time_exp.image") %>% select(term, estimate)
    pred.val[["quad.time_exp.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 + as.numeric(est[3,2])*(1-exp(as.numeric(est[4,2])*image)),
             pred.time = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 ,
             pred.image = as.numeric(est[3,2])*(1-exp(as.numeric(est[4,2])*image)),
             func = "quad.time_exp.image")
  }
  
  
  #14
  if (!is.null(list.model$quad.time_pow.image)) {
    est <- param %>% filter(func == "quad.time_pow.image") %>% select(term, estimate)
    pred.val[["quad.time_pow.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 + as.numeric(est[3,2])*(image)^as.numeric(est[4,2]), 
             pred.time =  as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2,
             pred.image =   as.numeric(est[3,2])*(image)^as.numeric(est[4,2]),
             func = "quad.time_pow.image")
  }
  
  #15
  if (!is.null(list.model$pow.time_quad.image)) {
    est <- param %>% filter(func == "pow.time_quad.image") %>% select(term, estimate)
    pred.val[["pow.time_quad.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(time)^as.numeric(est[2,2]) +
               as.numeric(est[3,2])*image + as.numeric(est[4,2])*(image)^2,
             pred.time =  as.numeric(est[1,2])*(time)^as.numeric(est[2,2]) ,
             pred.image =  as.numeric(est[3,2])*image + as.numeric(est[4,2])*(image)^2,
             func = "pow.time_quad.image")
  }
  
  
  #16
  if (!is.null(list.model$pow.time_exp.image.neg)) {
    est <- param %>% filter(func == "pow.time_exp.image.neg") %>% select(term, estimate)
    pred.val[["pow.time_exp.image.neg"]] <-  df %>%
      mutate(pred.all = as.numeric(est[1,2])*(time)^as.numeric(est[2,2]) +
               as.numeric(est[3,2])*(1-exp(as.numeric(est[4,2])*image)),
             pred.time = as.numeric(est[1,2])*(time)^as.numeric(est[2,2]),
             pred.image = as.numeric(est[3,2])*(1-exp(as.numeric(est[4,2])*image)),
             func = "pow.time_exp.image ")
  }
  
  #17
  
  if (!is.null(list.model$only.lin.time)) {
    est <- param %>% filter(func == "only.lin.time") %>% select(term, estimate)
    pred.val[["only.linear.time"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time, 
             func = "only_lin_time")
  }
  
  #18
  if (!is.null(list.model$only.exp.time)) {
    est <- param %>% filter(func == "only.exp.time") %>% select(term, estimate)
    pred.val[["only.exp.time"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*(time)/60)),
             func = "only_exp_time")
  }
  
  #19
  if (!is.null(list.model$only.quad.time)) {
    est <- param %>% filter(func == "only.quad.time") %>% select(term, estimate)
    pred.val[["only.quad.time"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*time + as.numeric(est[2,2])*(time)^2 ,
             func = "only.quad.time")
  }
  
  
  #20
  if (!is.null(list.model$only.pow.time)) {
    est <- param %>% filter(func == "only.pow.time") %>% select(term, estimate)
    pred.val[["only.pow.time"]] <- df %>% 
      mutate(pred.all =  as.numeric(est[1,2])*(time)^as.numeric(est[2,2]) ,
             func = "only_pow_time")
  }
  
  #21
  if (!is.null(list.model$only.lin.image)) {
    est <- param %>% filter(func == "only.lin.image") %>% select(term, estimate)
    pred.val[["only.linear.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*image, 
             func = "only_lin_image")
  }
  
  #22
  
  if (!is.null(list.model$only.exp.image)) {
    est <- param %>% filter(func == "only.exp.image") %>% select(term, estimate)
    pred.val[["only.exp.image"]] <-df %>% 
      mutate(pred.all = as.numeric(est[1,2])*(1-exp(as.numeric(est[2,2])*(image)/60)),
             func = "only_exp_image")
  }
  
  #23
  if (!is.null(list.model$only.quad.image)) {
    est <- param %>% filter(func == "only.quad.image") %>% select(term, estimate)
    pred.val[["only.quad.image"]] <- df %>% 
      mutate(pred.all = as.numeric(est[1,2])*image + as.numeric(est[2,2])*(image)^2 ,
             func = "only_quad_image")
  }
  
  #24
  if (!is.null(list.model$only.pow.image)) {
    est <- param %>% filter(func == "only.pow.image") %>% select(term, estimate)
    pred.val[["only.pow.image"]] <- df %>% 
      mutate(pred.all =  as.numeric(est[1,2])*(image)^as.numeric(est[2,2]) ,
             func = "only_pow_image")
  }
  
  pred.val <- pred.val %>% bind_rows() 
  
  return(pred.val)
  
}

#----------------------------------------------------------------------

#----------------------------------------------------------------------

#Plot the AIC and BIC scores 
aic.bic.plot <- function(model.out){
  aic.fig <- model.out %>% 
    dplyr::select(.,func,
                  AIC) %>% 
    pivot_longer(., cols = "AIC" ) %>% 
    arrange(value) %>% 
    mutate(fun.new = paste0(func,"_",name)) %>% 
    mutate(fun.new = factor(fun.new, levels = fun.new)) %>% 
    ggplot(.,aes(fun.new, value , color = name))+
    geom_point()+
    theme(axis.text.x = element_text(angle = 90))+
    xlab("function")
  
  bic.fig <- model.out %>% 
    dplyr::select(.,func,
                  BIC) %>% 
    pivot_longer(., cols = "BIC" ) %>% 
    arrange(value) %>% 
    mutate(fun.new = paste0(func,"_",name)) %>% 
    mutate(fun.new = factor(fun.new, levels = fun.new)) %>% 
    ggplot(.,aes(fun.new, value , color = name))+
    geom_point()+
    theme(axis.text.x = element_text(angle = 90))+
    xlab("function")
  
  aic.bic.figs <- list(aic.fig , bic.fig)
  
  return(aic.bic.figs)
}

#----------------------------------------------------------------------

#----------------------------------------------------------------------
#function 14: plot.pred.val()
#this plot the predicted values for all the converged models 
plot.pred.val <- function(cellsInfoDf, expt, PredValsDf){ 
  #cellsInfoDf is the final df used for modeling ; 
  #expt is a vector of string specifying which experiment c("20min","40min","60min")
  #PredValsDf is the df with the predicted values for each successful functional form
  #PredInt is a vector of all the 
  predPlots <- lapply(expt, function(a){
    actual.data.20min.31tp <- cellsInfoDf %>% filter(experiment == a) 
    PredValsDf %>%
      # select(-pred.image) %>% 
      filter(experiment == a) %>% 
      pivot_longer(pred.all:pred.image) %>% 
      rename("intensity" = "value") %>% 
      ggplot(.,aes(x = time, y = intensity, col = name ))+
      geom_line()+
      facet_wrap(~func)+
      geom_line(data = actual.data.20min.31tp, aes(x = time, y = intensity , group = unique.trackID ), color = "grey" , alpha = 0.01 , inherit.aes = FALSE)+
      theme_pubr()
  })
  
  names(predPlots) <- c("20min","40min","60min")
  return(predPlots)
}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#function 15: plot.bestfit.predVal()
#plotting the predicted values for the model with the lowest AIC score. this model name is a parameter which needs to be inputted by the user. 
plot.bestfit.predVal <- function(cellsInfoDf, PredValsDf, bestFit){ 
  #cellsInfoDf is the final df used for modeling ; 
  #PredValsDf is the df with the predicted values for each successful functional form
  #bestFit is a string with the best fit functional form
  
   bestfitplot <- PredValsDf %>%
      select(-predVal) %>% 
      pivot_longer(starts_with("pred")) %>% 
      rename("intensity" = "value") %>% 
      filter(func == bestFit) %>%
      ggplot(.,aes(x = time, y = intensity, color = name ))+
      geom_line(alpha = 1 )+
     scale_color_manual(values = c(""))+
    facet_wrap(~experiment , scales = "free_x")+
      geom_line(data = cellsInfoDf, aes(x = time, y = intensity , group = unique.trackID ), color = "grey" , alpha = 0.05 , inherit.aes = FALSE)+
      theme_pubr()
  
  return(bestfitplot)
}

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#function 15.a: plotSepAxisBestFit()
#plotting the predicted values for the model with the lowest AIC score. this model name is a parameter which needs to be inputted by the user. but this plots the predicted values for the image component on the image x axis and the time component on the time x axis. 
plotSepAxisBestFit <- function(cellsInfoDf, PredValsDf, bestFit){
  
  df.pred <- list(data.frame(x = "time", predVal = "pred.image"), #if i am removing the pred.image from the df, then plot time on the x axis
                  data.frame(x = "image", predVal = "pred.time")) #if i am removing the pred.time from the df, then plot image on the x axis
  
  bestfitplotSepX <- lapply(df.pred, function(a){
    PredValsDf %>%
      select(-a$predVal) %>%                    #remove the predicted values column for either time or image regressor 
      pivot_longer(starts_with("pred")) %>% 
      rename("intensity" = "value") %>% 
      filter(func == bestFit) %>%
      ggplot(.,aes_string(x = a$x, y = "intensity", color = "name" ))+
      geom_line(alpha = 1 )+
      # scale_color_manual(values = c(""))+
      facet_wrap(~experiment , scales = "free_x")+
      geom_line(data = cellsInfoDf, aes_string(x = a$x, y = "intensity" , group = "unique.trackID" ), color = "grey" , alpha = 0.01 , inherit.aes = FALSE)+
      theme_pubr()
  })
  names(bestfitplotSepX) <- c("time","image")
  return(bestfitplotSepX)
}
#----------------------------------------------------------------------
#----------------------------------------------------------------------

