input_dir = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/stable/data/"; //data structure: data/20min/s[1-8]. 
output_bg_roi = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/stable/background/image_bg_roi/";
output = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/stable/background/bg_gfp/";
list_dir = getFileList(input_dir);
setBatchMode(true);
for(k=0 ; k<list_dir.length ; k++){
	input_mask = input_dir + list_dir[k] + "mask/";						//Add in the path for the folder with yeastSpotter mask
	input_GFP = input_dir + list_dir[k] + "gfp/";						//Add in the path for the GFP folder 
	//input_mcherry = input_dir + list_dir[k] + "mcherry/";				//Add in the path for mcherry folder 
	//input_dapi = input_dir + list_dir[k] + "dapi/";

	list_mask = getFileList(input_mask);
	list_GFP = getFileList(input_GFP);
	//list_mcherry = getFileList(input_mcherry);
	//list_dapi = getFileList(input_dapi);
	
	for (i = 0; i < list_mask.length; i++) {
		open(input_mask + list_mask[i]);                     //open the mask from yeast spotter 
		selectWindow(list_mask[i]);                    //select the mask window
		setAutoThreshold("Default dark no-reset");
		run("Threshold...");
		setThreshold(0, 0);
		setOption("BlackBackground", true);
		run("Convert to Mask");
		run("Dilate");
		run("Analyze Particles...", "display clear add");
		close("Results");
		selectWindow(list_mask[i]);
		run("Select All");
		roiManager("Add");
		roiManager("Show All without labels");
		roiManager("XOR");                     				//this subtracts the cells mask from the entire image 
		roiManager("Add");									// Add the new subtracted area as a new roi 
		x = roiManager("count");
		roiManager("Select", x-1);   						//select the last ROI , x-1 because the first roi is 0, and x is the total no. of ROI 
		run("Enlarge...", "enlarge=-10");
		roiManager("Add");									//add a new roi which is the area outside the cells 
		roiManager("Select", x);
		roiManager("Set Fill Color", "yellow");
		selectWindow(list_mask[i]);
		saveAs("Jpeg", output_bg_roi + list_mask[i]);

//getting GFP bg intensity
		open(input_GFP + list_GFP[i]);
		selectWindow(list_GFP[i]);
		roiManager("Select", x);							
		run("Measure");
		selectWindow("Results");
		saveAs("Results", output + replace(list_GFP[i],".TIF",".csv"));

//getting dapi bg intensity
//		open(input_dapi + list_dapi[i]);
//		selectWindow(list_dapi[i]);
//		roiManager("Select", x);							
//		run("Measure");

//getting mcherry bg intensity	
		//open(input_mcherry + list_mcherry[i]);
		//selectWindow(list_mcherry[i]);
		//roiManager("Select", x);	
		//run("Measure");

//Saving the results		
		//selectWindow("Results");
		//saveAs("Results", output_mcherry + replace(list_mcherry[i],".TIF","_bg.csv"));
		//close();
	}
}
setBatchMode(false);
run("Close All");