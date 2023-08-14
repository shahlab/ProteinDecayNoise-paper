input_dir = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/cln2/data/";     //takes in the individual experiments one at a time ie 20min, 40min, 60min
output_bg_roi = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/cln2/background/image_bg_roi_dapi_mcherry/";
output_mcherry = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/cln2/background/bg_dapi_mcherry/";
list_dir = getFileList(input_dir);  //gives in 
setBatchMode(true);
for(k=0 ; k<list_dir.length ; k++){
	input_mask = input_dir + list_dir[k] + "mask-t1-t31/";	//Add in the path for the folder with yeastSpotter mask for t1 and t31 since dapi and mcherry is only imaged at t1 and t31
	//input_GFP = input_dir + list_dir[k] + "gfp/";			//Add in the path for the GFP folder 
	input_mcherry = input_dir + list_dir[k] + "mcherry/";   //Add in the path for mcherry folder with mcherry images 
	input_dapi = input_dir + list_dir[k] + "dapi/";        //Add in the path for dapi folder with dapi images

	list_mask = getFileList(input_mask);                   
	//list_GFP = getFileList(input_GFP);
	list_mcherry = getFileList(input_mcherry);
	list_dapi = getFileList(input_dapi);
	
	for (i = 0; i < list_mask.length; i++) {
		open(input_mask + list_mask[i]);                //open the mask from yeast spotter 
		selectWindow(list_mask[i]);                    //select the mask window
		setAutoThreshold("Default dark no-reset");
		run("Threshold...");
		setThreshold(0, 0);
		setOption("BlackBackground", true);
		run("Convert to Mask");
		run("Dilate");
		//run("Close");         
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
	//	open(input_GFP + list_GFP[i]);
	//	selectWindow(list_GFP[i]);
	//	roiManager("Select", x);							
	//	run("Measure");
	//	selectWindow("Results");
	//	saveAs("Results", output + replace(list_GFP[i],".TIF",".csv"));

//getting dapi bg intensity
		open(input_dapi + list_dapi[i]);
		selectWindow(list_dapi[i]);
		roiManager("Select", x);							
		run("Measure");

//getting mcherry bg intensity	
		open(input_mcherry + list_mcherry[i]);
		selectWindow(list_mcherry[i]);
		roiManager("Select", x);	
		run("Measure");

//Saving the results		
		selectWindow("Results");
		saveAs("Results", output_mcherry + replace(list_mcherry[i],".TIF","_bg.csv"));
		close();
	}
}
setBatchMode(false);
run("Close All");