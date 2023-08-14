//This is to be used right before imaris analysis where all the files need to be in 16 bit. 
//Directory structure: 20min/s1/*.TIF and 20mins/s1/mcherry-dapi/*.TIF
input_dir = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/cln2/data/"; //

setBatchMode(true);
list_exp = getFileList(input_dir);


for(i=0 ; i<list_exp.length ; i++){
	
	list_image = getFileList(input_dir + list_exp[i]);
	//list_sample = getFileList(input_dir);
	
	for(j=0; j<list_image.length ; j++){
		if (endsWith(list_image[j], ".TIF")) {
		open(input_dir + list_exp[i] + list_image[j]);
			//selectWindow(list_sample[j]);
		selectWindow(list_image[j]);
		run("16-bit");
		run("Save");
				//close();
		close("*");
		}	
	}
}


close("*");
setBatchMode(false);

