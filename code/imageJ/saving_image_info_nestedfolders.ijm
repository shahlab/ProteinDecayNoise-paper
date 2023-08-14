input_dir = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/cln2/data/"; 
output_dir = "/Volumes/Backup_sd1/Nikon_TiE/10-7-22-pup1/cln2/image_info_imageJ/";

setBatchMode(true);

//list_exp = getFileList(input_dir);

//for(i=0 ; i<list_exp.length ; i++){
	
	//list_sample = getFileList(input_dir + list_exp[i]);
	list_sample = getFileList(input_dir);
	
	for(j=0; j<list_sample.length ; j++){

		list_images = getFileList(input_dir + list_sample[j]);
		//list_images = getFileList(input_dir);
		
		//list_images = getFileList(input_dir + list_exp[i] + list_sample[j]);
		
		for(k=0; k<list_images.length ; k++){
			if (endsWith(list_images[k], ".TIF")) {
				open(input_dir + list_sample[j]+ list_images[k]);
				selectWindow(list_images[k]);
				run("Show Info...");
				saveAs("Text", output_dir + list_images[k]);
				run("Close");
				continue
				}
			else {
				if (list_images[k] == "blurry/") {
					continue
					}
					
				if (list_images[k] == "extra_mcherry_dapi/") {
					continue
					}
				
				else {
					list_gfp = getFileList(input_dir + list_sample[j]+ list_images[k]);
						
					for(g = 0; g<list_gfp.length ; g++){
						open(input_dir + list_sample[j]+ list_images[k] + list_gfp[g]);
						selectWindow(list_gfp[g]);
						run("Show Info...");
						saveAs("Text", output_dir + list_gfp[g]);
						close("*");
						run("Close");
					
					}
				}
				}
			}
		}
	//}
close("*");
setBatchMode(false);