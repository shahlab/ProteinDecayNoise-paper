@files = `ls *TIF`;
chomp(@files);
for($i=0; $i<@files; $i++){
  `mkdir image-$i`;
  `mkdir image-seg_$i`;
  `mv $files[$i] image-$i`;
  `echo $files[$i] image-$i >> image.list`;
}
