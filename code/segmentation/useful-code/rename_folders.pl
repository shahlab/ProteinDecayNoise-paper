@file = `ls image-seg*`;
@num = (1001..1227);
@num2 = (1..227);
chomp(@files);
for($i=0; $i<@file; $i++){
  `mv image-$num[$i] image-$num2[$i]`;
  `mv image-seg_$num[$i] image-seg_$num2[$i]`;
  `echo image-$num[$i] image-@num2[$i] >> image-foler.list`;
}
