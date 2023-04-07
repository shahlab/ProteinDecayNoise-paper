#!/bin/bash

#BATCH --partition=main          # Partition (job queue)
#SBATCH --job-name=cln2-seg           # Assign an 8-character name to your job
#SBATCH --nodes=1                 # Number of nodes
#SBATCH --ntasks=1                # Number of tasks (usually = cores) on each node
#SBATCH --cpus-per-task=2        # Threads per process (or per core)
#SBATCH --mem=60GB                # Real memory required (MB)
#SBATCH --time=01:00:00           # Total run time limit (HH:MM:SS)
#SBATCH --output=slurm.%N.%j.out  # STDOUT output file
#SBATCH --error=slurm.%N.%j.err   # STDERR output file
#SBATCH --export=ALL              # Export you current env to the job env
#SBATCH --array=124               #assign values from 0 to total no. of images eg: 0:100
#SBATCH --mail-type=all
#SBATCH --mail-user=sukanya.das@rutgers.edu #change to your email ID associated with your highcomputing cluster

module purge
module use /projects/community/modulefiles/   #change to the path where your dependencies are located 
module load py-data-science-stack/5.1.0-kp807 
source activate tensorflow-1.8.0
date
python /home/sd882/yeast_spotter_copy/segmentation.py -i "/home/sd882/nikon-ti-e-images/nikon-ti-e-images/SD/2022/10-7-22-pup1/cln2/segmentation/image-$SLURM_ARRAY_TASK_ID/" -o "/home/sd882/nikon-ti-e-images/nikon-ti-e-images/SD/2022/10-7-22-pup1/cln2/segmentation/image-seg_$SLURM_ARRAY_TASK_ID/" #change the path to where your input images are before -i and where the outtputt folders are before -o 
date
hostname
