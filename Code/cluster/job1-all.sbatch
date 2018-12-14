#!/bin/bash
#SBATCH --job-name=job1-all
#SBATCH --output=job1-all.out
#SBATCH --error=job1-all.err
#SBATCH --time=02:45:00
#SBATCH --partition=bigmem2
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --mem-per-cpu=60000

module load rstudio
cd /home/lunl/Research/Advertisement/Code/cluster
Rscript job1-all.R
module unload R