#!/bin/bash
#SBATCH --job-name=job1-spottv
#SBATCH --output=job1-spottv.out
#SBATCH --error=job1-spottv.err
#SBATCH --time=00:30:00
#SBATCH --partition=bigmem2
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --mem-per-cpu=60000

module load rstudio
cd /home/lunl/Research/Advertisement/Code/cluster
Rscript job1-spottv.R
module unload R