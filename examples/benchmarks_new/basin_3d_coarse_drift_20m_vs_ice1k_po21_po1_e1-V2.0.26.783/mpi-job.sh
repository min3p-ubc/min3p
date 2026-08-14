#!/bin/bash
#SBATCH --account=rrg-min3p
#SBATCH --nodes=2                          # number of nodes, each with 40 procs
#SBATCH --ntasks-per-node=40               # without hyperthreading
#SBATCH --time=0-23:55                     # time (DD-HH:MM)
#SBATCH --job-name=basin-3d                # job name
#SBATCH --mail-user=danyang.su@gmail.com
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

module load intel/2019u4
module load intelmpi/2019u4
module load petsc/3.13.5

cd $SLURM_SUBMIT_DIR
mpirun -env I_MPI_EXTRA_FILESYSTEM=on -env I_MPI_EXTRA_FILESYSTEM_LIST=gpfs ../../min3p-hpc-mpi basin-3d -log_view -flow_ksp_gmres_restart 300 -flow_pc_type asm -flow_pc_factor_shift_type nonzero -flow_sub_pc_type ilu -flow_sub_pc_factor_shift_type nonzero -flow_ksp_max_it 1000 -react_ksp_gmres_restart 300 -react_pc_type asm -react_pc_factor_shift_type nonzero -react_sub_pc_type ilu -react_sub_pc_factor_shift_type nonzero -react_ksp_max_it 1000 -heat_ksp_gmres_restart 300 -heat_pc_type asm -heat_pc_factor_shift_type nonzero -heat_sub_pc_type ilu -heat_sub_pc_factor_shift_type nonzero -heat_ksp_max_it 1000
