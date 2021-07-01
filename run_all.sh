# This script sets up all analyses reported in the glmeiv paper. It 1) creates
# a nextflow.config file in the current directory (which can be used to set several
# advanced options to simulatr) and 2) sets up to offsite (or data) directory
# structure.

# source LOCAL_GLMEIV_DATA_DIR and REMOTE_GLMEIV_DATA_DIR
source ~/.research_config

# 1. Create a nextflow config file. Create a "work" directory in
# the top-level glmeiv data directory.
if [ -f nextflow.config ]; then
  rm nextflow.config
fi
touch nextflow.config
echo workDir = \"$LOCAL_GLMEIV_DATA_DIR\work\" >> nextflow.config

# 2. Initialize the offsite directory structure
sim_dir=$LOCAL_GLMEIV_DATA_DIR"private/simulations"
remote_sim_dir=$REMOTE_GLMEIV_DATA_DIR"private/simulations"
mkdir -p $sim_dir $LOCAL_GLMEIV_DATA_DIR"public"

# 3. rclone sync remote to local
rclone sync -i $remote_sim_dir $sim_dir

# 4. Create the simulatr specifier objects
Rscript simulations/create_simulatr_spec_objects.R

# 5. Run the simulations
# i.
if [[ ! -f $sim_dir"/raw_result_1.rds" ]]
then
$SIMULATR -f $sim_dir"/sim_spec_1.rds" -r $sim_dir"/raw_result_1.rds"
fi

# 5. rclone copy results to remote
rclone copy $sim_dir $remote_sim_dir

# 6. Analyze the simulations
