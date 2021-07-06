source ~/.research_config
rclone sync -i ${LOCAL_GLMEIV_DATA_DIR}private ${REMOTE_GLMEIV_DATA_DIR}private 
rclone sync -i ${LOCAL_GLMEIV_DATA_DIR}public ${REMOTE_GLMEIV_DATA_DIR}public
