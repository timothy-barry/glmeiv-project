source ~/.research_config
rclone copy -i ${LOCAL_GLMEIV_DATA_DIR}private ${REMOTE_GLMEIV_DATA_DIR}private 
rclone copy -i ${LOCAL_GLMEIV_DATA_DIR}public ${REMOTE_GLMEIV_DATA_DIR}public
