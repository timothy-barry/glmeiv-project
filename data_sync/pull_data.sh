source ~/.research_config
rclone sync -i ${REMOTE_GLMEIV_DATA_DIR}private ${LOCAL_GLMEIV_DATA_DIR}private
# rclone copy -i ${REMOTE_GLMEIV_DATA_DIR}public ${LOCAL_GLMEIV_DATA_DIR}public
