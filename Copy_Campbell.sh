#!/bin/sh

cd ~/ownCloud/Shared/current_season_data/Campbell_loggers

# create a backup of the current files in a compressed archive
tar cfa ~/Owncloud_backup_Agface/Campbell_logger_backup_`date +"%Y%m%d"`.tar.gz *

if [ -f  ~/Owncloud_backup_Agface/Campbell_logger_backup_`date +"%Y%m%d"`.tar.gz ]
then
  # remove the files from the local synced directory
  # will be populated again with latest files
rm -r *
fi

echo "Waiting 30 sec for the Owncloud client to grab the latest files"
sleep 30

# copy the latest files to the Agface folder
cp -r ~/ownCloud/Shared/current_season_data/Campbell_loggers/logger_data ~/AgFace/2014/Campbell_logger/

exit