#!/bin/sh

cd ~/ownCloud/Shared/current_season_data/Campbell_loggers

# create a backup of the current files in a compressed archive
tar cfa ~/Owncloud_backup_Agface/Campbell_logger_backup_`date +"%Y%m%d"`.tar.gz *

if [ -f  ~/Owncloud_backup_Agface/Campbell_logger_backup_`date +"%Y%m%d"`.tar.gz ]
then
  # remove the files from the local synced directory
  # will be populated again with latest files
#rm -r *
# only remove dat files
rm ./logger_data/*.dat
fi

echo "Waiting 120 sec for the Owncloud client to grab the latest files"
sleep 120

# copy the latest files to the Agface folder
cp -r ~/ownCloud/Shared/current_season_data/Campbell_loggers/logger_data ~/AgFace/2015/Campbell_logger/

# search replace SYS3 in the data file for SYS7
# by calling another script
cd ~/AgFace/2015/Campbell_logger/logger_data

~/AgFace/R_scripts/Campbell_sed_replace_SYS7.sh

# create a notification
notify-send -i /usr/share/icons/Adwaita/48x48/emotes/face-cool.png "Copy Campbell" "Latest files received"
exit
