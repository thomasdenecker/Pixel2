#!/bin/bash

BASEDIR=$(dirname "$0")
echo "$BASEDIR"

docker pull tdenecker/pixel2_app
docker pull tdenecker/pixel_db
docker run --name PIXEL_DB -d tdenecker/pixel_db
docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" PIXEL_DB > $BASEDIR/Database/ipDB.txt

echo '#!/bin/bash' > $BASEDIR/Pixel2.sh
echo 'docker stop $(docker ps -a -q)' >> $BASEDIR/Pixel2.sh
echo docker start PIXEL_DB >> $BASEDIR/Pixel2.sh
echo 'docker run --rm --link PIXEL_DB:postgres -p 3838:3838 -v' $BASEDIR':/srv/shiny-server tdenecker/pixel2_app' >> $BASEDIR/Pixel2.sh

chmod +x $BASEDIR/Pixel2.sh
