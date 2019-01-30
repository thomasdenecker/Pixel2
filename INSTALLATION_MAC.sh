#!/bin/bash

BASEDIR=$(dirname "$0")
echo "$BASEDIR"

docker pull tdenecker/pixel2_app
docker pull tdenecker/pixel2_db
docker run --name PIXEL2_DB -d tdenecker/pixel2_db
docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" PIXEL2_DB > $BASEDIR/Database/ipDB.txt

echo '#!/bin/bash' > $BASEDIR/Pixel2.sh
echo docker stop PIXEL2_DB >> $BASEDIR/Pixel2.sh
echo docker start PIXEL2_DB >> $BASEDIR/Pixel2.sh
echo 'docker run -d --rm --link PIXEL2_DB:postgres -p 3838:3838 -v' $BASEDIR':/srv/shiny-server tdenecker/pixel2_app' >> $BASEDIR/Pixel2.sh

chmod +x $BASEDIR/Pixel2.sh
