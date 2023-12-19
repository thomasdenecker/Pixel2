docker pull tdenecker/pixel2_db
docker run --name PIXEL2_DB -d tdenecker/pixel2_db
docker start PIXEL2_DB
docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" PIXEL2_DB > Database/ipDB.txt

docker pull tdenecker/pixel2_app
echo docker stop PIXEL2_DB >> Pixel2.ps1
echo docker start PIXEL2_DB >> Pixel2.ps1
echo docker run -d --rm --link  PIXEL2_DB:postgres -p 3838:3838 -v ${PWD}/srv/shiny-server tdenecker/pixel2_app >> Pixel2.ps1
