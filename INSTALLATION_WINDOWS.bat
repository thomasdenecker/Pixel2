docker pull tdenecker/pixel2_db
docker run --name PIXEL2_DB -d tdenecker/pixel2_db
docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" PIXEL2_DB > Database/ipDB.txt

docker pull tdenecker/pixel2_app
echo FOR /F "tokens=*" %%%%g IN ('docker ps -a -q') do (docker stop %%%%g) > Pixel2.bat
echo docker start PIXEL2_DB >> Pixel2.bat
echo docker run --rm --link  PIXEL2_DB:postgres -p 3838:3838 -v %CD%:/srv/shiny-server tdenecker/pixel2_app >> Pixel2.bat
