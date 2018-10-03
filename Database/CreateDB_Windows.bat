docker stop PIXEL_DB

docker rm PIXEL_DB

docker build -t pixel_db .

docker tag pixel_db tdenecker/pixel_db

docker run --name PIXEL_DB -d tdenecker/pixel_db

docker start PIXEL_DB

docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" bPeaksDB > Database/ipDB.txt
