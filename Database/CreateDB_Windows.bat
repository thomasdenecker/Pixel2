docker stop PIXEL_DB

docker rm PIXEL_DB

docker build -t pixel2_db .

docker tag pixel_db tdenecker/pixel2_db

docker run --name PIXEL_DB -d tdenecker/pixel2_db

docker start PIXEL2_DB

docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" PIXEL2_DB > ipDB.txt
