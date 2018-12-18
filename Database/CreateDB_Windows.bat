docker stop PIXEL2_DB

docker rm PIXEL2_DB

docker build -t pixel2_db .

docker tag pixel2_db tdenecker/pixel2_db

docker run --name PIXEL2_DB -d tdenecker/pixel2_db

docker start PIXEL2_DB

docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" PIXEL2_DB > ipDB.txt
