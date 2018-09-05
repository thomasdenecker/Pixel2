docker build -t pixel_db .

docker build -t pixel_db .

docker tag bpeaks_db tdenecker/pixel_db

docker run --name PIXEL_DB -d tdenecker/pixel_db

docker start PIXEL_DB
