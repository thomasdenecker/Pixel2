# Pixel database


## Create a Pixel database docker

### Build an Pixel database image from a Dockerfile
```
docker build -t pixel_db .
```

### Change docker name (to push on DockerHub)
```
docker tag bpeaks_db tdenecker/pixel_db
```

### Run docker
```
docker run --name PIXEL_DB -d tdenecker/pixel_db
```

### Start Docker
```
docker start PIXEL_DB
```

## Connect to database with  (command line)
```
docker run -it --rm --link PIXEL_DB:postgres postgres psql -h postgres -U docker
```

The default password is docker. If you change this password, remember to make the change in the application code.
