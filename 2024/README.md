# How to run in a Docker container

Pull the Julia Docker image: 
```console
docker pull julia:latest
```

## One-off run
Spins a container, executes the script and shuts down the container.

Command:
```console
docker run -it --rm -v "$PWD":/usr/myapp -w /usr/myapp julia julia script.jl arg1 arg2
```

## Continuous run
Keeps container running in the detached mode to avoid the overhead of spinning up the container. 

1. Create a container from the `julia` image and start it in the detached mode:
```console
docker run -it -d -v "$PWD":/usr/myapp -w /usr/myapp --name julia-container julia
```
  (If the container already exists use `docker start julia-container` to start it)

2. Execute the script using the following command: 
```console
docker exec -it julia-container julia script.jl arg1 arg2
``` 
3. If you want to stop the container execute: 
```console
docker stop julia-container
```
4. You can restart the container with:
```console
docker start julia-container
```
## What is the overhead of spinning the container?

### One-off run
```console
time docker run -it --rm -v "$PWD":/usr/myapp -w /usr/myapp julia julia hello_world.jl
Hello, World!
docker run -it --rm -v "$PWD":/usr/myapp -w /usr/myapp julia julia   0.01s user 0.01s system 5% cpu 0.492 total
```

### Continuous run
```console
time docker exec -it julia-container julia hello_world.jl
Hello, World!
docker exec -it julia-container julia hello_world.jl  0.01s user 0.01s system 7% cpu 0.236 total
```
