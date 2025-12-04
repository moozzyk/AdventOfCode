# How to run in a Docker container

Pull the Java Docker image:
```console
docker pull julia:latest
```

## One-off run
Spins a container, executes the command and shuts down the container.

Run from the 2025 directory

### Compile

Note update the `Day04` in the command below

Command:
```console
docker run -it --rm -v "$PWD":/opt/app -w /opt/app eclipse-temurin javac -d bin Day04/Problems.java utils/*.java
```

### Run

Command:
```console
docker run -it --rm -v "$PWD":/opt/app -w /opt/app eclipse-temurin java -cp bin Problems Day04/testdata.txt
```

Note update the `Day04` in the command below
