# jobworker

Haskell Servant + Websocket

## Build

```
stack build
```

Build Docker images

```
make all tag=latest
```

## Example

```
cd example
docker compose up -d
curl -s -XPOST localhost:8080/api/jobs/hello-world | jq
curl -s localhost:8080/api/workers | jq
curl -s localhost:8080/api/jobs | jq
```
