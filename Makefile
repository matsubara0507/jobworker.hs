apps:
	stack --docker --local-bin-path=./bin install

server-image: apps
	docker build -t ghcr.io/matsubara0507/jobworker:${tag} . --build-arg local_bin_path=./bin -f image/server/Dockerfile

runner-image: apps
	docker build -t ghcr.io/matsubara0507/jobworker-runner:${tag} . --build-arg local_bin_path=./bin -f image/runner/Dockerfile

all: server-image runner-image
