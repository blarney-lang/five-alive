FiveAlive.v:
	cabal run

# Docker variables
USER=$(if $(shell id -u),$(shell id -u),9001)
GROUP=$(if $(shell id -g),$(shell id -g),1000)

# Build the docker image
.PHONY: build-docker
build-docker:
	(cd docker; docker build --build-arg UID=$(USER) --build-arg GID=$(GROUP) . --tag five-alive-ubuntu2204)

# Enter the docker image
.PHONY: shell
shell: build-docker
	docker run -it --shm-size 256m --hostname five-alive-ubuntu2204 -u $(USER) -v /home/$(shell whoami)/.ssh:/home/dev-user/.ssh  -v $(shell pwd):/workspace five-alive-ubuntu2204:latest /bin/bash

.PHONY: clean
clean:
	make -C software/hello clean
	make -C software/dhrystone clean
	make -C software/tests clean
	make -C sim clean
	make -C de10-pro-e clean
	cabal clean
	rm -f FiveAlive.v
