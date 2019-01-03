$(eval squid_ip := $(shell docker inspect squid|jq -r '.[].NetworkSettings.IPAddress'))

docker:
	docker build --build-arg squid=$(squid_ip) --rm=true -t confluence .
	docker tag datadog jaimef/confluence

push:
	docker push jaimef/confluence
