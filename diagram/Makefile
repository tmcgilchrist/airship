
.PHONY: build run run-quick

DEFAULT: run-quick

assets:
	mkdir assets

# TODO Find a hosted fixed version of this rather than hoping the current version is compatible
assets/coffee-script.js: assets
	curl -s http://coffeescript.org/extras/coffee-script.js > assets/coffee-script.js

assets/joint-0.8.1.css: assets
	curl -s https://cdnjs.cloudflare.com/ajax/libs/jointjs/0.8.1/joint.css > assets/joint-0.8.1.css

assets/joint.shapes.fsa-0.8.1.js: assets
	curl -s https://cdnjs.cloudflare.com/ajax/libs/jointjs/0.8.1/joint.shapes.fsa.js > assets/joint.shapes.fsa-0.8.1.js

http-decision-diagram/httpdd.fsm.html.coffee:
	git submodule update --init

api-pegjs/node_modules:
	# cosmogo is broken
	# npm install api-pegjs@0.4.23

	git submodule update --init

	cd api-pegjs; git submodule update --init
	# Runs the tests and may fail depending on your version of npm
	cd api-pegjs; npm install || true

httpdd.fsm.json: api-pegjs/node_modules httpdd.fsm.cosmogol
	cat httpdd.fsm.cosmogol | ./api-pegjs/node_modules/.bin/coffee api-pegjs/bin/parse.coffee 'core/ietf/draft-bortzmeyer-language-state-machines' 'state_machine' > httpdd.fsm.json

build: httpdd.fsm.json

run-quick: assets/coffee-script.js assets/joint-0.8.1.css assets/joint.shapes.fsa-0.8.1.js http-decision-diagram/httpdd.fsm.html.coffee
	python -m SimpleHTTPServer

run: build run-quick
