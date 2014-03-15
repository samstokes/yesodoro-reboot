PROJECT=yesodoro-reboot
ENVIRONMENT=Production
DEPLOY_DIR=/var/keter/incoming
FIX=fix
CASPERJS=casperjs
KARMA=node_modules/karma/bin/karma
JSHINT=node_modules/.bin/jshint
ROOT_URL=http://localhost:3000/

-include Makefile.local

EXE=dist/build/$(PROJECT)/$(PROJECT)
KETER=$(PROJECT).keter

$(KETER): dist config/postgresql-$(ENVIRONMENT).yml config/settings.yml config/keter.yaml
	./build-keter.sh $(ENVIRONMENT)

dist: $(EXE)
	strip $(EXE)

$(EXE):
	cabal-dev configure
	cabal-dev build

test: test-unit test-smoke

test-unit:
	$(KARMA) start config/karma.conf.js

test-smoke:
	ROOT_URL=$(ROOT_URL) $(CASPERJS) test tests/casper

deploy: $(KETER)
	scp $(KETER) $(DEPLOY_HOST):$(DEPLOY_DIR)/

host:
	cd $(CHEF_KITCHEN) && $(FIX) node:$(DEPLOY_HOST)

clean:
	cabal-dev clean

distclean: clean
	rm $(KETER)
