PROJECT=yesodoro-reboot
ENVIRONMENT=Production
DEPLOY_DIR=/var/keter/incoming
FIX=fix
CASPERJS=casperjs --ssl-protocol=tlsv1
KARMA=node_modules/karma/bin/karma
JSHINT=node_modules/.bin/jshint
ROOT_URL=http://localhost:3000/
TESTER_EMAIL=nonexistent@example.com
TESTER_PASSWORD=wibble

-include Makefile.local

EXE=dist/build/$(PROJECT)/$(PROJECT)
CREATE_TEST_ACCOUNT_EXE=dist/build/$(PROJECT)-create-test-account/$(PROJECT)-create-test-account
KETER=$(PROJECT).keter

JAVASCRIPTS=static/js

$(KETER): dist config/postgresql-$(ENVIRONMENT).yml config/settings.yml config/keter.yaml
	./build-keter.sh $(ENVIRONMENT)

dist: $(EXE)
	strip $(EXE)

$(EXE):
	cabal-dev configure
	cabal-dev build

check: check-js

check-js:
	$(JSHINT) $(JAVASCRIPTS)

test: test-unit test-smoke

test-unit:
	$(KARMA) start config/karma.conf.js

test-smoke:
	ROOT_URL=$(ROOT_URL) TESTER_EMAIL=$(TESTER_EMAIL) TESTER_PASSWORD=$(TESTER_PASSWORD) $(CASPERJS) test tests/casper

setup-test-account: $(CREATE_TEST_ACCOUNT_EXE)
	$(CREATE_TEST_ACCOUNT_EXE) Testing $(TESTER_EMAIL) $(TESTER_PASSWORD)

deploy: $(KETER)
	scp $(KETER) $(DEPLOY_HOST):$(DEPLOY_DIR)/

host:
	cd $(CHEF_KITCHEN) && $(FIX) node:$(DEPLOY_HOST)

clean:
	cabal-dev clean

distclean: clean
	rm $(KETER)
