PROJECT=yesodoro-reboot
ENVIRONMENT=Production
DEPLOY_DIR=/var/keter/incoming

include Makefile.local

EXE=dist/build/$(PROJECT)/$(PROJECT)
KETER=$(PROJECT).keter

$(KETER): dist config/postgresql-$(ENVIRONMENT).yml config/settings.yml config/keter.yaml
	./build-keter.sh $(ENVIRONMENT)

dist: $(EXE)
	strip $(EXE)

$(EXE):
	cabal-dev configure
	cabal-dev build

deploy: $(KETER)
	scp $(KETER) $(DEPLOY_HOST):$(DEPLOY_DIR)/

clean:
	cabal-dev clean

distclean: clean
	rm $(KETER)
