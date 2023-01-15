DRONES_DIR = lib
INIT_FILES = "skip-init-file-build"  # a non-exist file path
EMACS_EXTRA = --eval "(setq borg-maketexi-filename-regexp nil)"  # do not generate *.texi files, to keep the submodules clean

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url git@github.com:emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD
