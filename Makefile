DRONES_DIR = lib
INIT_FILES = "skip-init-file-build"  # a non-exist file path

EMACS = env LSP_USE_PLISTS=true emacs
EMACS_EXTRA = --eval "(setq borg-maketexi-filename-regexp nil)"  # do not generate *.texi files, to keep the submodules clean

-include $(DRONES_DIR)/borg/borg.mk
