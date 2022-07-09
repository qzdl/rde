CONFIG_FILE = ./configs.scm
HOSTS = ixy
GLP = ../../../

DOWNGRADE = --verbosity=9 --fallback --allow-downgrades

# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
targets-home = $(addsuffix -home, $(HOSTS))
targets-system = $(addsuffix -system, $(HOSTS))

.PHONY: %-home-build %-home-reconfigure
%-home-build:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-home \
	guix home build $(CONFIG_FILE) $(ARGS)

%-home-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-home \
	guix home reconfigure $(CONFIG_FILE) $(ARGS)


.PHONY: %-system-build %-system-reconfigure
%-system-build:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-system \
	guix system build $(CONFIG_FILE) $(ARGS)

%-system-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-system \
	guix system reconfigure $(CONFIG_FILE) $(ARGS)


$(targets-home): %-home-reconfigure
$(targets-system): %-system-reconfigure

# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
.PHONY: help

help:
	$(info The targets are '$(targets-home)' and '$(targets-system)')
