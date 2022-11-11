GUILE_LOAD_PATH=../../..

minimal-emacs:
	guix shell --pure -Df minimal-emacs.scm \
	-E '.*GTK.*|.*XDG.*|.*DISPLAY.*' \
	--rebuild-cache -- emacs -q
	#--eval "(require 'feature-loader-portable)"


targets-home = $(addsuffix -home, $(HOSTS))
targets-system = $(addsuffix -system, $(HOSTS))

all: tangle inputs %-home-reconfigure %-system-reconfigure

home: tangle inputs %-home-reconfigure

system: tangle inputs %-system-reconfigure

inputs:
	git rev-parse HEAD
	guix hash -x ./configs.scm
	guix hash -x ./emacs.scm

tangle:
	emacs -Q --batch -f package-initialize ./configs.org -f org-babel-tangle

# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
%-home-build: inputs
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-home \
	guix home build $(CONFIG_FILE)

%-home-reconfigure: inputs
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-home \
	guix home reconfigure $(CONFIG_FILE)

%-system-build: inputs
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-system \
	guix system build $(CONFIG_FILE)

%-system-reconfigure: inputs
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-system \
	guix system reconfigure $(CONFIG_FILE)

$(targets-home): %-home-reconfigure
$(targets-system): %-system-reconfigure


.PHONY: help

help:
	$(info help expected here)
