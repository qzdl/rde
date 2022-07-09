
PANDOC = pandoc
SED = sed
MAKEINFO = makeinfo

# NOTE: pipefail is not POSIX complaint

default: help

_rde:
	@echo "                    _/           "
	@echo "   _/  _/_/    _/_/_/    _/_/    "
	@echo "  _/_/      _/    _/  _/_/_/_/   "
	@echo " _/        _/    _/  _/          "
	@echo "_/          _/_/_/    _/_/_/     "
	@echo
	@echo "(c) Andrew Tropin 2020,2022"
	@echo
	@echo

.PHONY: help
help: _rde
	@echo "help:		print this message"
	@echo "all:		build info pages"
	@echo "home:		reload home"
	@echo "system:		reload system; run with 'sudo -E'"
	@echo "home-box:	reload home"
	@echo "iso		create a bootable system image"
	@echo "check:	      	"
	@echo "check-channel:	"
	@echo "check-channel:	"

.PHONY: all
all: doc/rde.info

.PHONY: install
install:
	@echo some installation will happen here

.PHONY: home
home:
	RDE_TARGET=ixy-home \
	GUILE_LOAD_PATH=./ \
	guix home reconfigure ./rde/examples/abcdw/configs.scm --allow-downgrades

.PHONY: system
system:
	RDE_TARGET=ixy-system \
	GUILE_LOAD_PATH=./ \
	guix system reconfigure ./rde/examples/abcdw/configs.scm --allow-downgrades

.PHONY: check-channel
check-channel:
	guix pull -C ./rde/examples/channels.tmpl

.PHONY: check
check: check-channel
	RDE_TARGET=ixy-home \
	./pre-inst-env guix home build \
	./gnu/home/examples/minimal.tmpl

# home-reconfigure-local:
# 	GUILE_LOAD_PATH=./ ../gnu/guix/pre-inst-env guix \
# 	home reconfigure ../rde/rde/config.scm

# TODO Rewrite to glakes
.PHONY: env
env:
	guix time-machine -C stale/guix-related/guix/channels-lock -- \
	environment --ad-hoc make

.PHONY: channels-pull
channels-pull:
	guix pull -C stale/guix-related/guix/channels-lock

.PHONY: channels-update-lock
channels-update-lock:
	guix time-machine -C stale/guix-related/guix/channels -- \
	describe -f channels > stale/guix-related/guix/channels-lock

.PHONY: iso
iso:
	guix time-machine -C stale/guix-related/guix/channels-lock -- \
	system -L ./ image -t iso9660 stale/guix-related/system/install.scm

.PHONY: doc/rde-tool-list.texi
doc/rde-tool-list.texi: doc/rde-tool-list.org
	$(PANDOC) doc/rde-tool-list.org -f org -t texinfo \
	-o doc/rde-tool-list.texi
	$(SED) -i '1,3d' doc/rde-tool-list.texi

.PHONY: doc/rde.info
doc/rde.info: doc/rde.texi
	$(MAKEINFO) -o doc/rde.info doc/rde.texi

.PHONY: doc/rde.html
doc/rde.html: doc/rde.texi
	$(MAKEINFO) --html --no-split \
	--css-ref=/assets/manual.css \
	-c "EXTRA_HEAD=<meta name=\"viewport\" \
content=\"width=device-width, initial-scale=1\" />" \
	-o doc/rde.html doc/rde.texi

.PHONY: doc/rde.pdf
doc/rde.pdf: doc/rde.texi
	$(MAKEINFO) --pdf -o doc/rde.pdf doc/rde.texi

DRIVER = neato
GUILD = guild
.PHONY: doc/graphs
doc/graphs:
	mkdir -p doc/graphs;
	for file in $$(find . -name '*.scm') ; do\
		[ ! -f $$file ] && continue;\
		df="doc/graphs/$$(basename $$file).dot";\
		echo rde doc $$df;\
		$(GUILD) use2dot $$file > $$df;\
		dp="$$df.png";\
		$(DRIVER) $$df -Tpng -o $$dp -Goverlap=false -Gsplines=true;\
	done\

.PHONY: clean
clean:
	rm -f doc/rde.html
	rm -f doc/rde.pdf
	rm -f doc/rde.info
	rm -f doc/rde-tool-list.texi
	rm -f doc/graphs/*
