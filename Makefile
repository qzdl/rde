
PANDOC = pandoc
SED = sed
MAKEINFO = makeinfo

# NOTE: pipefail is not POSIX complaint

all: doc/rde.info

install:
	@echo some installation will happen here

home:
	RDE_TARGET=ixy-home \
	GUILE_LOAD_PATH=./ \
	guix home reconfigure ./rde/examples/abcdw/configs.scm --allow-downgrades

system:
	RDE_TARGET=ixy-system \
	GUILE_LOAD_PATH=./ \
	guix system reconfigure ./rde/examples/abcdw/configs.scm --allow-downgrades

check-channel:
	guix pull -C ./rde/examples/channels.tmpl

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

DRIVER=neato
GUILD=guild
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
