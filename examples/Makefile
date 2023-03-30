# profiles.mk provides guix version specified by rde/channels-lock.scm
# To rebuild channels-lock.scm use `make -B rde/channels-lock.scm`
include profiles.mk

# Also defined in .envrc to make proper guix version available project-wide
GUIX_PROFILE=target/profiles/guix
GUIX=./pre-inst-env ${GUIX_PROFILE}/bin/guix


RDE_LOCAL ?= qzdl
SRC_DIR=./src
_CONFIG_DIR=${SRC_DIR}/${RDE_LOCAL}
_CONFIG=${_CONFIG_DIR}/configs

CONFIGO=${_CONFIG}.org
CONFIGS=${_CONFIG}.scm
CONFIGE=${_CONFIG_DIR}/emacs.scm

PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

VERSION=latest

###
all: tangle inputs ixy/home/reconfigure ixy/system/reconfigure

home: tangle inputs ixy/home/reconfigure

system: tangle inputs ixy/system/reconfigure

inputs:
	git rev-parse HEAD
	guix hash -x ${CONFIGO}
	guix hash -x ${CONFIGS}
	guix hash -x ${CONFIGE}

tangle:
	emacs -Q --batch \
	  --eval '(setq python-indent-guess-indent-offset nil)' \
	  -f package-initialize ${CONFIGO} \
	  -f org-babel-tangle
###

ixy/home/build: guix
	RDE_TARGET=ixy-home ${GUIX} home \
		build ${CONFIGS}

ixy/home/reconfigure: guix
	RDE_TARGET=ixy-home ${GUIX} home \
		reconfigure ${CONFIGS}

ixy/system/reconfigure: # guix
	RDE_TARGET=ixy-system ${GUIX} system \
		reconfigure ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

ixy/system/init: guix
	RDE_TARGET=ixy-system ${GUIX} system \
		init ${CONFIGS} ${ROOT_MOUNT_POINT}

target:
	mkdir -p target

live/image/build: guix
	RDE_TARGET=live-system ${GUIX} system \
		image --image-type=iso9660 \
		${CONFIGS}

target/rde-live.iso: guix target
	RDE_TARGET=live-system ${GUIX} system \
		image --image-type=iso9660 \
		${CONFIGS} -r target/rde-live-tmp.iso
	mv -f target/rde-live-tmp.iso target/rde-live.iso

target/release:
	mkdir -p target/release

# TODO: Prevent is rebuilds.
release/rde-live-x86_64: target/rde-live.iso target/release
	cp -df $< target/release/rde-live-${VERSION}-x86_64.iso
	gpg -ab target/release/rde-live-${VERSION}-x86_64.iso

minimal-emacs: guix
	${GUIX} shell --pure -Df ./src/abcdw/minimal-emacs.scm \
	-E '.*GTK.*|.*XDG.*|.*DISPLAY.*' \
	--rebuild-cache -- emacs -q \
	--eval "(load \"~/.config/emacs/early-init.el\")"
	#--eval "(require 'feature-loader-portable)"

minimal/home/build: guix
	${GUIX} home build ./src/abcdw/minimal.scm

clean-target:
	rm -rf ./target

clean: clean-target
