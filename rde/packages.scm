;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages base)

  #:use-module (gnu packages man)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages python)
  #:use-module (srfi srfi-1)

  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%rde-patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/rde/packages.scm")))
        %load-path))

(define %rde-patch-path
  (make-parameter
   (append
    (list (string-append %channel-root "rde/packages/patches"))
    (%patch-path))))

(define-public rofi-wayland
  (package
   (inherit rofi)
   (name "rofi-wayland")
   (version "1.7.2+wayland1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/lbonn/rofi"
                                "/releases/download/"
                                version "/rofi-" version ".tar.xz"))
            (sha256
             (base32
              "1smrxjq693z48c7n5pcfrvb0m0vsn6pxn7qpn8bm68j942n8rg3x"))))
   (build-system meson-build-system)
   (arguments
    (substitute-keyword-arguments (package-arguments rofi)
      ((#:configure-flags flags '())
       #~(list "-Dxcb=disabled"))))
    (inputs
     (list cairo
           glib
           libjpeg-turbo
           librsvg
           libxkbcommon
           wayland-latest
           wayland-protocols-latest
           pango
           startup-notification))
    (description "Rofi is a minimalist application launcher.  It memorizes which
applications you regularly use and also allows you to search for an application
by name.

This is a fork with added support for Wayland via layer shell protocol.")))

(define-public wtype
  (package
   (name "wtype")
   (version "0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/atx/wtype"
                                "/archive/refs/tags/v" version ".tar.gz"))
            (sha256
             (base32
              "1ya6hxmgmsmxsy3yzssq4q2xm7lkfc253g44p0lnwslbh9npi4fs"))))
   (build-system meson-build-system)
   (inputs (list libxkbcommon wayland pkg-config))
   (home-page "https://github.com/atx/wtype")
   (synopsis "xdotool type for wayland")
   (description "wtype is a Wayland tool that allows you to simulate
keyboard input like xdotool type for X11.")
   (license license:gpl3+)))

(use-modules (guix build-system go)
             (gnu packages golang)
             (gnu packages syncthing))

(define-public go-gopkg-in-alecthomas-kingpin-v2
  (package
    (name "go-gopkg-in-alecthomas-kingpin-v2")
    (version "2.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/alecthomas/kingpin.v2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0mndnv3hdngr3bxp7yxfd47cas4prv98sqw534mx7vp38gd88n5r"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github-com-alecthomas-template go-github-com-alecthomas-units
           go-github-com-stretchr-testify))
    (arguments
      '(#:import-path
        "gopkg.in/alecthomas/kingpin.v2"
        #:unpack-path
        "gopkg.in/alecthomas/kingpin.v2"
        #:phases %standard-phases))
    (home-page "https://gopkg.in/alecthomas/kingpin.v2")
    (synopsis "Kingpin - A Go (golang) command line and flag parser")
    (description "Package kingpin provides command line interfaces like this:")
    (license license:expat)))

(define-public clipman
  (package
    (name "clipman")
    (version "1.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yory8/clipman")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0b9kvj0dif4221dy6c1npknhhjxvbc4kygzhwxjirpwjws0yv6v9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/yory8/clipman"))
    (propagated-inputs
     (list
      go-gopkg-in-alecthomas-kingpin-v2
      go-github-com-kballard-go-shellquote
      go-github-com-alecthomas-units
      go-github-com-alecthomas-template))
    (home-page "https://github.com/yory8/clipman")
    (synopsis "Clipman")
    (description "GPL v3.0 2019- (C) yory8 <yory8@users.noreply.github.com>")
    (license license:gpl3)))

(define-public rde
  (package
    (name "rde")
    (version "0.1.0")
    (home-page "https://trop.in/rde")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://git.sr.ht/~abcdw/rde")
                          (commit "0d72e180b7eb92ea5a23d8dd481bde93b6ec252c")))
      (sha256
       (base32
        "0vcsgbziv6cm4b4sccllsg67anpxg0q9mm3d80nms60ng6ld3i6b"))
      (file-name (string-append "rde-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnu-make texinfo))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (add-after 'install 'install-info
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (info (string-append out "/share/info")))
                (install-file "doc/rde.info" info)))))))
    (synopsis "Developers and power user friendly GNU/Linux distribution")
    (description "The GNU/Linux distribution, a set of tools for managing
development environments, home environments, and operating systems, a set of
predefined configurations, practices and workflows.")
    (license license:gpl3+)))

;; (define-public rde-latest
;;   (package
;;     (inherit rde)
;;     (source
;;      (local-file (dirname (dirname (current-filename))) #:recursive? #t))))

(use-modules
  (guix packages)
  (guix download)
  (guix git-download)
  (guix build-system emacs)
  (gnu packages emacs-xyz)
  ((guix licenses) #:prefix license:))

(define-public emacs-consult-recoll
  (package
   (name "emacs-consult-recoll")
   (version "0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/jao/consult-recoll")
           (commit "42dea1d40fedf7894e2515b4566a783b7b85486a")))
     (sha256
      (base32 "0nzch4x58vgvmcjr6p622lkzms2gvjfdgpvi6bbj5qdzkln5q23a"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-consult" ,emacs-consult)))
   (home-page "https://codeberg.org/jao/consult-recoll")
   (synopsis "A consulting-read interface for recoll")
   (description
    "A consulting-read interface for recoll")
   (license license:gpl3+)))

(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system emacs)
 (gnu packages emacs-xyz)
 ((guix licenses) #:prefix license:))

(define-public emacs-consult-eglot
  (package
   (name "emacs-consult-eglot")
   (version "20210905.1830")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mohkale/consult-eglot.git")
           (commit "f93c571dc392a8b11d35541bffde30bd9f411d30")))
     (sha256
      (base32 "1jqg6sg6iaqxpfn7symiy221mg9sn4y1rn0l1rw9rj9xmcnng7s0"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-eglot" ,emacs-eglot) ("emacs-consult" ,emacs-consult)))
   (home-page "https://github.com/mohkale/consult-eglot")
   (synopsis "A consulting-read interface for eglot")
   (description
    "Query workspace symbol from eglot using consult.

This package provides a single command `consult-eglot-symbols' that uses the
lsp workspace/symbol procedure to get a list of symbols exposed in the current
workspace. This differs from the default document/symbols call, that eglot
exposes through imenu, in that it can present symbols from multiple open files
or even files not indirectly loaded by an open file but still used by your
project.

This code was partially adapted from the excellent consult-lsp package.")
   (license license:expat)))

(define-public emacs-ob-restclient-latest
  (let ((commit "f81f2f4f3fe6882947b8547ccd570f540106ed4d"))
    (package
      (name "emacs-ob-restclient")
      (version (git-version "0.02" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alf/ob-restclient.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16byn85gsl99k818686jp3r4ipjcwdyq3nilmb32g4hgg0dlgaij"))))
      (propagated-inputs
       (list emacs-restclient))
      (build-system emacs-build-system)
      (home-page "https://github.com/alf/ob-restclient.el")
      (synopsis "Org-babel functionality for @code{restclient-mode}")
      (description
       "This package integrates @code{restclient-mode} with Org.")
      (license license:gpl3+))))

(define-public emacs-modus-themes-latest
  (package
   (inherit emacs-modus-themes)
   (name "emacs-modus-themes")
   (version "2.0.0-patch")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/protesilaos/modus-themes")
           (commit "4f177f036b30dd6c1f2e6c5342f2b87e034dc97e")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "15zxab2wg97ldy85wv2sx7j31z0cg0h28vm9yjnp3b7vl7sbzf33"))
     (patches (search-patches
               "0001-DRAFT-Add-tentative-support-for-ement.el.patch"))))))

(define-public emacs-justify-kp
 (let ((commit "385e6b8b909ae0f570f30101cec3677e21c9e0a0"))
  (package
   (name "emacs-justify-kp")
   (version "20171119")
   (home-page "https://github.com/qzdl/justify-kp")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13fylx4mvw7cgzd2mq060x43b1x7g5vdf16jm49c31f6b3jj1qi0"))))
   (build-system emacs-build-system)
   (inputs (list emacs-dash emacs-s))
   (synopsis "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (description
    "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (license license:gpl3+))))

(define-public tessen
  (package
    (name "tessen")
    (version "1.3.1")
    (home-page "https://github.com/ayushnix/tessen")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archives/refs/tags/v" version ".tar.gz"))
       (sha256
        (base32 "07ddb5himj4c9ijdibjy25dshil3k5nqd7869z9rkldzh97zh29f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check))))
    (inputs (list password-store
                  wl-clipboard
                  libnotify
                  pass-otp
                  xdg-utils
                  scdoc))
    (synopsis
     "An interactive menu to autotype and copy password-store data ")
    (description
     "tessen is a bash script that can use any wayland native
dmenu-like backend as an interface for auto-typing and copying
password-store data, known to work with bemenu, fuzzel, rofi, wofi")
    (license license:gpl3+)))

(use-modules (gnu packages libffi)
             (gnu packages xml)
             (gnu packages docbook))
(define-public wayland-latest
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/wayland/wayland/-/archive/"
                                  version "/wayland-" version ".tar.bz2"))
              (sha256
               (base32
                "0r0zjjqfcb6rykgcgy4w84g7jgfr400jwh4g0wh4d7g14r71qga7"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:parallel-tests? #f
        #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* (find-files "." "\\.xml$")
                 (("http://www.oasis-open.org/docbook/xml/4\\.5/")
                  (string-append (assoc-ref (or native-inputs inputs)
                                            "docbook-xml")
                                 "/xml/dtd/docbook/"))
                 (("http://www.oasis-open.org/docbook/xml/4\\.2/")
                  (string-append (assoc-ref (or native-inputs inputs)
                                            "docbook-xml-4.2")
                                 "/xml/dtd/docbook/"))))))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     `(("docbook-xml-4.2" ,docbook-xml-4.2)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xmlto" ,xmlto)
       ("xsltproc" ,libxslt)
       ,@(if (%current-target-system)
             `(("pkg-config-for-build" ,pkg-config-for-build)
               ("wayland" ,this-package)) ; for wayland-scanner
             '())))
    (inputs
     (list expat libxml2))           ; for XML_CATALOG_FILES
    (propagated-inputs
     (list libffi))
    (home-page "https://wayland.freedesktop.org/")
    (synopsis "Core Wayland window system code and protocol")
    (description "Wayland is a project to define a protocol for a compositor to
talk to its clients as well as a library implementation of the protocol.  The
compositor can be a standalone display server running on Linux kernel
modesetting and evdev input devices, an X application, or a wayland client
itself.  The clients can be traditional applications, X servers (rootless or
fullscreen) or other display servers.")
    (license license:expat)))

(use-modules (gnu packages wm)
             (gnu packages gl)
             (gnu packages xorg))
;; (define-public eglexternalplatform
;;   (package
;;     (name "eglexternalplatform")
;;     (version "1.1")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri
;;         (git-reference
;;          (url "https://github.com/NVIDIA/eglexternalplatform")
;;          (commit version)))
;;        (file-name
;;         (git-file-name name version))
;;        (sha256
;;         (base32 "0lr5s2xa1zn220ghmbsiwgmx77l156wk54c7hybia0xpr9yr2nhb"))))
;;     (build-system copy-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (add-after 'unpack 'patch-pkgconfig
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (substitute* "eglexternalplatform.pc"
;;                (("/usr")
;;                 (assoc-ref outputs "out")))))
;;          (add-after 'install 'revise
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (let* ((out (assoc-ref outputs "out")))
;;                (mkdir-p (string-append out "/include/EGL"))
;;                (rename-file
;;                 (string-append out "/interface")
;;                 (string-append out "/include/EGL"))
;;                (mkdir-p (string-append out "/share/pkgconfig"))
;;                (rename-file
;;                 (string-append out "/eglexternalplatform.pc")
;;                 (string-append out "/share/pkgconfig/eglexternalplatform.pc"))
;;                (for-each delete-file-recursively
;;                          (list
;;                           (string-append out "/samples")
;;                           (string-append out "/COPYING")
;;                           (string-append out "/README.md")))))))))
;;     (synopsis "EGL External Platform interface")
;;     (description "EGLExternalPlatform is an specification of the EGL External
;; Platform interface for writing EGL platforms and their interactions with modern
;; window systems on top of existing low-level EGL platform implementations.  This
;; keeps window system implementation specifics out of EGL drivers by using
;; application-facing EGL functions.")
;;     (home-page "https://github.com/NVIDIA/eglexternalplatform")
;;     (license license:expat)))

(define-public egl-wayland-latest
  (package
    (name "egl-wayland")
    (version "1.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/egl-wayland")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iz86cpc4v7izckrcslllnw0vvvgsxg1sr65yb8s9d0f8xa8djdd"))))
    (build-system meson-build-system)
    (native-inputs
     (list libglvnd ;needed for headers
           mesa-headers pkg-config))
    (inputs
     (list mesa wayland-latest wayland-protocols-latest))
    (propagated-inputs
     (list eglexternalplatform))
    (synopsis "EGLStream-based Wayland external platform")
    (description "EGL-Wayland is an implementation of a EGL External Platform
library to add client-side Wayland support to EGL on top of EGLDevice and
EGLStream families of extensions.")
    (home-page "https://github.com/NVIDIA/egl-wayland")
    (license license:expat)))

(define-public wlroots-latest
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xwayland/server.c"
               (("Xwayland") (string-append (assoc-ref inputs
                                                       "xorg-server-xwayland")
                                            "/bin/Xwayland")))
             #t)))))
    (propagated-inputs
     `(;; As required by wlroots.pc.
           ("eudev" ,eudev)
           ("libinput" ,libinput)
           ("xkbcommon" ,libxkbcommon)
           ;; ("mesa" ,mesa-latest)
           ("mesa" ,mesa)
           ("pixman" ,pixman)
           ("seatd" ,seatd)
           ("wayland" ,wayland-latest)
           ("egl-wayland" ,egl-wayland-latest)
           ("libglvnd" ,libglvnd)
           ("libdrm" ,libdrm-latest)
           ("wayland-protcols" ,wayland-protocols-latest)
           ("xcb-util-errors" ,xcb-util-errors)
           ("xcb-util-wm" ,xcb-util-wm)
           ("xorg-server-xwayland" ,xorg-server-xwayland)))
    (native-inputs
     (list pkg-config wayland-latest))
    (home-page "https://github.com/swaywm/wlroots")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))

(use-modules (ice-9 match))
(define-public libdrm-latest
  (package
    (name "libdrm")
    (version "2.4.109")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "09kzrdsd14zr0i3izvi5mck4vqccl3c9hr84r9i4is0zikh554v2"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              '("-Dexynos=true"
                "-Domap=true"
                "-Detnaviv=true"
                "-Dtegra=true"
                "-Dfreedreno-kgsl=true"))
             (_ '())))

       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "meson" "test" "--timeout-multiplier" "5")))))))
    (propagated-inputs
     (list libpciaccess))
    (native-inputs
     (list pkg-config))
    (home-page "https://dri.freedesktop.org/wiki/")
    (synopsis "Direct rendering userspace library")
    (description "The Direct Rendering Infrastructure, also known as the DRI,
is a framework for allowing direct access to graphics hardware under the
X Window System in a safe and efficient manner.  It includes changes to the
X server, to several client libraries, and to the kernel (DRM, Direct
Rendering Manager).  The most important use for the DRI is to create fast
OpenGL implementations providing hardware acceleration for Mesa.
Several 3D accelerated drivers have been written to the DRI specification,
including drivers for chipsets produced by 3DFX, AMD (formerly ATI), Intel
and Matrox.")
    (license license:x11)))

(use-modules (gnu packages video)
             (gnu packages vulkan)
             (gnu packages elf)
             (gnu packages gl)
             (gnu packages llvm)
             (gnu packages bison)
             (gnu packages flex)
             (gnu packages gettext)
             (gnu packages python-xyz))

(define libva-without-mesa
  ;; Delay to work around circular import problem.
  (delay
    (package
      (inherit libva)
      (name "libva-without-mesa")
      (inputs `(,@(fold alist-delete (package-inputs libva)
                        '("mesa" "wayland"))))
      (arguments
       (strip-keyword-arguments
        '(#:make-flags)
        (substitute-keyword-arguments (package-arguments libva)
          ((#:configure-flags flags)
           '(list "--disable-glx" "--disable-egl"))))))))

(define-public mesa-latest
  (package
    (name "mesa")
    (version "21.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (list (string-append "https://mesa.freedesktop.org/archive/"
                                  "mesa-" version ".tar.xz")
                   (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                  "mesa-" version ".tar.xz")
                   (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                  version "/mesa-" version ".tar.xz")))
        (sha256
         (base32
          "08c118j440xpfbjjxmwzm6dfnv4y35q540mmzkchhpbwx89lczxd"   ; 21.3.3
          ;;"1g96y59bw10ml8h4jl259g41jdmf5ww3jbwqpz1sprq7hgxvmrz2" ; 21.3.2
          ))
        (patches
         (search-patches "mesa-skip-tests.patch"))))
    (build-system meson-build-system)
    (propagated-inputs
      (list ;; The following are in the Requires.private field of gl.pc.
            libdrm-latest
            libvdpau
            libglvnd
            libx11
            libxdamage
            libxfixes
            libxshmfence
            libxxf86vm
            xorgproto))
    (inputs
     `(("expat" ,expat)
       ;;
       ("gstreamer" ,(@ (gnu packages gstreamer) gstreamer))
       ;;
        ("libelf" ,elfutils)  ;required for r600 when using llvm
        ("libva" ,(force libva-without-mesa))
        ("libxml2" ,libxml2)
        ("libxrandr" ,libxrandr)
        ("libxvmc" ,libxvmc)
        ,@(match (%current-system)
            ((or "x86_64-linux" "i686-linux" "powerpc64le-linux" "aarch64-linux"
                 "powerpc-linux" "riscv64-linux")
             ;; Note: update the 'clang' input of mesa-opencl when bumping this.
             `(("llvm" ,llvm-11)))
            (_
             `()))
        ("wayland" ,wayland-latest)
        ("wayland-protocols" ,wayland-protocols-latest)))
    (native-inputs
      `(("bison" ,bison)
        ("flex" ,flex)
        ("gettext" ,gettext-minimal)
        ,@(match (%current-system)
            ((or "x86_64-linux" "i686-linux" "powerpc64le-linux" "aarch64-linux"
                 "powerpc-linux" "riscv64-linux")
             `(("glslang" ,glslang)))
            (_
             `()))
        ("pkg-config" ,pkg-config)
        ("python" ,python-wrapper)
        ("python-libxml2", python-libxml2) ;for OpenGL ES 1.1 and 2.0 support
        ("python-mako" ,python-mako)
        ("which" ,(@ (gnu packages base) which))))
    (outputs '("out" "bin"))
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              ;; TODO: Fix svga driver for non-Intel architectures.
              '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,panfrost,r300,r600,swrast,tegra,v3d,vc4,virgl"))
             ((or "powerpc64le-linux" "powerpc-linux" "riscv64-linux")
              '("-Dgallium-drivers=nouveau,r300,r600,radeonsi,swrast,virgl"))
             (_
              '("-Dgallium-drivers=iris,nouveau,r300,r600,radeonsi,svga,swrast,virgl")))
         ;; Enable various optional features.  TODO: opencl requires libclc,
         ;; omx requires libomxil-bellagio
         "-Dplatforms=x11,wayland"
         "-Dglx=dri"        ;Thread Local Storage, improves performance
         ;; "-Dopencl=true"
         ;; "-Domx=true"
         "-Dosmesa=true"
         "-Dgallium-xa=enabled"

         ;; features required by wayland
         "-Dgles2=enabled"
         "-Dgbm=enabled"
         "-Dshared-glapi=enabled"

         "-Dglvnd=true"

         ;; Explicitly enable Vulkan on some architectures.
         ,@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              '("-Dvulkan-drivers=intel,amd"))
             ((or "powerpc64le-linux" "powerpc-linux")
              '("-Dvulkan-drivers=amd,swrast"))
             ("aarch64-linux"
              '("-Dvulkan-drivers=freedreno,amd,broadcom,swrast"))
             ("riscv64-linux"
              '("-Dvulkan-drivers=amd,swrast"))
             (_
              '("-Dvulkan-drivers=auto")))

         ;; Enable the Vulkan overlay layer on architectures using llvm.
         ,@(match (%current-system)
             ((or "x86_64-linux" "i686-linux" "powerpc64le-linux" "aarch64-linux"
                  "powerpc-linux" "riscv64-linux")
              '("-Dvulkan-layers=device-select,overlay"))
             (_
              '()))

         ;; Also enable the tests.
         "-Dbuild-tests=true"

         ;; on non-intel systems, drop i915 and i965
         ;; from the default dri drivers
         ,@(match (%current-system)
             ((or "x86_64-linux" "i686-linux")
              '("-Ddri-drivers=i915,i965,nouveau,r200,r100"
                "-Dllvm=enabled"))      ; default is x86/x86_64 only
             ((or "powerpc64le-linux" "aarch64-linux" "powerpc-linux" "riscv64-linux")
              '("-Ddri-drivers=nouveau,r200,r100"
                "-Dllvm=enabled"))
             (_
              '("-Ddri-drivers=nouveau,r200,r100"))))

       ;; XXX: 'debugoptimized' causes LTO link failures on some drivers.  The
       ;; documentation recommends using 'release' for performance anyway.
       #:build-type "release"

       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build utils)
                  (guix build meson-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; Disable the intel vulkan (anv_state_pool) tests, as they may
             ;; fail in a nondeterministic fashion (see:
             ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/5446).
             (substitute* "src/intel/vulkan/meson.build"
               (("if with_tests")
                "if false"))
             ,@(match (%current-system)
                 ("riscv64-linux"
                  ;; According to the test logs the llvm JIT is not designed
                  ;; for this architecture and the llvmpipe tests all segfault.
                  ;; The same is true for mesa:gallium / osmesa-render.
                  `((substitute* '("src/gallium/drivers/llvmpipe/meson.build"
                                   "src/gallium/targets/osmesa/meson.build")
                      (("if with_tests") "if false"))))
                 ("powerpc64le-linux"
                  ;; Disable some of the llvmpipe tests.
                  `((substitute* "src/gallium/drivers/llvmpipe/lp_test_arit.c"
                      (("0\\.5, ") ""))))
                 ("powerpc-linux"
                  ;; There are some tests which fail specifically on powerpc.
                  `((substitute* '(;; LLVM ERROR: Relocation type not implemented yet!
                                   "src/gallium/drivers/llvmpipe/meson.build"
                                   ;; This is probably a big-endian test failure.
                                   "src/gallium/targets/osmesa/meson.build")
                      (("if with_tests") "if not with_tests"))
                    (substitute* "src/util/tests/format/meson.build"
                      ;; This is definately an endian-ness test failure.
                      (("'u_format_test', ") ""))
                    ;; It is only this portion of the test which fails.
                    (substitute* "src/mesa/main/tests/meson.build"
                      ((".*mesa_formats.*") ""))
                    ;; This test times out and receives SIGTERM.
                    (substitute* "src/amd/common/meson.build"
                      (("and not with_platform_windows") "and with_platform_windows"))))
                 ("i686-linux"
                  ;; Disable new test from Mesa 19 that fails on i686.  Upstream
                  ;; report: <https://bugs.freedesktop.org/show_bug.cgi?id=110612>.
                  `((substitute* "src/util/tests/format/meson.build"
                      (("'u_format_test',") ""))))
                 ("aarch64-linux"
                  ;; The ir3_disasm test segfaults.
                  ;; The simplest way to skip it is to run a different test instead.
                  `((substitute* "src/freedreno/ir3/meson.build"
                      (("disasm\\.c'") "delay.c',\n    link_args: ld_args_build_id"))))
                 (_
                  '((display "No tests to disable on this architecture.\n"))))))
         (add-before 'configure 'fix-dlopen-libnames
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Remain agnostic to .so.X.Y.Z versions while doing
               ;; the substitutions so we're future-safe.
               (substitute* "src/glx/meson.build"
                 (("-DGL_LIB_NAME=\"lib@0@\\.so\\.@1@\"")
                  (string-append "-DGL_LIB_NAME=\"" out
                                 "/lib/lib@0@.so.@1@\"")))
               (substitute* "src/gbm/backends/dri/gbm_dri.c"
                 (("\"libglapi\\.so")
                  (string-append "\"" out "/lib/libglapi.so")))
               (substitute* "src/gbm/main/backend.c"
                 ;; No need to patch the gbm_gallium_drm.so reference;
                 ;; it's never installed since Mesa removed its
                 ;; egl_gallium support.
                 (("\"gbm_dri\\.so")
                  (string-append "\"" out "/lib/dri/gbm_dri.so"))))))
         (add-after 'install 'split-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (assoc-ref outputs "bin")))
               ;; Not all architectures have the Vulkan overlay control script.
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file (string-append out "/bin/.empty")
                 (const #t))
               (copy-recursively (string-append out "/bin")
                                 (string-append bin "/bin"))
               (delete-file-recursively (string-append out "/bin")))))
         (add-after 'install 'symlinks-instead-of-hard-links
           (lambda* (#:key outputs #:allow-other-keys)
             ;; All the drivers and gallium targets create hard links upon
             ;; installation (search for "hardlink each megadriver instance"
             ;; in the makefiles).  This is no good for us since we'd produce
             ;; nars that contain several copies of these files.  Thus, turn
             ;; them into symlinks, which saves ~124 MiB.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (files  (find-files lib
                                        (lambda (file stat)
                                          (and (string-contains file ".so")
                                               (eq? 'regular
                                                    (stat:type stat))))))
                    (inodes (map (compose stat:ino stat) files)))
               (for-each (lambda (inode)
                           (match (filter-map (match-lambda
                                                ((file ino)
                                                 (and (= ino inode) file)))
                                              (zip files inodes))
                             ((_)
                              #f)
                             ((reference others ..1)
                              (format #t "creating ~a symlinks to '~a'~%"
                                      (length others) reference)
                              (for-each delete-file others)
                              (for-each (lambda (file)
                                          (if (string=? (dirname file)
                                                        (dirname reference))
                                              (symlink (basename reference)
                                                       file)
                                              (symlink reference file)))
                                        others))))
                         (delete-duplicates inodes))))))))
    (home-page "https://mesa3d.org/")
    (synopsis "OpenGL and Vulkan implementations")
    (description "Mesa is a free implementation of the OpenGL and Vulkan
specifications - systems for rendering interactive 3D graphics.  A variety of
device drivers allows Mesa to be used in many different environments ranging
from software emulation to complete hardware acceleration for modern GPUs.")
    (license license:x11)))

(define-public wayland-protocols-latest
  (package
    (name "wayland-protocols")
    (version "1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "1hlb6gvyqlmsdkv5179ccj07p04cn6xacjkgklakbszczv7xiw5z"))))
    (build-system meson-build-system)
    (inputs
     (list wayland-latest))
    (native-inputs
     (list pkg-config python))
    (synopsis "Wayland protocols")
    (description "Wayland-Protocols contains Wayland protocols that add
functionality not available in the Wayland core protocol.  Such protocols either
add completely new functionality, or extend the functionality of some other
protocol either in Wayland core, or some other protocol in wayland-protocols.")
    (home-page "https://wayland.freedesktop.org")
    (license license:expat)))

(use-modules (gnu packages web))

(define-public sway-latest
  (package
    (name "sway")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/sway")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ss3l258blyf2d0lwd7pi7ga1fxfj8pxhag058k7cmjhs3y30y5l"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode path to swaybg.
             (substitute* "sway/config.c"
               (("strdup..swaybg..")
                (string-append "strdup(\"" (assoc-ref inputs "swaybg")
                               "/bin/swaybg\")")))
             ;; Hardcode path to scdoc.
             (substitute* "meson.build"
               (("scdoc.get_pkgconfig_variable..scdoc..")
                (string-append "'" (assoc-ref inputs "scdoc")
                               "/bin/scdoc'")))
             #t)))))
    (inputs (list cairo
                  elogind
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput
                  libxkbcommon
                  libdrm-latest
                  pango
                  swaybg
                  ;;egl-wayland
                  wayland-latest
                  wlroots-latest))
    (native-inputs
     (list linux-pam mesa pkg-config scdoc wayland-protocols-latest))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Wayland compositor compatible with i3")
    (description "Sway is a i3-compatible Wayland compositor.")
    (license license:expat)))

(use-modules (guix transformations)
             (nongnu packages nvidia))

(define-public mesa->mesa-latest
  ;; This is a procedure to replace MESA by MESA-LATEST,
  ;; recursively.
  (package-input-rewriting `((,mesa . ,mesa))
                           #:deep? #f))

(define mesa/fake
  (package
    (inherit mesa-latest)
    (replacement (mesa->mesa-latest nvda))))

(define-public mesa->nvda
  ;; This is a procedure to replace MESA-LATEST by NVDA,
  ;; recursively.
  (package-input-rewriting `((,mesa-latest . ,mesa/fake))
                           #:deep? #f))

(define-public (transform-nvidia pkg)
  (mesa->nvda (mesa->mesa-latest pkg)))

;; (mesa-nvda (mesa->mesa-latest sway-latest))
;; or
;; (map (compose mesa->mesa-latest mesa->nvda)
;;      (list wayland-protocols-latest
;;            wayland-latest
;;            libglvnd
;;            libdrm-latest
;;            wlroots-latest
;;            sway-latest)))

(define-public emacs-org-roam-ui
  (package
  (name "emacs-org-roam-ui")
  (version "20220225.2151")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/org-roam/org-roam-ui.git")
             (commit "df1f9522c5a9cdb248208427fa9df4f2a7666e2a")))
      (sha256
        (base32 "03kyg95f012ql0gpzy58kzxgdfksig5zlbr1p9m9ycgqmmxyq4jp"))))
  (build-system emacs-build-system)
  (propagated-inputs
   (list emacs-org-roam
         emacs-simple-httpd
         emacs-websocket))
  (arguments
    '(#:include
      '("^[^/]+.el$"
        "^[^/]+.el.in$"
        "^dir$"
        "^[^/]+.info$"
        "^[^/]+.texi$"
        "^[^/]+.texinfo$"
        "^doc/dir$"
        "^doc/[^/]+.info$"
        "^doc/[^/]+.texi$"
        "^doc/[^/]+.texinfo$"
        "^out$")
      #:exclude
      '("^.dir-locals.el$"
        "^test.el$"
        "^tests.el$"
        "^[^/]+-test.el$"
        "^[^/]+-tests.el$")))
  (home-page "https://github.com/org-roam/org-roam-ui")
  (synopsis "User Interface for Org-roam")
  (description
    "Org-roam-ui provides a web interface for navigating around notes
created within Org-roam.")
  (license license:gpl3+)))

(use-modules (gnu packages python))
(define-public python-3.6.4
  (package (inherit python-2)
    (version "3.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-fix-tests.patch"
                        "python-3-fix-tests.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "1fna7g8jxzl4kd2pqmmqhva5724c5m920x3fsrpsgskaylmr76qm"))
              (snippet
               '(begin
                  (for-each delete-file
                            '("Lib/ctypes/test/test_structures.py" ; fails on aarch64
                              "Lib/ctypes/test/test_win32.py" ; fails on aarch64
                              "Lib/test/test_fcntl.py")) ; fails on aarch64
                  #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:tests? _) #t)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-timestamp-for-pyc-files
             (lambda _
               ;; We set DETERMINISTIC_BUILD to only override the mtime when
               ;; building with Guix, lest we break auto-compilation in
               ;; environments.
               (setenv "DETERMINISTIC_BUILD" "1")
               (substitute* "Lib/py_compile.py"
                 (("source_stats\\['mtime'\\]")
                  "(1 if 'DETERMINISTIC_BUILD' in os.environ else source_stats['mtime'])"))

               ;; Use deterministic hashes for strings, bytes, and datetime
               ;; objects.
               (setenv "PYTHONHASHSEED" "0")

               ;; Reset mtime when validating bytecode header.
               (substitute* "Lib/importlib/_bootstrap_external.py"
                 (("source_mtime = int\\(source_stats\\['mtime'\\]\\)")
                  "source_mtime = 1"))
               #t))
           ;; These tests fail because of our change to the bytecode
           ;; validation.  They fail because expected exceptions do not get
           ;; thrown.  This seems to be no problem.
           (add-after 'unpack 'disable-broken-bytecode-tests
             (lambda _
               (substitute* "Lib/test/test_importlib/source/test_file_loader.py"
                 (("test_bad_marshal")
                  "disable_test_bad_marshal")
                 (("test_no_marshal")
                  "disable_test_no_marshal")
                 (("test_non_code_marshal")
                  "disable_test_non_code_marshal"))
               #t))
           ;; Unset DETERMINISTIC_BUILD to allow for tests that check that
           ;; stale pyc files are rebuilt.
           (add-before 'check 'allow-non-deterministic-compilation
             (lambda _ (unsetenv "DETERMINISTIC_BUILD") #t))
           ;; We need to rebuild all pyc files for three different
           ;; optimization levels to replace all files that were not built
           ;; deterministically.

           ;; FIXME: Without this phase we have close to 2000 files that
           ;; differ across different builds of this package.  With this phase
           ;; there are about 500 files left that differ.
           (add-after 'install 'rebuild-bytecode
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "DETERMINISTIC_BUILD" "1")
               (let ((out (assoc-ref outputs "out")))
                 (for-each
                  (lambda (opt)
                    (format #t "Compiling with optimization level: ~a\n"
                            (if (null? opt) "none" (car opt)))
                    (for-each (lambda (file)
                                (apply invoke
                                       `(,(string-append out "/bin/python3")
                                         ,@opt
                                         "-m" "compileall"
                                         "-f" ; force rebuild
                                         ;; Don't build lib2to3, because it's Python 2 code.
                                         ;; Also don't build obviously broken test code.
                                         "-x" "(lib2to3|test/bad.*)"
                                         ,file)))
                              (find-files out "\\.py$")))
                  (list '() '("-O") '("-OO"))))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))
(define-public emacs-sql-indent
  (package
   (name "emacs-sql-indent")
   (version "1.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/sql-indent-"
           version
           ".tar"))
     (sha256
      (base32 "000pimlg0k4mrv2wpqq8w8l51wpr1lzlaq6ai8iaximm2a92ap5b"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/alex-hhh/emacs-sql-indent")
   (synopsis "Support for indenting code in SQL files.")
   (description
    "`sqlind-minor-mode' is a minor mode that enables syntax-based indentation for
`sql-mode' buffers: the TAB key indents the current line based on the SQL code
on previous lines.  To setup syntax-based indentation for every SQL buffer, add
`sqlind-minor-mode' to `sql-mode-hook'.  Indentation rules are flexible and can
be customized to match your personal coding style.  For more information, see
the \"sql-indent.org\" file.

The package also defines align rules so that the `align' function works for SQL
statements, see `sqlind-align-rules'.")
   (license license:gpl3+)))

(define-public emacs-uuidgen
  (package
   (name "emacs-uuidgen")
   (version "20200816.1308")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/kanru/uuidgen-el.git")
           (commit "b50e6fef2de4199a8f207b46588c2cb3890ddd85")))
     (sha256
      (base32 "08m74kj7h70kna3pifk3sgsy7mck11p32vi48h9wzqnafyq3n55d"))))
   (build-system emacs-build-system)
   (home-page "unspecified")
   (synopsis "Provides various UUID generating functions")
   (description
    " This is a naive implementation of RFC4122 Universally Unique IDentifier
generation in elisp.  Currently implemented are UUID v1 v3, v4 and v5
generation.  The resolution of the time based UUID is microseconds, which is 10
times of the suggested 100-nanosecond resolution, but should be enough for
general usage.

Get development version from git:

    git clone git://github.com/kanru/uuidgen-el.git")
   (license #f)))

(define-public emacs-code-review
  (package
   (name "emacs-code-review")
   (version "20220328.108")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/wandersoncferreira/code-review.git")
           (commit "85ab2080e489b4ca01c787f5a316ade02a4ee877")))
     (sha256
      (base32 "0j0ijnzfd7b3a2jqi94zlky8iqv9g7vj9fx5fd4g2k53ilgapmdl"))))
   (build-system emacs-build-system)
   (propagated-inputs
    (list emacs-closql
          emacs-magit
          emacs-a
          emacs-ghub
          emacs-uuidgen
          emacs-deferred
          emacs-markdown-mode
          emacs-forge
          emacs-emojify))
   (home-page "https://github.com/wandersoncferreira/code-review")
   (synopsis "Perform code review from Github, Gitlab, and Bitbucket Cloud")
   (description
    "Review Pull Request in Emacs using a modern interface based on Magit
Section and Transient.  Currently supports Github, Gitlab, and
Bitbucket Cloud.")
   (license license:gpl3+)))
