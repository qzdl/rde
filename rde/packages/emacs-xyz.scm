;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
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

(define-module (rde packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages texinfo)
  #:use-module ((guix licenses) #:prefix license:))

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

(define-public emacs-org-modern
  (package
   (name "emacs-org-modern")
   (version "0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/minad/org-modern")
                  (commit "dc19304f409259d1b258c51cedd2d362e0ff9b98")))
            (sha256
             (base32 "1b0cis1n786c4lkrsi71ak2wv21mhgbfk3q2pp6qiqhddah0l1cg"))
            (file-name (git-file-name name version))))
   (build-system emacs-build-system)
   (license license:gpl3+)
   (home-page "https://github.com/minad/org-modern")
   (synopsis "")
   (description "")))

(define-public emacs-vertico-latest
  (let* ((commit "2de617a9199d152533ce280c6eb653147f15f8d1")
         (revision "2"))
    (package
     (inherit emacs-vertico)
     (name "emacs-vertico")
     (version (git-version "0.20" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minad/vertico")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08h1lv63dkrfk3m94z73xmjabch6699kd9qm3cvkcr8n67h6j6fp")))))))

(define-public emacs-marginalia-latest
  (let* ((commit "5767b6ff49e26ecd6aa26f552397d5d2b8213d25")
         (revision "0"))
    (package
     (inherit emacs-marginalia)
     (name "emacs-marginalia")
     (version (git-version "0.12" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minad/marginalia")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "143d57fy5i5ziwfxxix595k0f98ay5l57x5z69g8lkp6nb7b1rq7")))))))

(define-public emacs-vterm-latest
  (let ((version "0.0.1")
        (revision "1")
        (commit "a940dd2ee8a82684860e320c0f6d5e15d31d916f"))
    (package
     (inherit emacs-vterm)
     (name "emacs-vterm")
     (version (git-version version revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/akermu/emacs-libvterm")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r1iz92sn2ddi11arr9s8z7cdpjli7pn55yhaswvp4sdch7chb5r")))))))

(define-public emacs-consult-dir
  (package
   (name "emacs-consult-dir")
   (version "0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/karthink/consult-dir")
                  (commit (string-append "v" version))))
            (sha256
             (base32 "1cff4ssrn1mw2s5n090pdmwdirnfih8idg5f0ll2bi2djc4hq5kn"))
            (file-name (git-file-name name version))))
   (build-system emacs-build-system)
   (license license:gpl3+)
   (propagated-inputs (list emacs-consult))
   (home-page "https://github.com/karthink/consult-dir")
   (synopsis "Insert paths into minibuffer prompts in Emacs")
   (description "Consult-dir allows you to easily insert directory
paths into the minibuffer prompt in Emacs.

When using the minibuffer, you can switch - with completion and
filtering provided by your completion setup - to any directory you’ve
visited recently, or to a project or bookmarked directory. The
minibuffer prompt will be replaced with the directory you choose.")))

(define-public emacs-cyrillic-dvorak-im
  (package
    (name "emacs-cyrillic-dvorak-im")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xFA25E/cyrillic-dvorak-im")
             (commit version)))
       (sha256
        (base32 "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n"))
       (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (home-page "https://github.com/xFA25E/cyrillic-dvorak-im")
    (synopsis "Cyrillic input method for dvorak layout")
    (description "Cyrillic input method for dvorak layout.")
    (license license:gpl3+)))

(define-public emacs-mini-frame
  (package
   (inherit emacs-unfill)
   (name "emacs-mini-frame")
   (version "1.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/muffinmad/emacs-mini-frame.git")
                  (commit "41afb3d79cd269726e955ef0896dc077562de0f5")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0yghz9pdjsm9v6lbjckm6c5h9ak7iylx8sqgyjwl6nihkpvv4jyp"))))))

(define-public emacs-hide-header-line
  (package
    (inherit emacs-hide-mode-line)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-it-update-header-line
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "hide-mode-line.el"
	       ((" mode-line-format")
                " header-line-format"))
             #t)))))))

(define-public emacs-git-email-latest
  (let* ((commit "b5ebade3a48dc0ce0c85699f25800808233c73be")
         (revision "0"))
    (package
      (name "emacs-git-email")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~yoctocell/git-email")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1lk1yds7idgawnair8l3s72rgjmh80qmy4kl5wrnqvpmjrmdgvnx"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; piem is not yet packaged in Guix.
           (add-after 'unpack 'remove-piem
             (lambda _
               (delete-file "git-email-piem.el")
               (delete-file "git-email-gnus.el")
               (delete-file "git-email-mu4e.el")))
           (add-before 'install 'makeinfo
             (lambda _
               (invoke "makeinfo" "doc/git-email.texi"))))))
      (native-inputs
       `(("texinfo" ,texinfo)))
      (inputs
       `(("emacs-magit" ,emacs-magit)
         ("notmuch" ,emacs-notmuch)))
      (license license:gpl3+)
      (home-page "https://sr.ht/~yoctocell/git-email")
      (synopsis "Format and send Git patches in Emacs")
      (description "This package provides utilities for formatting and
sending Git patches via Email, without leaving Emacs."))))

(define-public emacs-git-gutter-transient
  (package
   (name "emacs-git-gutter-transient")
   (version "0.1.0")
   (source
    (local-file "../features/emacs/git-gutter-transient" #:recursive? #t))
   (build-system emacs-build-system)
   (inputs
    `(("emacs-magit" ,emacs-magit)))
   (propagated-inputs
    `(("emacs-git-gutter" ,emacs-git-gutter)
      ("emacs-transient" ,emacs-transient)))
   (license license:gpl3+)
   (home-page "https://sr.ht/~abcdw/git-gutter-transient")
   (synopsis "Navigate, stage and revert hunks with ease")
   (description "This package provides transient interface for git-gutter function
to manipulate and navigate hunks.")))

(define-public emacs-es-mode-latest
  (package
    (name "emacs-es-mode")
    (version "4.3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dakrone/es-mode")
             (commit "cde5cafcbbbd57db6d38ae7452de626305bba68d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02zzwf9ykfi2dggjbspg7mk77b5x1fnkpp3bcp6rd4h95apnsjq5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     ;; The version of org in Emacs 24.5 is not sufficient, and causes
     ;; tables to be rendered incorrectly
     (list emacs-dash
           emacs-org
           emacs-spark
           emacs-s
           emacs-request))
    (home-page "https://github.com/dakrone/es-mode")
    (synopsis "Major mode for editing Elasticsearch queries")
    (description "@code{es-mode} includes highlighting, completion and
indentation support for Elasticsearch queries.  Also supported are
@code{es-mode} blocks in @code{org-mode}, for which the results of queries can
be processed through @code{jq}, or in the case of aggregations, can be
rendered in to a table.  In addition, there is an @code{es-command-center}
mode, which displays information about Elasticsearch clusters.")
    (license license:gpl3+)))

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

(define-public emacs-perfect-margin
  (package
    (name "emacs-perfect-margin")
    (version "0.1")
    (home-page "https://github.com/mpwang/perfect-margin")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit "94b055c743b1859098870c8aca3e915bd6e67d9d")))
       (sha256
        (base32 "02k379nig43j85wfm327pw6sh61kxrs1gwz0vgcbx9san4dp83bk"))))
    (build-system emacs-build-system)
    (synopsis "A global margin-making-mode, great for ultrawides")
    (description
     "[emacs] auto center emacs windows, work with minimap and/or linum-mode")
    (license license:gpl3+)))

(define-public emacs-mpv
  (package
   (name "emacs-mpv")
   (version "20211228.2043")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/kljohann/mpv.el.git")
           (commit "4fd8baa508dbc1a6b42b4e40292c0dbb0f19c9b9")))
     (sha256
      (base32 "03zziy1lcvpf1wq15bsxwy0dhdb2z7rrdcj6srgrmgykz2wf33q7"))))
   (build-system emacs-build-system)
   (propagated-inputs
    (list ;;emacs-json
          emacs-org))
   (home-page "https://github.com/kljohann/mpv.el")
   (synopsis "control mpv for easy note-taking")
   (description
    "This package is a potpourri of helper functions to control a mpv process via its
IPC interface.  You might want to add the following to your init file:

(org-add-link-type \"mpv\" #'mpv-play) (defun org-mpv-complete-link (&optional
arg)   (replace-regexp-in-string    \"file:\" \"mpv:\"    (org-file-complete-link
arg)    t t)) (add-hook 'org-open-at-point-functions
#'mpv-seek-to-position-at-point)")
   (license license:gpl3+)))

(define-public emacs-waveform-el
  (package
   (name "emacs-waveform-el")
   (version "20220105.0826")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/sachac/waveform-el.git")
           (commit "ee52c6a72b3e9890743e3a6e2fc1f3195f5687b2")))
     (sha256
      (base32 "082ls7khd22fjwnk7h1zxrmqqcmxqh2wx2vljlxhjh9bcp1y2pyr"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-mpv emacs-org))
   (home-page "https://github.com/sachac/waveform-el")
   (synopsis "Display a waveform in GNU Emacs, and use it to navigate a file in mpv.")
   (description
    "To help [...] select timestamps [..].  Finding the right time in MPV
was hard because it didn’t have a waveform view.  Audacity could show
waveforms, but it didn’t have an easy way to copy the timestamp. So
the obvious answer is, of course, to make the text editor do the
job.  Yay Emacs!")
   (license license:gpl3+)))

(define-public emacs-ob-go
  (package
   (name "emacs-ob-go")
   (version "20190201.214")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/pope/ob-go")
           (commit "2067ed55f4c1d33a43cb3f6948609d240a8915f5")))
     (sha256
      (base32 "069w9dymiv97cvlpzabf193nyw174r38lz5j11x23x956ladvpbw"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org))
   (home-page "https://github.com/pope/ob-go")
   (synopsis "Org-Babel support for evaluating go code.")
   (description
    "@code{ob-go} enables @{Org-Babel} support for evaluating @code{go}
code. It was created based on the usage of @code{ob-C}. The @code{go}
code is compiled and run via the @code{go run} command. If a
@code{main} function isn’t present, by default the code is wrapped in
a simple @{main func}. If @code{:package} option isn’t set, and no
package is declared in the code, then the @code{main package} is
declared.")
   (license license:gpl3+)))
