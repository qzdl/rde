(use-modules (guix channels))

(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "ceeeb5365de0106919857fbf1cead741b0735cfe"))
      (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
<<<<<<< HEAD
          "8c360eeae19f5002efa801bcd5cf84465c9b815d")
||||||| 2adc2957
          "866a0fbca5e9fcf2bbc97cd605ea669e810c5aa1")
=======
          "a7b59443405169600a00f0b295a3fb1de360cb0b")
>>>>>>> origin/master
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
<<<<<<< HEAD
          "f3baa5da06159853ba63c5e666cfc32bb41b0c7c")
||||||| 2adc2957
          "3a2200e1ad2049ad7e25295e6b4e013f74dd84e2")
=======
          "04724e59971b03f86a410285653d24005c62b924")
>>>>>>> origin/master
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
