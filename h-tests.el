;;; h-tests.el --- Project navigation and remote checkout -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Félix Baylac Jacqué
;; Author: Félix Baylac Jacqué <felix at alternativebit.fr>
;; Maintainer: Félix Baylac Jacqué <felix at alternativebit.fr>
;; Version: 1.14.0

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

;;; Commentary:

;; TODO before publish

;;; Code:

(require 'ert)
(require 'h)

; For reference: test-root-1 looks like this
; test-root-1
; ├── example1.tld
; │   ├── user1
; │   │   ├── proj1
; │   │   └── proj2
; │   └── user2
; │       └── proj1
; └── example2.tld
;     └── user1
;         └── proj1
(ert-deftest h-get-code-root-projects-test ()
  "Testing the `h--get-code-root-projects with test-root-1 setup."
  (let
      (
       (results (h--get-code-root-projects "./test/fixtures/test-root-1")))
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user1/proj2" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 4))
  ))

(h--get-code-root-projects "./test/fixtures/test-root-1")




(provide 'h-tests)
;;; h-tests.el ends here
