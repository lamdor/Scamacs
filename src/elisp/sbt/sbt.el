;; Support for running sbt in inferior mode.

;; 20090918 Suggestions from Florian Hars 
;;  - Removed global manipulations.
;;  - Removed colorization attempts to use base sbt anis colorization.

(eval-when-compile (require 'cl))
(require 'tool-bar)
(require 'compile)

(defgroup sbt nil
  "Run SBT REPL as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)

(defconst sbt-copyright    "Copyright (C) 2008 Raymond Paul Racine")
(defconst sbt-copyright-2  "Portions Copyright (C) Free Software Foundation")

(defconst sbt-version      "0.021")
(defconst sbt-author-name  "Raymond Racine")
(defconst sbt-author-email "ray.racine@gamail.com")

(defconst sbt-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  You
should have received a copy of the GNU General Public License along with Emacs;
see the file `COPYING'.  If not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

(defgroup sbt nil
  "Support for sbt build REPL."
  :group  'sbt
  :prefix "sbt-")

(defcustom sbt-program-name "sbt"
  "Program invoked by the `run-sbt' command."
  :type 'string
  :group 'sbt)

(defun sbt-build-buffer-name (mode)
 "*Scala Build Tool*")

(defun sbt-shell ()
 "Launch the sbt shell."
 (interactive)
 (let ((compilation-buffer-name-function 'sbt-build-buffer-name))
   (compile (concat "cd " (sbt-find-path-to-parent-project) "; sbt") t)
   (pop-to-buffer (sbt-build-buffer-name nil))
   (make-local-variable 'comint-prompt-read-only)
   (set 'comint-prompt-read-only t)
   (goto-char (point-max))))

(defun sbt-clear ()
  "Clear (erase) the SBT buffer."
  (interactive)
  (with-current-buffer (sbt-build-buffer-name nil)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;; set to nil as could be causing a problem
(set 'compilation-auto-jump-to-first-error nil)

(ansi-color-for-comint-mode-on)

;; Locate the project root directory from the source buffer location.

(defun sbt-project-dir-p (path)
  "Does a project/build.properties exists in the given path."
  (file-exists-p (concat path "/project/build.properties")))

(defun sbt-at-root (path)
  "Determine if the given path is root."
  (equal path (sbt-parent-path path)))

(defun sbt-parent-path (path)
  "The parent path for the given path."
  (file-truename (concat path "/..")))

;; Search up the directory tree until directory with a "project" subdir 
;; is found with build.properties
(defun sbt-find-path-to-project ()
  "Move up the directory tree for the current buffer until root or a directory with a project/build.properties is found."
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((path (file-name-directory fn)))
      (while (and (not (sbt-project-dir-p path))
		  (not (sbt-at-root path)))
	(setf path (file-truename (sbt-parent-path path))))
      path)))

(defun sbt-find-path-to-parent-project ()
  (interactive)
  "Search up the directory tree find an SBT project dir, then see if it has a parent above it."
  (let ((path (sbt-find-path-to-project)))
    (let ((parent-path (file-truename (concat path "/.."))))
      (if (not (sbt-project-dir-p parent-path))
	  path
	parent-path))))
	
