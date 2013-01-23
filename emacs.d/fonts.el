(defvar preferred-font "Inconsolata-dz")
(defvar preferred-font-size 120)

(defun set-font-size (new-size)
  (setq preferred-font-size new-size)
  (when window-system
    (set-face-attribute 'default nil :font preferred-font :height preferred-font-size)))

(defun increase-font-size ()
  (interactive)
  (set-font-size (+ preferred-font-size 10)))

(defun decrease-font-size ()
  (interactive)
  (set-font-size (- preferred-font-size 10)))

;; (global-set-key "\C--" 'decrease-font-size)
;; (global-set-key "\C-=" 'increase-font-size)
