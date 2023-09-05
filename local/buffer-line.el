(defgroup buffer-line '()
  "Faces from the buffer line")

(defface buffer-line-face-active nil
  "Face for the currently selected buffer in the buffer line"
  :group 'buffer-line)
(defface buffer-line-face-inactive nil
  "Face for buffers in the buffer line"
  :group 'buffer-line)
(defface buffer-line-face-separator nil
  "Face for the separator inbetween buffers, default: inherits buffer-line-face-inactive"
  :group 'buffer-line)
(defface buffer-line-face-status nil
  "Face for the status indicator in the buffer line"
  :group 'buffer-line)

(set-face-attribute
 'buffer-line-face-active nil
 :inherit 'default
 :weight 'bold)
(set-face-attribute
 'buffer-line-face-inactive nil
 :inherit 'default)
(set-face-attribute
 'buffer-line-face-separator nil
 :inherit 'buffer-line-face-inactive)
(set-face-attribute
 'buffer-line-face-status nil
 :background "#777777"
 :inherit 'default)

(defcustom buffer-line-separator " "
  "The separator between buffers in the buffer line"
  :type 'string
  :group 'buffer-line)
(defcustom buffer-line-prefix ""
  "Prefix to be displayed at the left end of the line"
  :type 'string
  :group 'buffer-line)
(defcustom buffer-line-postfix ""
  "Postfix to be displayed at the right end of the line"
  :type 'string
  :group 'buffer-line)
(defcustom buffer-line-name-padding " "
  "String to be appended and prepended to buffer names for padding"
  :type 'string
  :group 'buffer-line)
(defcustom buffer-line-top-margin 0.2
  "A margin to be added on top of the text"
  :type 'float
  :group 'buffer-line)
(defcustom buffer-line-bottom-margin 0.2
  "A margin to be added below the text"
  :type 'float
  :group 'buffer-line)

(defun buffer-line ()
  (setq header-line-format
	(list (propertize " %+ " 'face 'buffer-line-face-status)
	      (concat
	       buffer-line-prefix
	       (propertize " " 'display `(raise ,buffer-line-top-margin))
	       (string-join
		(mapcar
		 (lambda (name)
		   (propertize
		    (concat
		     buffer-line-name-padding
		     name
		     buffer-line-name-padding)
		    'face
		    (if (eq name (buffer-name (current-buffer)))
			'buffer-line-face-active
		      'buffer-line-face-inactive)))
		 (sort
		  (seq-filter
		   (lambda (name) (not (string-prefix-p "*" name)))
		   (mapcar
		    (lambda (buffer)
		      (string-trim (buffer-name buffer)))
		   (buffer-list)))
		 (lambda (name1 name2) (string-collate-lessp name1 name2 system-time-locale t))))
	       (propertize buffer-line-separator
			   'face 'buffer-line-face-separator))
	      (propertize " " 'display `(raise ,(- buffer-line-bottom-margin)))
	      buffer-line-postfix))))

(setq default-frame-alist
      (append
       (list
	'(vertical-scroll-bars . nil)
	'(tool-bar-lines . 0)
	'(menu-bar-lines . 0)
	'(left-fringe    . 1)
        '(right-fringe   . 1)
	'(internal-border-width . 30))))
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(tooltip-mode 0)
(menu-bar-mode 0)
(fringe-mode nil)

(add-hook 'buffer-list-update-hook (lambda () (buffer-line)))
(setq-default mode-line-format nil)

(provide 'buffer-line)

