(defpackage :xkb
  (:use :common-lisp :cffi))

(in-package :xkb)

(define-foreign-library xkb
  (:unix (:or "/usr/lib64/libxkbcommon.so.0" "/usr/lib64/libxkbcommon.so" "/usr/lib/x86_64-linux-gnu/libxkbcommon.so"))
  (t (:default "libxkbcommon")))

(use-foreign-library xkb)

(defctype display :pointer)

(defcstruct xkb-rule-names
  (rules :string)
  (model :string)
  (layout :string)
  (variant :string)
  (options :string))

(defcfun "xkb_context_new" :pointer
  (flags :uint32))

(defcfun "xkb_keymap_new_from_names" :pointer
  (context :pointer)
  (names :pointer)
  (flags :uint32))

(defcfun ("XkbKeycodeToKeysym" xkb-keycode->keysym) :long
  (display display)
  (keycode :int)
  (group :uint32)
  (level :uint32))

;;(defcfun "xkb_keymap_get_as_string" :string
 ;; (keymap :pointer)
 ;; (format :int32))

(defun new-keymap-from-names (ctx rules model layout variant options)
  (let* (;;(ctx (xkb-context-new 0))
	 (names (foreign-alloc '(:struct xkb-rule-names)))
	 (rules-ptr (foreign-alloc :char :count (+ (length rules) 1)))
	 (model-ptr (foreign-alloc :char :count (+ (length model) 1)))
	 (layout-ptr (foreign-alloc :char :count (+ (length layout) 1)))
	 (variant-ptr (foreign-alloc :char :count (+ (length variant) 1)))
	 (options-ptr (foreign-alloc :char :count (+ (length options) 1))))
    (lisp-string-to-foreign rules rules-ptr (+ (length rules) 1))
    (lisp-string-to-foreign model model-ptr (+ (length model) 1))
    (lisp-string-to-foreign layout layout-ptr (+ (length layout) 1))
    (lisp-string-to-foreign variant variant-ptr (+ (length variant) 1))
    (lisp-string-to-foreign options options-ptr (+ (length options) 1))
    (setf (foreign-slot-value names '(:struct xkb-rule-names) 'rules) rules-ptr)
    (setf (foreign-slot-value names '(:struct xkb-rule-names) 'model) model-ptr)
    (setf (foreign-slot-value names '(:struct xkb-rule-names) 'layout) layout-ptr)
    (setf (foreign-slot-value names '(:struct xkb-rule-names) 'variant) variant-ptr)
    (setf (foreign-slot-value names '(:struct xkb-rule-names) 'options) options-ptr)
    (let ((keymap (xkb-keymap-new-from-names ctx names 0)))
      (foreign-free names)
      keymap)))


;;(new-keymap-from-names "" "pc105" "gb" "qwerty" "")

(defcfun "xkb_state_new" :pointer
  (keymap :pointer))

(defcfun "xkb_state_key_get_one_sym" :uint32
  (state :pointer)
  (keycode :uint32))

(defcfun ("xkb_keysym_get_name") :int
  (keysym :uint32)
  (buffer :string)
  (size :uint32))

(defun get-keysym-name (keysym)
  (let* ((keysym-name (foreign-alloc :char :count 64)))
    (xkb-keysym-get-name keysym keysym-name 64)
    (let ((name (foreign-string-to-lisp keysym-name)))
      (foreign-free keysym-name)
      name)))


(defcfun "xkb_state_mod_name_is_active" :int
  (state :pointer)
  (modifier :string)
  (type :int)) ;; effective 8

(defcfun "xkb_keymap_get_as_string" :string
  (keymap :pointer)
  (format :int)) ;; 1

(defcfun "xkb_keymap_key_by_name" :uint32
  (keymap :pointer)
  (name :string))

(defcfun "xkb_keymap_min_keycode" :uint32
  (keymap :pointer))

(defcfun "xkb_keymap_max_keycode" :uint32
  (keymap :pointer))

(defcfun "xkb_state_update_key" :int
  (state :pointer)
  (key :uint32)
  (direction :int))

(defcfun "xkb_state_serialize_mods" :uint32
  (state :pointer)
  (components :int))

(defcfun "xkb_state_serialize_layout" :uint32
  (state :pointer)
  (components :int))

(defcfun "xkb_state_unref" :void
  (state :pointer))
