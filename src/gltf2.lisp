(in-package :cl-gltf2)

(defvar *object*)

(defclass gltf2 ()
  ((parse-tree :accessor parse-tree)
   (json :accessor json)
   (buffers :accessor buffers)))

(defun load-gltf (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (with-buffer-read (:stream in)
      (let ((*object* (make-instance 'gltf2)))
        (setf (parse-tree *object*) (parse-datastream))
        *object*))))
