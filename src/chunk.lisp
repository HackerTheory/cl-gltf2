(in-package :cl-gltf2)

(defvar *chunk*)

(defclass chunk ()
  ((%length :reader chunk-length)
   (%type :reader %chunk-type)
   (%data :reader chunk-data)))

(defmethod print-object ((object chunk) stream)
  (print-unreadable-object (object stream :type t)
    (let ((*chunk* object))
      (format stream "~s" (chunk-type)))))

(defun chunk-type ()
  (case (%chunk-type *chunk*)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun last-chunk-p ()
  (= (file-length (buffer-stream)) (buffer-position)))

(defun parse-chunk ()
  (let ((*chunk* (make-instance 'chunk)))
    (with-slots (%length %type %data) *chunk*
      (setf %length (read-uint-le 4)
            %type (read-uint-le 4)
            %data (parse-chunk-data (chunk-type))))
    *chunk*))

(defgeneric parse-chunk-data (chunk-type)
  (:method :around (chunk-type)
    (with-buffer-read (:sequence (read-bytes (chunk-length *chunk*)))
      (call-next-method))))

(defmethod parse-chunk-data ((chunk-type (eql :json-content)))
  (let ((data (read-string :encoding :utf-8)))
    (setf (json *object*) (json:decode-json-from-string data))
    data))

(defmethod parse-chunk-data ((chunk-type (eql :binary-buffer)))
  (parse-json-data :root)
  nil)

(defmethod parse-chunk-data ((chunk-type (eql :unknown)))
  (warn "Ignoring an unknown chunk type."))
