(in-package :cl-gltf2)

(defvar *buffer*)

(defun get-attr-location (attr)
  (ecase (car attr)
    (:+position+ 0)
    (:+normal+ 1)
    (:+tangent+ 2)
    (:+color-0+ 3)
    (:+texcoord-0+ 4)
    (:+texcoord-1+ 5)
    (:+joints-0+ 6)
    (:+weights-0+ 7)))

(defun get-component-type (accessor)
  (ecase (lookup :component-type accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-component-count (accessor)
  (ecase (make-keyword (lookup :type accessor))
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun lookup (key &optional (value (json *object*)))
  (cdr (assoc key value)))

(defun get-buffer-data (buffer-index offset size)
  (let ((buffer (aref (buffers *object*) buffer-index)))
    (subseq buffer offset (+ offset size))))

(defun make-gpu-buffer (target attr accessor buffer-view)
  (let* ((index (lookup :buffer buffer-view))
         (size (lookup :byte-length buffer-view))
         (offset (+ (or (lookup :byte-offset accessor) 0)
                    (or (lookup :byte-offset buffer-view) 0)))
         (stride (or (lookup :byte-stride buffer-view) 0))
         (data (static-vectors:make-static-vector
                size
                :element-type '(unsigned-byte 8)
                :initial-contents (get-buffer-data index offset size)))
         (pointer (static-vectors:static-vector-pointer data)))
    (let ((buffer-id (gl:gen-buffer)))
      (gl:bind-buffer target buffer-id)
      (unless attr
        (format t "~&EBO: ~a" buffer-id))
      (%gl:buffer-data target size pointer :static-draw)
      (when attr
        (gl:enable-vertex-attrib-array (get-attr-location attr))
        (%gl:vertex-attrib-pointer
         (get-attr-location attr)
         (get-component-count accessor)
         (get-component-type accessor)
         0
         stride
         pointer)))
    (static-vectors:free-static-vector data)))

;;

(defgeneric parse-json-data (type &key &allow-other-keys))

(defmethod parse-json-data ((type (eql :root)) &key)
  (parse-json-data :buffers)
  (loop :for mesh :in (lookup :meshes)
        :append (parse-json-data :mesh :node mesh)))

(defmethod parse-json-data ((type (eql :mesh)) &key node)
  (loop :for primitive :in (lookup :primitives node)
        :collect (parse-json-data :primitive :node primitive)))

(defmethod parse-json-data ((type (eql :primitive)) &key node)
  (let ((attributes (lookup :attributes node))
        (indices (lookup :indices node))
        (vao-id (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao-id)
    (format t "~&VAO: ~a" vao-id)
    (dolist (attr attributes)
      (parse-json-data :attribute :node attr))
    (when indices
      (parse-json-data :indices :node indices))
    (gl:bind-vertex-array 0)
    vao-id))

(defmethod parse-json-data ((type (eql :attribute)) &key node)
  (let ((accessor (elt (lookup :accessors) (cdr node))))
    (parse-json-data :accessor
                     :node accessor
                     :attr node
                     :target :array-buffer)))

(defmethod parse-json-data ((type (eql :indices)) &key node)
  (let ((accessor (elt (lookup :accessors) node)))
    (parse-json-data :accessor
                     :node accessor
                     :target :element-array-buffer)))

(defmethod parse-json-data ((type (eql :accessor)) &key node attr target)
  (let ((buffer-view (elt (lookup :buffer-views)
                          (lookup :buffer-view node))))
    (parse-json-data :buffer-view
                     :node buffer-view
                     :attr attr
                     :accessor node
                     :target target)))

(defmethod parse-json-data ((type (eql :buffer-view))
                            &key node attr accessor target)
  (make-gpu-buffer target attr accessor node))

(defmethod parse-json-data ((type (eql :buffers)) &key)
  (let ((buffers (lookup :buffers)))
    (setf (buffers *object*) (make-array (length buffers)))
    (loop :for buffer :in buffers
          :for i :below (length buffers)
          :for size = (lookup :byte-length buffer)
          :do (setf (aref (buffers *object*) i) (read-bytes size)))))
