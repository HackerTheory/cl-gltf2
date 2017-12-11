(asdf:defsystem #:cl-gltf2
  :description "A loader for glTF2 (OpenGL Transmission Format) files."
  :author ("Michael Fiano <michael.fiano@gmail.com>")
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/cl-gltf2"
  :bug-tracker "https://github.com/mfiano/cl-gltf2/issues"
  :source-control (:git "git@github.com:mfiano/cl-gltf2.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:parsley
               #:cl-json
               #:cl-opengl)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "gltf2")
   (:file "datastream")
   (:file "chunk")
   (:file "buffers")))
