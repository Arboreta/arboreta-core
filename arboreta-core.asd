(defclass cl-file (cl-source-file)
  ((type :initform "cl")))

(defsystem "arboreta-core"
  :name "arboreta-core" 
  :serial t
  :version "0.3"
  :author "Dylan Ball <Arathnim@gmail.com>"
  :depends-on 
    (alexandria anaphora cl-cairo2
     cl-cairo2-xlib cl-colors cl-pango
     cl-ppcre iterate)
  :components 
    ((:module :src
      :components
        ((:cl-file "cl-xkb")
         (:cl-file "base" :depends-on ("cl-xkb"))))))
