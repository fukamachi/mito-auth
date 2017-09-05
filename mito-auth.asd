(defsystem "mito-auth"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("mito"
               "ironclad"
               "babel")
  :components ((:module "src"
                :components
                ((:file "mito-auth"))))
  :description "User authorization for Mito classes")
