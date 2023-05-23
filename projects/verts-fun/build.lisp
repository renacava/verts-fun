(load "verts-fun.asd")
(ql:quickload :verts-fun)
(sb-ext:save-lisp-and-die "verts-fun.exe" :executable t :toplevel #'verts-fun::start)