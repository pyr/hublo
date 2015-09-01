;;; -*- lexical-binding: t;

(defstruct hb/config    source-dir output-dir context)
(defstruct hb/transform name phases)
(defstruct hb/page      pattern transforms)
(defstruct hb/handler   pattern transforms)
(defstruct hb/item      path payload route transforms meta)
