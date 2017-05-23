(ns plantilus.resources
  (:require [clojure.java.io :as io]))

(def resources-dir (.getParentFile (io/as-file (io/resource "plantilus.marker"))))

(def project-root (.getParentFile ^java.io.File resources-dir))

(def bin-dir  (io/file project-root "bin"))

(def data-dir (io/file resources-dir "data"))

(def pict-dir (io/file data-dir "pictures"))

(def plantdb-dir (io/file data-dir "plantdb"))

(def web-plantdb-dir (io/file data-dir "web-plantdb"))


(def plantdb-url "ftp://u51942341-miner:$PWD$@plantilus.com/plantilus/plantdb/")

(def junk-url "ftp://u51942341-miner:$PWD$@plantilus.com/junk/")
  
(def template-dir (io/file resources-dir "template"))

(def tmp-dir (io/file "/tmp/plantdb"))

;; Lisa exports FileMaker in MacRoman.  You need to use emacs M-x
;; my-unix-eol to fix the line endings

;; From Bento, it comes with unix-eof and already in UTF-8 so it's good
;; to go.

(def plants-csv-file (io/file data-dir "Plants.csv"))

(def daylillies-csv-file (io/file data-dir "Daylillies.csv"))

(def protea-csv-file (io/file data-dir "Protea.csv"))

(def orchid-csv-file (io/file data-dir "Orchid.csv"))

(def pub-csv-file (io/file data-dir "Publications.csv"))

(def author-csv-file (io/file data-dir "Authors.csv"))

(def genera-csv-file (io/file data-dir "Genera.csv"))

(def pinus-csv-file (io/file data-dir "Pinus.csv"))

(def botreg-csv-file (io/file data-dir "BotReg.csv"))

(def botreg-protea-csv-file (io/file data-dir "ProteaBotReg.csv"))

(def botreg-orchid-csv-file (io/file data-dir "OrchBotReg.csv"))

(def botrepos-csv-file (io/file data-dir "BotRep.csv"))

(def botrepos-protea-csv-file (io/file data-dir "ProteaBotRep.csv"))

(def botrepos-orchid-csv-file (io/file data-dir "OrchBotRep.csv"))

(def jacqplrar-csv-file (io/file data-dir "JacqPlRar.csv"))

(def cultprot-csv-file (io/file data-dir "CultProt.csv"))

(def tpids [ "AbelChin" "AbelGran" "AbelGranCany" "AbelGranConf"
	    "AbelGranGold" "AbelGranHopl" "AbelGranKale" "AbelGranLitt"
	    "AbelGranRose" "AbelGranSilv" "AbelGranSuns" "AbelMard" "AbelMosa"
	    "AbelRupe" "AbelRupeGran" "AlstPsit" "AlstPsitVari" "AmpeArbo"
	    "ArtePowi" "BuddLind" "CalcDich" "CalcDichDuet" "CercChin" "ChamPisi"
	    "ChamPisiDevo" "ChamPisiGold" "ClerThom" "ClerThomDele" "DahlHsro"
	    "EuryChin" "EuryEmar" "EuryJapo" "EuryJapoMout" "EuryLitt" "HamaChin"
	    "HamaInte" "IlexCorn" "IlexCornCari" "IlexCornCariFoob"
	    "IlexCornCariZook" "IlexCornDbur" "IlexCornNeed" "IlexCornOspr"
	    "IlexCornRotu" "IlexEmar" "LoroChin" "LoroChinRubr" ])



