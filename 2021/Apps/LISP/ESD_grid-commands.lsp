;;
;;
;;

;***
;***Grids
;***

(defun c:esd_grid ()
 (command "ucs" "world")
 (command "layer" "M" "G-REFR" "")
 (command "xref" "o" "R:\\Grids\\DWG\\KC_Lidar_key.dwg" "0,0" "" "" "")
 (command "zoom" "o" "l" "")
 (command "draworder" "l" "" "back")
)

;***
;***Aerials
;***

(defun c:esd_rstr ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (setvar "filedia" 0)
  (command "layer" "M" "G-RSTR" "")
  (princ "\nLoading Imagery...")
  (initget 1 "2017 2015 2014 2013 2012 2009 OTHER")
  (setq IMAGE (getkword "\nChoose [2017/2015/2014/2013/2012/2009/OTHER]: "))
  (cond
  ( (= IMAGE "2017") (setq GRDREF (getfiled "Pick a Image File" "//coacd.org/gis/Raster/Aerials/2017_02_6IN_TC_CIR/Austin_6in/jpeg2000/" "jp2" 8))
		(setq MAPPATH GRDREF))
	( (= IMAGE "2015") (setq GRDREF (getfiled "Pick a Image File" "//coacd.org/gis/Raster/Aerials/2015_02_6IN_TC_CIR/jp2/" "jp2" 8))
		(setq MAPPATH GRDREF))
	( (= IMAGE "2014") (setq GRDREF (getfiled "Pick a Image File" "//coacd.org/gis/Raster/Aerials/2014_07_1FT_TC_CIR/jp2/" "jp2" 8))
		(setq MAPPATH GRDREF))
	( (= IMAGE "2013") (setq GRDREF (getfiled "Pick a Image File" "//coacd.org/gis/Raster/Aerials/2013_03_6IN_TC_CIR/" "jp2" 8))
		(setq MAPPATH GRDREF))
	( (= IMAGE "2012") (setq GRDREF (getfiled "Pick a Image File" "//coacd.org/gis/Raster/Aerials/2012_01_6IN_TC/" "jp2" 8))
		(setq MAPPATH GRDREF))
	( (= IMAGE "2009") (setq GRDREF (getfiled "Pick a Image File" "//coacd.org/gis/Raster/Aerials/2009_02_6IN_TC/" "sid" 8))
		(setq MAPPATH GRDREF))
	( (= IMAGE "OTHER") (setq GRDREF (getfiled "Pick a Image File" "" "" 8))
		(setq MAPPATH GRDREF))
	)
  (command "_mapiinsert" MAPPATH "")
  (command "zoom" "o" "l" "")
  (command "draworder" "l" "" "back")
  (setvar "filedia" 1)
  (princ)
)




;***
;***Planimetrics
;***

(defun c:esd_plan ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (princ "\n Loading Planimetrics...")
  (initget 1 "ALL ROAD TEXT")
  (setq CAD (getkword "\nChoose [ALL/ROAD/TEXT]: "))
  (cond
	( (= CAD "ALL") (setq GRDREF (getfiled "Pick a CAD File" "Q:/Planimetrics/2017/DWG/" "dwg" 8))
		(setq MAPPATH GRDREF))
        ( (= CAD "ROAD") (setq GRDREF "Q:/Maps/Streets/DWG/ROAD.dwg")
		(setq MAPPATH GRDREF))
        ( (= CAD "TEXT") (setq GRDREF "Q:/Maps/Streets/DWG/ROAD-TEXT.dwg")
		(setq MAPPATH GRDREF))
  )
  (command "adedrawings" "AT" MAPPATH "X")
  (command "adequery" "C" "D" "L" "W" "C" PAUSE PAUSE "X" "E" "D")
  (command "adedrawings" "DE" "*" "X")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)

;***
;***Planimetrics_old
;***

(defun c:esd_plan_old ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (princ "\n2012 Grid Planimetrics...")
  (setq GRDREF (getfiled "Pick a CAD File" "Q:/Planimetrics/2013/DWG/" "dwg" 8))
  (setq MAPPATH GRDREF)
  (command "adedrawings" "AT" MAPPATH "X")
  (command "adequery" "C" "D" "L" "W" "C" PAUSE PAUSE "X" "E" "D")
  (command "adedrawings" "DE" "*" "X")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)

;***
;***Contours
;***

(defun c:esd_cont ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (princ "\n2012 Grid Contours...")
  (setq GRDREF (getfiled "Pick a CAD File" "Q:/Elevation/2012/DTM_CONTOURS/DWG/" "dwg" 8))
  (setq MAPPATH GRDREF)
  (command "adedrawings" "AT" MAPPATH "X")
  (command "adequery" "C" "D" "L" "W" "C" PAUSE PAUSE "X" "E" "D")
  (command "adedrawings" "DE" "*" "X")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)



;***
;***DTM
;***

(defun c:esd_dtm ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (princ "\n2012 DTM Points...")
  (setq GRDREF (getfiled "Pick a CAD File" "Q:/Elevation/2012/DTM_POINTS/DWG/" "dwg" 8))
  (setq MAPPATH GRDREF)
  (command "adedrawings" "AT" MAPPATH "X")
  (command "adequery" "C" "D" "L" "W" "C" PAUSE PAUSE "X" "E" "D")
  (command "adedrawings" "DE" "*" "X")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)


;***
;***Insert Cadastre
;***

(defun c:esd_prop ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (setvar "filedia" 0)
  (setvar "cmddia" 0)   
  (princ "\nBringing in GIS CAD Data...")
  (initget 1 "TCAD WCAD WCAD-TEXT ZONING ESMT ESMT-TEXT")
  (setq GRDREF (getkword "\nChoose [TCAD/WCAD/WCAD-TEXT/ZONING/ESMT/ESMT-TEXT]: "))
  (cond
	( (= GRDREF "TCAD") (setq MAPPATH (strcat "Q:\\Cadastre\\TCAD\\DWG\\" GRDREF)))
	( (= GRDREF "WCAD") (setq MAPPATH (strcat "Q:\\Cadastre\\WCAD\\DWG\\" GRDREF)))
	( (= GRDREF "WCAD-TEXT") (setq MAPPATH (strcat "Q:\\\Cadastre\\WCAD\\DWG\\" GRDREF)))
	( (= GRDREF "ZONING") (setq MAPPATH (strcat "Q:\\Cadastre\\Zoning\\" GRDREF)))
	( (= GRDREF "ESMT") (setq MAPPATH (strcat "Q:\\Cadastre\\Easement\\DWG\\" GRDREF)))
	( (= GRDREF "ESMT-TEXT") (setq MAPPATH (strcat "Q:\\Cadastre\\Easement\\DWG\\" GRDREF)))
	)
  (command "adedrawings" "AT" MAPPATH "X")
  (command "adequery" "C" "D" "L" "W" "C" PAUSE PAUSE "X" "E" "D")
  (command "adedrawings" "DE" "*" "X")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)














(DEFUN C:LIDAR-pln ()
 (command "-insert" "l:\\autodesk\\autocad civil 3d\\2014\\tool\\lidar\\esd_lidar" "0,0" ^c^c)
 (command "-insert" "l:\\autodesk\\autocad civil 3d\\2014\\tool\\lidar\\ncs_lidar" "0,0" ^c^c)
 (command "-purge" "b" "lidarlayers" "n" "")
 (prompt "\nDONE!")
)
;_ end of defun


(DEFUN C:jl3dpc ()
 (command "convert3dpolys" (ssget "x" '((8 . "VA-TOPO-MAJR*,VA-TOPO-MINR*"))) "")
 (command "pedit" "m" (ssget "x" '((8 . "VA-TOPO-MAJR*,VA-TOPO-MINR*"))) "" "j" "" "l" "on" "")
 (command "-layer" "ltype" "hidden" "VA-topo-majr,VA-topo-majr-depr" "")
 (command "-layer" "ltype" "hidden2" "VA-topo-minr,VA-topo-minr-depr" "")
)

(DEFUN C:jl3dp ()
 (command "convert3dpolys" (ssget "x" '((0 . "polyline"))) "")
 (command "pedit" "m" (ssget "x" '((0 . "line,arc"))) "" "y" "")
 (prompt "\nConverted all polylines, lines and arcs to lwpolylines!!!")
 (command "pedit" "m" (ssget "x" '((8 . "VA-ROAD-~PVD"))) "" "j" "" "l" "on" "")
 (prompt "\nJoined all VA-ROAD-~PVD polylines, lines and arcs to lwpolylines!!!")
 (command "pedit" "m" (ssget "x" '((8 . "VA-SITE-OBSC-HIDE"))) "" "j" "" "l" "on" "")
 (prompt "\nJoined all VA-SITE-OBSC-HIDE polylines, lines and arcs to lwpolylines!!!")
 (command "pedit" "m" (ssget "x" '((8 . "VA-BLDG-LRGE"))) "" "j" "" "l" "on" "")
 (prompt "\nJoined all VA-BLDG-LRGE polylines, lines and arcs to lwpolylines!!!")
 (command "pedit" "m" (ssget "x" '((8 . "VA-BLDG-OTLN"))) "" "j" "" "l" "on" "")
 (prompt "\nJoined all VA-BLDG-OTLN polylines, lines and arcs to lwpolylines!!!")
)


;;;
;;;RENAME all Lidar DTM layers to NCS standard layers
;;;

(DEFUN C:LIDAR-dtm ()
 (setvar "cmdecho" 0)
 (command "-insert" "l:\\autodesk\\autocad civil 3d\\2014\\tool\\lidar\\esd_lidar" "0,0" ^c^c)
 (command "-insert" "l:\\autodesk\\autocad civil 3d\\2014\\tool\\lidar\\ncs_lidar" "0,0" ^c^c)

 (command "-laymrg" "n" "DTM_BREAKLINE" "" "n" "VA-TINN-BRKL" "y")

 (command "-laymrg" "n" "DTM_MASSPOINT" "" "n" "VA-TINN-SPOT" "y")

 (command "-laymrg" "n" "MASSPOINT" "" "n" "VA-TINN-SPOT" "y")

 (command "zoom" "e")

 (command "-purge" "all" "" "n")
 (command "-purge" "all" "" "n")
 (setvar "cmdecho" 1)
)


;***
;***Grid Key - Lidar 2009
;***

(defun c:gkl04 ()
(command "ucs" "world")
(command "xref" "o" "\\\\coacd.org\\dfs\\autodesk\\autocad civil 3d\\2014\\Tool\\Lidar\\gridkey_2004_Lidar.dwg" "0,0" "" "" "")
(command "ucs" "previous")
)

;***
;***Grid Key - COA Grid
;***

(defun c:gkcoa ()
(command "ucs" "world")
(command "xref" "o" "\\\\coacd.org\\dfs\\gis-cad\\grids\\dwg\\gridkey_200_COA.dwg" "0,0" "" "" "")
(command "ucs" "previous")
)


;***
;***Grid Key - Travis County Streets
;***

(defun c:gktrst ()
(command "ucs" "world")
(command "xref" "o" "\\\\coacd.org\\dfs\\autodesk\\autocad civil 3d\\Tool\\Lidar\\gridkey_StreetCL_Travis.dwg" "0,0" "" "" "")
(command "ucs" "previous")
)


;***
;***Grid Key - City of Austin Jurisdictions
;***

(defun c:gkjuris ()
(command "ucs" "world")
(command "xref" "o" "\\\\coacd.org\\dfs\\gis shape files\\city of austin gis\\regional\\austin_jurisdictions\\austin_jurisdictions.dwg" "0,0" "" "" "")
(command "ucs" "previous")
)


;***
;***Grid Key - Austin Mapsco
;***

(defun c:gkmapsco ()
(command "ucs" "world")
(command "xref" "o" "\\\\coacd.org\\dfs\\gis-cad\\grids\\dwg\\gridkey_mapsco.dwg" "0,0" "" "" "")
(command "ucs" "previous")
)


;***
;***Insert LIDAR Planmetrics W/Contours
;***

(defun c:ILPLNF ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (princ "\nPlanimetrics-2004 PANEL W/CONTOURS insert & explode command...")
  (setq GRDREF (getstring "Enter PLN Grid Reference: "))
  (setq MAPPATH (strcat "l:/lidar/dwg/pln/" GRDREF "_PLN.dwg"))
  (command "_.insert" MAPPATH "0,0,0" "1" "" "")
  (command "zoom" "o" "l" "")
  (command "explode" "l" "")
  (command "zoom" "p")
  (alert "/nLidar Planimetrics " grdref " has been imported.")
)


;***
;***Insert LIDAR Digital Terrain Model
;***

(defun c:ILDTM ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (princ "\nDigital Terrain Model-2004 PANEL insert & explode command...")
  (setq GRDREF (getstring "Enter DTM Grid Reference: "))
  (setq MAPPATH (strcat "l:/lidar/dwg/dtm/" GRDREF "_DTM.dwg"))
  (command "_.insert" MAPPATH "0,0,0" "1" "" "")
  (command "zoom" "o" "l" "")
  (command "explode" "l")
  (command "zoom" "p")
  (alert "/nLidar Digital Terrain Model " grdref " has been imported.")
)




;***
;***Clean Lidar for Location Map
;***

(defun c:cl-locmap ()
 (setq cllm (ssget "x" '((8 . "contour*,spot*,*sidewalk,*centerline,*_non_*,*topo*,*swlk*,*cntr*"))))
 (command "erase" cllm "")
)


;***
;***Xref LIDAR Planmetrics
;***

(defun c:XLPLN ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (princ "\nPlanimetrics-2013 PANEL REFERENCE ONLY command...")
  (setq GRDREF (getstring "Enter PLN Grid Reference (DO NOT ENTER THE LAST 2 DIGITS: "))
  (setq MAPPATH (strcat "//coacd.org/dfs/gis-cad/planimetrics/2013/dwg/" GRDREF ".dwg"))
  (command "_.xref" "o" MAPPATH "0,0,0" "" "" "")
  (alert "/nLidar Planimetrics " grdref " has been xrefed.")
)

;***
;***Insert LIDAR Planmetrics W/O Contours
;***

(defun c:ILPLN ( / GRDREF MAPPATH)
  (setvar "cmdecho" 1)
  (princ "\nPlanimetrics-2004 PANEL insert & explode command...")
  (setq GRDREF (getstring "Enter PLN Grid Reference (DO NOT ENTER '_' OR THE LAST 2 DIGITS: "))
  (setq MAPPATH (strcat "//coacd.org/dfs/gis-cad/planimetrics/2013/dwg/" GRDREF ".dwg"))
  (command "_.insert" MAPPATH "0,0,0" "1" "" "")
  (command "zoom" "o" "l" "")
  (command "explode" "l" "")
  (command "zoom" "p")
  (alert "/nLidar Planimetrics " grdref " has been imported.")
)

;***
;***Xref COA Map
;***
;***

(defun c:icoam ()
(command "ucs" "world")
(command "xref" "o" "m:\\autodesk\\autocad civil 3d\\Tool\\Lidar\\gridkey_map_coa.dwg" "0,0" "" "" "")
(command "ucs" "previous")
)
