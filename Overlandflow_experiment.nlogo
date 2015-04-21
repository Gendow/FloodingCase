; 
; January 2009
; Wageningen UR, Centre for Geo-information
; Arend Ligtenberg: arend.ligtenberg@wur.nl
; For demonstration and education purposes only
;;============================================

extensions [gis]

patches-own 
[
  elevation
  ;roughness
]

turtles-own
[
  water-height
]

globals 
[
  ahn
  rough
  cellSize
  nodatavalue
  border 
  start-water-height
  waterQuantity
  waterheight
  max-water-height
  sumDeltaStreamHeight
  totStreamArea
  exp-count
  elevationDataSetName
  roughnessDataSetName
  exportGridName
  experimentnr
  maxRough
  minRough
  NrOfSecsPerTick
  widthHeightConversionParam
  quantityConversionParam
  nrOfElevationDataSets
  fileNr
  fileNrOffset
  export-frequentie
]


;;SOME BASIC SETTINGS
;;===================
to init
  ;; some parameters that define mapping for cm3 to m3 or cm to m depending on the input data
  ;;-----------------------------------------------------------------------------------------
  set   quantityConversionParam 1000000
  set   widthHeightConversionParam 1
  
  ;; mapping for simulation tick to real world time
  ;;-----------------------------------------------
  set   NrOfSecsPerTick 10
  
  ;; cell size of the input grids
  ;;-----------------------------
  set cellSize  1000
  
  ;;the input elevation grid
  ;;-------------------------
  set elevationDataSetName "export/defaultDEM.asc" ;path need to exist!
  

  ;;name of the export grids showing the results of the flooding (will be postfixed by the clocktick)
  ;;-------------------------------------------------------------------------------------------------
  set exportGridName ""  ;;path need to exist!
  
  
  ;; the default waterheight at the dam
  ;;----------------------------------------
  set waterheight 500 ; 
  
  ;; declare the number of elevation data set realisations that need to be run
  ;;--------------------------------------------------------------------------
  set nrOfElevationDataSets 100
 
  ;; set the default nr of ticks after which a output grid need to be exported
  ;;--------------------------------------------------------------------------
  set export-frequentie 100 
  reset-ticks
  end



;; START OF THE DYNAMIC MODEL CALCULATIONS (clicking the "go" button)
;;===================================================================  
to go
  if ticks <= 2160
  [
    set export-frequentie 2160
    runmodel
    do-plots
    tick
  ]
end
 
;; START OF THE DYNAMIC MODEL CALCULATIONS USING A PARAMETER FILE (clicking the "run experiment" button)
;;===================================================================  
to go-batch
    init

    set fileNr 1
    file-open "experiment.txt"
    let runtime file-read  
    set export-frequentie runtime
    while [not file-at-end?]
    [
      show word "processing: " experimentnr 
      set experimentnr file-read
      ;the path to te directory with the different realisations of the DEM
      set elevationDataSetName word (word "d:/flowexperiment/input/TestDem" experimentnr) ".asc"
      set influx file-read
      show word "...elevationdata: " elevationDataSetName
      show word "...influx m3/sec: " influx   
      ;;call the loading routing to load geo-data
      ;;------------------------------------------
      loaddata
      ;;define the name for the result grid
      ;;-----------------------------------
      ;the path and output name for the export ascii grid file
      set exportGridName  word(word (word (word (word "d:/flowexperiment/output/result_" experimentnr)"_") influx) "_") fileNr
      ;;run the model 
      ;;-------------
      ;; check that model does not run more than number of ticks specified in the runtime variable
      while [ticks <= runtime]
      [
        runmodel
        tick  
      ]
      set fileNr (fileNr + 1)
      reset  
     ]
     file-close
     ; show timer

      
end

;; MAIN PROCEDURE FOR CALLING ALL OTHER PROCEDURES AND MANAGING PROCESS
;;---------------------------------------------------------------------
to runmodel
   set waterQuantity  (influx * quantityConversionParam ) * NrOfSecsPerTick
   set start-water-height waterheight * widthHeightConversionParam
   if not any? turtles
      [ ask patch x-breach y-breach
          [  
            sprout 1 [set color blue set water-height start-water-height ]  
            set max-water-height [elevation] of patch x-breach y-breach + start-water-height        
          ] 
      ]
      if count turtles > 0 
      [
        set sumDeltaStreamHeight  0

        ;;first calculate the current flooded area in terms of the total flooded height
        ;;this is needed to be able to estimate the speed wave
        ask turtles [calcTotalFlowArea]

        ;next call the routine the distributes the water over the flowarea based
        ;;on the ratio between local flow area and flow streamed area
        ask turtles [ flow ]
        
       ;;some essential bookkeeping
       ;don't allow the water to become highter than the level of the "river"
       ;---------------------------------------------------------------------
       ask turtles 
        [
          if water-height > start-water-height 
          [   
            set water-height start-water-height
          ]
        ]
         
          ;; when the water reach the edge of the world
          ;; kill it so it exits the system and we
          ;; don't get pooling at the edges
          ;---------------------------------------------
        ask border
          [
          ask turtles-here [ die ]
          ]

          ;call export routine if switched on
          ;----------------------------------
          if (export-step = true)
          [
            do-export
          ]
    ]
  end

;;CALCULATE TOTAL FLOW AREA
;;=========================
to calcTotalFlowArea
   let centreWaterSurfaceHeight water-height + [elevation] of patch-here
   let nbhScan neighbors4
   let nbhDistributionArea 0
   let nbhWaterSurfaceHeight 0
   ask nbhScan
   [
     ifelse count turtles-here > 0
     [
       set nbhWaterSurfaceHeight elevation + max [water-height] of turtles-here
     ]
     [
       set nbhWaterSurfaceHeight elevation
     ]   
     let deltaWaterSurfaceHeight centreWaterSurfaceHeight - nbhWaterSurfaceHeight
     if deltaWaterSurfaceHeight > 0
     [
        set sumDeltaStreamHeight sumDeltaStreamHeight + deltaWaterSurfaceHeight

     ]
     
   ]
end


;;DISTRIBUTE INCOMMING WATER 
;;==========================
;; turtle procedure  
;;this is a simple CA based algorithem that calculates the overlandflow based on weighted distribution
;;of water over cells in the neighbourhood. 
;;------------------------------------------------------------------------------------------------------
to flow 
   let feedElevation [elevation] of patch-here
   let feedWater water-height
   let feed_water_surface_height (feedElevation + feedWater)
   if feed_water_surface_height > max-water-height 
   [
     set water-height max-water-height
   ]
   let target neighbors4
   ;let total_delta_elevation 0
   
   ; now we can start distibuting the water at every tick of the machine
   ask target
   [
     ;;show roughnessfactor(roughness)
      ;if the is no water a this patch i.e there is no turtle then bring some water to it by sprouting a new turtle and provide it with an appropriate water-height
      ;-------------------------------------------------------------------------------------------------------------------------------------------------------------
      ifelse count turtles-here = 0
      [
         ;if the elevation of the neighborhoodpatch is lower than the surface level of the water: there is flow possible 
         ;--------------------------------------------------------------------------------------------------------------
         if elevation < (feed_water_surface_height)
         [
         
           let resulting_water_height (((feed_water_surface_height - elevation) / sumDeltaStreamHeight) * waterQuantity) / cellsize ^ 2
           if ( resulting_water_height + elevation) > feed_water_surface_height
           [
             set resulting_water_height (feed_water_surface_height - elevation) - 0.01
           ]
            ;deliver a new turtle
            ;--------------------
            sprout 1 [set color red set water-height resulting_water_height]
         ]
      ]  
      [
        ; if there is allready water than we can do this:
        ;------------------------------------------------
        ask turtles-here 
        [ 
          ;first calculate het height of the water level at this patch
          ;-----------------------------------------------------
          let local_water_surface_height (water-height + elevation)        
         
          ;if the water level at this patch is lower than the level at the centre patch then there is flow
          ;-----------------------------------------------------------------------------------------------
          if  local_water_surface_height < (feed_water_surface_height)
          [
             let resulting_water_height (((((feed_water_surface_height - local_water_surface_height) / sumDeltaStreamHeight) * waterQuantity) / cellsize ^ 2) + water-height)
             if resulting_water_height + elevation > feed_water_surface_height
               [
                 set resulting_water_height (feed_water_surface_height - elevation)
               ]
             set water-height resulting_water_height
             set color scale-color blue ((feed_water_surface_height - elevation) + 1)((max-water-height ) + 2 ) (0)
          ]
        ]
      ]    
   ] 
    end


;;this reporter function that calculates the effect of the roughess
;;factor. Currently just a linear thing as we don't have
;;an idea how roughness influencing current and speed of the
;;water
;to-report roughnessfactor [roughtness]
;  let normRoughness ( (maxRough - roughness) /   (maxRough -  minRough) )
;  let rf (normRoughness * (1 / roughWeight ))
;  report rf
;end


;;===================================================================================================================================================
;; BELOW ARE JUST CONVIENIANCE ROUTINES THAT PROVIDE FOR IMPORTING AND EXPORTING AND VISUALIZING GEODATA
;; YOU DON'T NEED TO CHANGE SOMETHING HERE
;;===================================================================================================================================================

;;THIS IS WHAT HAPPENS IF YOU CLICK THE RESET BUTTON
;;==================================================
to reset
  clear-drawing
  clear-turtles
  clear-all-plots 
  reset-ticks
  ;file-close-all
  set exp-count 1
  reset-ticks
  do-plots
end


;; Manually loading default data ifk you press the load button
;;---------------------------------------------------------------
to load
  init
  loaddata
end

;;LOADING EN COLORRAMPING ARCGIS GRIDs
;;================================================
to loaddata
 clear-patches

 show "loading elevation from file..."
 set ahn gis:load-dataset elevationDataSetName

 ;show "      loading elevation data from file"
 ;set rough gis:load-dataset roughnessDataSetName
 ;show word "      rows: "gis:height-of ahn 
 ;show word "      columns: "gis:width-of ahn
 
 ;;get the min and max in the rough dataset
 ;; and store it in the globel variables  maxRough and  minRough
 ;;we need it later
 ;;---------------------------------------------------------------
 ;set maxRough gis:maximum-of rough
 ;set minRough gis:minimum-of rough
 
 ;; get the min and max elevation just we need it 
 ;; to tune the color ramping of the elevation data
 ;;----------------------------------------------------
 let min-elevation gis:minimum-of ahn
 let max-elevation gis:maximum-of ahn
 show word "      lowest :" min-elevation
 show word "      highest :" max-elevation 

 ;;put everything (elevation en roughness on the netlogo
 ;;world. You need to enter the right extend of the word 
 ;;beforehand throught the gui as netlogo does not allow 
 ;;you to do by scriptin
 ;;----------------------------------------------------------------
 gis:set-world-envelope (gis:raster-world-envelope ahn 0 0)
 gis:apply-raster ahn elevation
 ;gis:apply-raster rough roughness

 ;;some stuff with coloring to create a 'nice' map
 ;;eliminate the "stuwwal" it affect the color ramping
 ;;----------------------------------------------------
;; ask patches
;; [
;;   if elevation > 2000 [set elevation -9999]
;; ]
  
  ;;use the scale-color operator to assign colors based
  ;;on the elevation
  ;;---------------------------------------------------
  ask patches
  [ 
   if (elevation > -9999)
   [ 
     set pcolor scale-color brown elevation min-elevation 2000 
   ] 
 ]
  set-default-shape turtles "square"
  show "    done loading data"

  set exp-count 1
  set border patches with [ count neighbors != 8 ]
end


;;EXPORT THE FLOODING GRIDS TO ARCGIS FORMAT
;;=========================================    
to do-export
   if export-frequentie > 0
    [
       if (exp-count = export-frequentie)
        [
           show "exporting to grid"
           ;;create raster data set according the input ahn
           ;;----------------------------------------------- 
           let dummy gis:create-raster gis:width-of ahn gis:height-of ahn  gis:envelope-of ahn
           let x 0
           repeat (gis:width-of dummy)
           [
             let y 0
             repeat (gis:height-of dummy)
             [
               if patch x y != nobody
               [
                 ifelse [elevation] of patch x y  > 0
                 [              
                   ifelse any? turtles-on patch x y
                   [
                     let waterTurtles turtles-on patch x y
                     gis:set-raster-value dummy x abs y  max [water-height] of waterTurtles
                   ]
                   [
                     gis:set-raster-value dummy x abs y -9999
                   ]
                  ]
                  [
                    gis:set-raster-value dummy x abs y -9999
                  ]
                ]
                   set y  y - 1
              ]
                set x x + 1
              ]
              
           gis:store-dataset dummy word(word exportGridName "_") (ticks + 1) 
           set exp-count 0
        ]
           set exp-count exp-count + 1
       ]
end




to do-plots
set-current-plot "watered-patches"
set-current-plot-pen "watered"
plot count turtles
end
@#$#@#$#@
GRAPHICS-WINDOW
404
32
1304
493
-1
-1
2.5
1
10
1
1
1
0
0
0
1
0
355
-171
0
0
0
1
ticks
30.0

BUTTON
78
31
154
64
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
158
31
233
64
NIL
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
11
79
376
259
watered-patches
time
patches
0.0
500.0
0.0
5000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""
"watered" 1.0 0 -13345367 true "" ""

BUTTON
12
32
75
65
Load
load
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
15
503
136
536
export-step
export-step
0
1
-1000

INPUTBOX
10
402
92
462
x-breach
308
1
0
Number

INPUTBOX
107
404
189
464
y-breach
-36
1
0
Number

INPUTBOX
10
332
92
392
influx
2697
1
0
Number

BUTTON
236
31
355
64
Run experiment
reset-timer\ngo-batch\nshow word \"elapsed # secs: \" timer
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
16
482
269
516
Settings for exporting
14
0.0
1

MONITOR
16
563
105
608
NIL
timer
17
1
11

@#$#@#$#@
## WHAT IS IT?

## HOW IT WORKS

## HOW TO USE IT

## THINGS TO NOTICE

## THINGS TO TRY

## EXTENDING THE MODEL

## NETLOGO FEATURES

## RELATED MODELS

## CREDITS AND REFERENCES
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.3
@#$#@#$#@
startup
setup
set draw? true
repeat 200 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>reset</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="quantityConversionParam">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export-frequentie">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influx">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="widthHeightConversionParam">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export-step">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waterheight">
      <value value="500"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
