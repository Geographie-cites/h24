breed [butchers butcher]
breed [parisians parisian]
patches-own [
  act1
  act2
  propGlobalMeaterPatch
  propGlobalVeggiePatch
  propLocalMeaterPatch
  propLocalVeggiePatch
  switchingParisians
  ;; attractivité par niveau d'éducation du patch
  AttractEdu0
  AttractEdu1
  AttractEdu2]
parisians-own [ act1? act2? PatchResidence PatchA1 PatchA2 EduLevel Meat?  probaA0 probaA1 probaA2 ;PatchEduLevel
  ]
globals [ size-city meatersA1 meatersA2 meatersA1A2 meatersImmo Edu0 Edu1 Edu2 switch-To-Meat switch-To-Veg]

;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;

to setup
  clear-all
  reset-ticks
  set switch-To-Meat 0
  set switch-To-Veg 0
  setup-patches
  setup-parisians
  ;;setup-butchers
  Update-globals
end


to go
  initialise-Switches
  GetActive
  Update-Color
  Update-globals
  tick
end

;;;;;;;;;;;;;;;;
;; RANDOM
;;;;;;;;;;;;;;;;

to-report random-normal-in-bounds [mid dev mmin mmax]
  let result random-normal mid dev
  if result < mmin or result > mmax
    [ report random-normal-in-bounds mid dev mmin mmax ]
  report result
end


to-report xy-gaussian-excentre [mid dev]
  let rxnorm random-normal-in-bounds mid dev 0 1
  let rynorm random-normal-in-bounds mid dev 0 1

  ifelse (rxnorm < mid) [set rxnorm mid + rxnorm ][ set rxnorm rxnorm - mid ]
  ifelse (rynorm < mid) [set rynorm mid + rynorm ][ set rynorm rynorm - mid ]

  report list round(rxnorm * (world-width - 1)) round( rynorm * (world-width - 1))
end

to-report xy-gaussian [mid dev]
  let rxnorm random-normal-in-bounds mid dev 0 1
  let rynorm random-normal-in-bounds mid dev 0 1

  report list round(rxnorm * (world-width - 1)) round(rynorm * (world-width - 1))
end

to-report reportXYParisian
  let xy []
  if xyParisians = "gaussian" [
    set xy xy-gaussian meanPGaussian stdPGaussian
  ]

  if xyParisians = "invGaussian" [
    set xy xy-gaussian-excentre meanPGaussian stdPGaussian
  ]

  if xyParisians = "uniform" [
    set xy list random-xcor random-ycor
  ]

  report xy

end

to-report reportXYA1
  let xy []
  if xyA1 = "gaussian" [
    set xy xy-gaussian meanA1Gaussian stdA1Gaussian
  ]

  if xyA1 = "invGaussian" [
    set xy xy-gaussian-excentre meanA1Gaussian stdA1Gaussian
  ]

  if xyA1 = "uniform" [
    set xy list random-xcor random-ycor
  ]

  report xy

end


;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;

to setup-patches
  set size-city max-pxcor + 1
  let NCells size-city * size-city
  let N2 (%PatchA2 / 100) * NCells
  let N1 (%PatchA1 / 100) * NCells

  ;; all equal to zero
  ask patches [
    set act1 0 set act2 0 set switchingParisians 0
    set AttractEdu0 0 set AttractEdu1 0 set AttractEdu2 0
  ]


  let N1empty N1
  while [N1empty > 0 ] [
    let reportedPatch reportXYA1
    ask patch item 0 reportedPatch item 1 reportedPatch
     [ if act1 = 0 [
         set act1 1
         set N1Empty N1Empty - 1 ]
     ]
  ]

  repeat N2 [ ask one-of patches with [act2 = 0] [set act2 1]]

  repeat (1 / 3) * NCells [ ask one-of patches [set AttractEdu0 1] ]
  repeat (1 / 3) * NCells [ ask one-of patches [set AttractEdu1 1] ]
  repeat (1 / 3) * NCells [ ask one-of patches [set AttractEdu2 1] ]

  ;ask patches with [AttractEdu0 = 1 ] [set pcolor white]
  ;ask patches with [AttractEdu1 = 1] [set pcolor grey]
  ;ask patches with [AttractEdu2 = 1 ] [set pcolor black]

end

to color-activity

  ask patches  [set pcolor white]
  if colorA1 [
    ask patches with [act1 = 1 and act2 = 0] [set pcolor black]
  ]

  if colorA2 [
    ask patches with [act1 = 0 and act2 = 1] [set pcolor grey]
  ]

  if colorA1A2 [
    ask patches with [act1 = 1 and act2 = 1] [set pcolor red]
  ]

  ;ask patches with [act1 = 1 and act2 = 1] [set pcolor 27]
  ;ask patches with [act1 = 0 and act2 = 1] [set pcolor 67]
  if colorP [
    ask parisians [
      ask [PatchResidence] of self [
        set pcolor orange
      ]
    ]
  ]
end

to-report get-probability [#eduLevel]
  let A1 random-float 1
  let A2 random-float 1
  let _act1 0
  let _act2 0
  let probability 0

  if #eduLevel = 0 [ set probability ProbabilityOfActivityEdu0]
  if #eduLevel = 1 [ set probability ProbabilityOfActivityEdu1]
  if #eduLevel = 2 [ set probability ProbabilityOfActivityEdu2]

  ifelse A1 < probability [set _act1 1][set _act1 0]
  ifelse A2 < probability [set _act2 1][set _act2 0]

  report list _act1 _act2
end

to setup-parisians

   create-parisians nPeople [
     set color black
     let xycalculated reportXYParisian
     setxy item 0 reportXYParisian item 1 reportXYParisian
     set PatchResidence patch-here
     set EduLevel random 3
   ]

   ;;REWRITE ?
   ask parisians[

     ;; get probability by EduLevel
     let result get-probability EduLevel
     set act1? item 0 result
     set act2? item 1 result
     let randomMeat random-float 1

     let pA0 0
     let pA1 0
     let pA2 0

     if EduLevel = 0 [
       set pA0 1
       set pA1 pA0 * (1 + ElasticityEduLevel)
       set pA2 pA0 * (1 + ElasticityEduLevel) ^ 2

     ifelse randomMeat < probaMeatEdu0  [set Meat? 1] [set Meat? 0]

     ]

     if EduLevel = 1 [
       set pA0 pA1 * (1 + ElasticityEduLevel)
       set pA1 1
       set pA2 pA1 * (1 + ElasticityEduLevel)

       ifelse randomMeat < probaMeatEdu1 [set Meat? 1] [set Meat? 0]
     ]

     if EduLevel = 2 [
       set pA1 pA2 * (1 + ElasticityEduLevel)
       set pA0 pA2 * (1 + ElasticityEduLevel) ^ 2
       set pA2 1

       ifelse randomMeat < probaMeatEdu2 [set Meat? 1] [set Meat? 0]

     ]

     ;; probabilité d'aller tirer patch activité d'éducation 0
     set probaA0 pA0 / (pA0 + pA1 + pA2)
     set probaA1 pA1 / (pA0 + pA1 + pA2)
     set probaA2 pA2 / (pA0 + pA1 + pA2)



   ;; dans quel niveau education on ira travaille pour le patch 1
   let EduPatchForA1 random-float 1
   ifelse EduPatchForA1 < probaA0 [
  ;   set patchEduLevel 0
     ifelse any? patches with [act1 = 1 and AttractEdu0 = 1]
     [set PatchA1 one-of patches with [act1 = 1 and AttractEdu0 = 1]]
     [set PatchA1 one-of patches with [act1 = 1]]
   ][
    ifelse EduPatchForA1 < probaA1 [
  ;   set patchEduLevel 1
    ifelse any? patches with [act1 = 1 and AttractEdu1 = 1]
     [set PatchA1 one-of patches with [act1 = 1 and AttractEdu1 = 1]]
     [set PatchA1 one-of patches with [act1 = 1]]
   ][
  ; set patchEduLevel 2
   ifelse any? patches with [act1 = 1 and AttractEdu2 = 1]
     [set PatchA1 one-of patches with [act1 = 1 and AttractEdu2 = 1]]
     [set PatchA1 one-of patches with [act1 = 1]]
   ]
   ]


   let EduPatchForA2 random-float 1
   ifelse EduPatchForA2 < probaA0 [
  ;   set patchEduLevel 0
     ifelse any? patches with [act2 = 1 and AttractEdu0 = 1]
     [set PatchA2 one-of patches with [act2 = 1 and AttractEdu0 = 1]]
     [set PatchA2 one-of patches with [act2 = 1]]
   ][
    ifelse EduPatchForA2 < probaA1 [
  ;   set patchEduLevel 1
    ifelse any? patches with [act2 = 1 and AttractEdu1 = 1]
     [set PatchA2 one-of patches with [act2 = 1 and AttractEdu1 = 1]]
     [set PatchA2 one-of patches with [act2 = 1]]
   ][
  ; set patchEduLevel 2
   ifelse any? patches with [act2 = 1 and AttractEdu2 = 1]
     [set PatchA2 one-of patches with [act2 = 1 and AttractEdu2 = 1]]
     [set PatchA2 one-of patches with [act2 = 1]]
   ]
   ]


 set color white
 ]

 ask parisians with [Meat? = 1] [set color red]
 ask parisians with [Meat? = 0] [set color white]

end

;to setup-butchers
;  create-butchers nPeople * EquipmentPerPerson
;  [setxy random-xcor random-ycor
;    set shape "box"
;    set color black
;    set size 0.01]
;end


to initialise-Switches
  set switch-To-Meat 0
  set switch-To-Veg 0
end

to GetActive
  doA1
  doA2
  comeBack
  ;ask patches [ set switchingParisians 0]
end

to doA1
  ask parisians with [act1? = 1] [move-to PatchA1]
  count-minority
  ask parisians with [act1? = 1] [Interact-and-update-eating-behaviour]
end

to doA2
  ask parisians with [act2? = 1] [move-to PatchA2]
  count-minority
  ask parisians with [act2? = 1] [
   Interact-and-update-eating-behaviour

   let EduPatchForA2 random-float 1
   ifelse EduPatchForA2 < probaA0 [
   ;set patchEduLevel 0
     ifelse any? patches with [act2 = 1 and AttractEdu0 = 1]
     [set PatchA2 one-of patches with [act2 = 1 and AttractEdu0 = 1]]
     [set PatchA2 one-of patches with [act2 = 1]]
   ][
    ifelse EduPatchForA2 < probaA1 [
   ;set patchEduLevel 1
    ifelse any? patches with [act2 = 1 and AttractEdu1 = 1]
     [set PatchA2 one-of patches with [act2 = 1 and AttractEdu1 = 1]]
     [set PatchA2 one-of patches with [act2 = 1]]
   ][
   ;set patchEduLevel 2
   ifelse any? patches with [act2 = 1 and AttractEdu2 = 1]
     [set PatchA2 one-of patches with [act2 = 1 and AttractEdu2 = 1]]
     [set PatchA2 one-of patches with [act2 = 1]]
   ]
   ]
   ]
end

to Interact-and-update-eating-behaviour

   if Global-Switch [
      let r-global random-float 1
      ifelse Meat? = 1 [
       let propGlobalVeggieOfMyPatch [propGlobalVeggiePatch] of patch-here
       if r-global * 100 < propGlobalVeggieOfMyPatch * k-global [set Meat? 0 set switch-To-Veg switch-To-Veg + 1 ]
     ] [
    let propGlobalMeaterOfMyPatch [propGlobalMeaterPatch] of patch-here
     if r-global * 100  < propGlobalMeaterOfMyPatch * k-global [set Meat? 1 set switch-To-Meat switch-To-Meat + 1 ]
    ]
   ]
     if Local-Switch [
         let r-local random-float 1
    ifelse Meat? = 1 [
       let propLocalVeggieOfMyPatch [propLocalVeggiePatch] of patch-here
         if r-local * 100  < propLocalVeggieOfMyPatch * k-local [set Meat? 0 set switch-To-Veg switch-To-Veg + 1 ]
     ] [
    let propLocalMeaterOfMyPatch [propLocalMeaterPatch] of patch-here
     if r-local * 100  < propLocalMeaterOfMyPatch * k-local [set Meat? 1 set switch-To-Meat switch-To-Meat + 1 ]
     ]
   ]
end

to comeBack
   ask parisians [move-to PatchResidence]
   count-minority
    ask parisians [
   Interact-and-update-eating-behaviour
    ]

end

to Update-Color
    ask parisians with [Meat? = 1] [set color red]
    ask parisians with [Meat? = 0] [set color white]
    count-minority
   ; let currentMeanMeat 50 ;(count parisians with [Meat? = 1]  * 100)/ count parisians
   ;let currentMeanVeggie 50;100 - currentMeanMeat
    ; ask patches with [propLocalMeaterPatch > currentMeanMeat] [set pcolor red]
    ;ask patches with [propLocalVeggiePatch > currentMeanVeggie] [set pcolor white]
end

to count-minority
  ask patches [
    set propGlobalMeaterPatch (count parisians-here with [Meat? = 1] * 100)/ count parisians with [Meat? = 1]
    set propGlobalVeggiePatch (count parisians-here with [Meat? = 0] * 100)/ count parisians with [Meat? = 0]
   ifelse any? parisians-here [
     set propLocalMeaterPatch (count parisians-here with [Meat? = 1] * 100)/ count parisians-here
      set propLocalVeggiePatch (count parisians-here with [Meat? = 0] * 100)/ count parisians-here
      ] [
      set propLocalMeaterPatch 0  set propLocalVeggiePatch 0
      ]
  ]
end

to Update-globals
 set meatersA1 round((count parisians with [Meat? = 1 and act1? = 1 and act2? = 0] * 100)/ count parisians with [act1? = 1 and act2? = 0])
 set meatersA2 round((count parisians with [Meat? = 1 and act1? = 0 and act2? = 1] * 100)/ count parisians with [act1? = 0 and act2? = 1])
 set meatersA1A2 round((count parisians with [Meat? = 1 and act1? = 1 and act2? = 1] * 100)/ count parisians with [act1? = 1 and act2? = 1])
 set meatersImmo round((count parisians with [Meat? = 1 and act1? = 0 and act2? = 0] * 100)/ count parisians with [act1? = 0 and act2? = 0])
 set Edu0 round((count parisians with [Meat? = 1 and EduLevel = 0 ] * 100)/ count parisians with [EduLevel = 0 ])
 set Edu1 round((count parisians with [Meat? = 1 and EduLevel = 1 ] * 100)/ count parisians with [EduLevel = 1 ])
 set Edu2 round((count parisians with [Meat? = 1 and EduLevel = 2 ] * 100)/ count parisians with [EduLevel = 2 ])
end
@#$#@#$#@
GRAPHICS-WINDOW
477
20
896
460
-1
-1
13.2
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
30
0
30
0
0
1
ticks
30.0

BUTTON
19
50
85
83
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
99
64
162
97
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
29
151
235
184
nPeople
nPeople
0
16000
200
100
1
NIL
HORIZONTAL

SLIDER
994
581
1166
614
EquipmentPerPerson
EquipmentPerPerson
0
1
0.5
0.1
1
NIL
HORIZONTAL

MONITOR
55
469
193
514
% meat-eaters A1A2
round((count parisians with [Meat? = 1 and act1? = 1 and act2? = 1] * 100)/ count parisians with [act1? = 1 and act2? = 1])
17
1
11

MONITOR
52
367
190
412
% meat-eaters A1
meatersA1
17
1
11

MONITOR
56
518
194
563
% meat-eaters A2
round((count parisians with [Meat? = 1 and act1? = 0 and act2? = 1] * 100)/ count parisians with [act1? = 0 and act2? = 1])
17
1
11

BUTTON
200
100
264
133
go10
repeat 10 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
268
396
339
429
shrink
ask parisians [set size 0.1]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
291
48
468
81
probaMeatEdu1
probaMeatEdu1
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
927
445
1099
478
k-global
k-global
0
0.2
0.2
0.01
1
NIL
HORIZONTAL

MONITOR
52
415
211
460
% meat-eaters Immobile
round((count parisians with [Meat? = 1 and act1? = 0 and act2? = 0] * 100)/ count parisians with [act1? = 0 and act2? = 0])
17
1
11

MONITOR
295
505
369
550
% meaters
round((count parisians with [Meat? = 1 ] * 100)/ count parisians)
17
1
11

PLOT
146
580
404
730
% Meat Eaters by Mobility
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"A1" 1.0 0 -10873583 true "" "plot meatersA1"
"A2" 1.0 0 -14070903 true "" "plot meatersA2"
"A1A2" 1.0 0 -15302303 true "" "plot meatersA1A2"
"Immo" 1.0 0 -955883 true "" "plot meatersImmo"

SLIDER
994
615
1166
648
%PatchA2
%PatchA2
0
100
33
1
1
NIL
HORIZONTAL

SLIDER
926
322
1119
355
ProbabilityOfActivityEdu0
ProbabilityOfActivityEdu0
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
926
355
1119
388
ProbabilityOfActivityEdu1
ProbabilityOfActivityEdu1
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
926
388
1119
421
ProbabilityOfActivityEdu2
ProbabilityOfActivityEdu2
0
1
0.5
0.01
1
NIL
HORIZONTAL

PLOT
407
580
653
730
% Meat eaters by Edu Level
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Edu0" 1.0 0 -6759204 true "" "plot  Edu0"
"Edu1" 1.0 0 -12345184 true "" "plot  Edu1"
"Edu2" 1.0 0 -14462382 true "" "plot  Edu2"

SWITCH
917
207
1051
240
Global-Switch
Global-Switch
1
1
-1000

SWITCH
917
241
1051
274
Local-Switch
Local-Switch
0
1
-1000

SLIDER
926
478
1099
511
k-local
k-local
0
1
1
0.0001
1
NIL
HORIZONTAL

BUTTON
206
59
278
92
go500
repeat 500 [go]
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
273
136
473
286
Conversions
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"toMeat" 1.0 0 -2139308 true "" "plot switch-To-Meat"
"toVeg" 1.0 0 -6565750 true "" "plot switch-To-Veg"

SLIDER
92
297
245
330
ElasticityEduLevel
ElasticityEduLevel
-1
1
-0.52
0.01
1
NIL
HORIZONTAL

SLIDER
292
11
464
44
probaMeatEdu0
probaMeatEdu0
0
1
0.25
0.01
1
NIL
HORIZONTAL

CHOOSER
1119
93
1257
138
xyParisians
xyParisians
"gaussian" "invGaussian" "uniform"
0

CHOOSER
1120
143
1258
188
xyA1
xyA1
"gaussian" "invGaussian" "uniform"
0

SLIDER
1273
93
1445
126
meanPGaussian
meanPGaussian
0
1.0
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
1459
94
1631
127
stdPGaussian
stdPGaussian
0.0
1.0
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
995
650
1167
683
%PatchA1
%PatchA1
0
100
20
1
1
NIL
HORIZONTAL

BUTTON
690
654
811
687
NIL
color-activity
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1274
150
1456
183
meanA1Gaussian
meanA1Gaussian
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
1467
150
1639
183
stdA1Gaussian
stdA1Gaussian
0
1
0.15
0.01
1
NIL
HORIZONTAL

TEXTBOX
694
494
715
512
A1
12
5.0
1

TEXTBOX
673
610
823
628
Parisians
12
25.0
1

SWITCH
736
482
847
515
colorA1
colorA1
0
1
-1000

SWITCH
736
523
848
556
ColorA2
ColorA2
0
1
-1000

SWITCH
736
561
867
594
ColorA1A2
ColorA1A2
1
1
-1000

SWITCH
737
601
840
634
ColorP
ColorP
0
1
-1000

TEXTBOX
692
535
718
553
A2
12
0.0
1

TEXTBOX
683
575
718
593
A1A2
12
15.0
1

SLIDER
290
85
462
118
ProbaMeatEdu2
ProbaMeatEdu2
0
1
1
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50"/>
    <metric>round((count parisians with [Meat? = 1 and act1? = 1 and act2? = 1] * 100)/ count parisians with [act1? = 1 and act2? = 1])</metric>
    <metric>round((count parisians with [Meat? = 1 and act1? = 0 and act2? = 0] * 100)/ count parisians with [act1? = 0 and act2? = 0])</metric>
    <metric>round((count parisians with [Meat? = 1 and act1? = 1 and act2? = 0] * 100)/ count parisians with [act1? = 1 and act2? = 0])</metric>
    <metric>round((count parisians with [Meat? = 1 and act1? = 0 and act2? = 1] * 100)/ count parisians with [act1? = 0 and act2? = 1])</metric>
    <enumeratedValueSet variable="EquipmentPerPerson">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPeople">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probMeat">
      <value value="0.15"/>
      <value value="0.5"/>
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
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
