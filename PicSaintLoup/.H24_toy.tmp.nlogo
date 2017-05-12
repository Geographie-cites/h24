extensions [csv]

breed [parisians parisian]
patches-own [
  act1
  act2
  propGlobalHealthyPatch
  propGlobalUnhealthyPatch
  propLocalHealthyPatch
  propLocalUnhealthyPatch
  switchingParisians
  ]
parisians-own [ act1? act2? PatchResidence PatchA1 PatchA2 EduLevel Healthy? age sex opinion constraint-budget constraint-time csv_o csv_n csv_p
  constraint-habit partner in-interaction? influencePartner influenceCell
  ]
globals [ size-city healthyA1 healthyA2 healthyA1A2 healthyImmo Edu0 Edu1 Edu2 switch-To-Healthy switch-To-Unhealthy]


to save-csv

  init-csv
  insert-csv
end

to init-csv

  let file_n "neighbours.csv"
  let file_p "paired.csv"
  let file_o "opinion.csv"

  if is-string? file_o
  [
    ;; If the file already exists, we begin by deleting it, otherwise
    ;; new data would be appended to the old contents.
    if file-exists? file_o
      [  file-close
         file-delete file_o ]
    file-open file_o
    ;; record the initial turtle data
  ]

  if is-string? file_p
  [
    ;; If the file already exists, we begin by deleting it, otherwise
    ;; new data would be appended to the old contents.
    if file-exists? file_p
      [  file-close
         file-delete file_p ]
    file-open file_p
    ;; record the initial turtle data
  ]

  if is-string? file_n
  [
    ;; If the file already exists, we begin by deleting it, otherwise
    ;; new data would be appended to the old contents.
    if file-exists? file_n
      [  file-close
         file-delete file_n ]
    file-open file_n
    ;; record the initial turtle data
  ]

  file-close

end


to insert-csv
  let file_o "opinion.csv"
  file-open file_o

  ask parisians[
    file-print csv:to-row (list who age sex EduLevel csv_o  )
  ]

  file-close

  let file_p "paired.csv"
  file-open file_p

  ask parisians[
    file-print csv:to-row (list who age sex EduLevel csv_p  )
  ]

  file-close

  let file_n "neighbours.csv"
  file-open file_n

  ask parisians[
    file-print csv:to-row (list who age sex EduLevel csv_n)
  ]

  file-close

end


;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;

to setup
  clear-all
  reset-ticks
  set switch-To-Healthy 0
  set switch-To-Unhealthy 0
  setup-patches
  setup-parisians
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
    set age random 3
    set sex random 2

    set csv_o []
    set csv_n []
    set csv_p []
   ]
   ask parisians[

     ;; get probability by EduLevel
     let result get-probability EduLevel
     set act1? item 0 result
     set act2? item 1 result
     let randomHealthy random-float 1

     if EduLevel = 0 [

     ifelse randomHealthy < probaHealthyEdu0  [set Healthy? TRUE] [set Healthy? FALSE]
      set constraint-budget 1
      set constraint-time 0

     ]

     if EduLevel = 1 [

       ifelse randomHealthy < probaHealthyEdu1 [set Healthy? TRUE] [set Healthy? FALSE]
         set constraint-budget 0
      set constraint-time 0

     ]

     if EduLevel = 2 [
      ifelse randomHealthy < probaHealthyEdu2 [set Healthy? TRUE] [set Healthy? FALSE]
   set constraint-budget 0
      set constraint-time 1

     ]

    if age = 0 [ set constraint-habit 0]
    if age = 1 [ set constraint-habit 1]
    if age = 2 [ set constraint-habit 1]

    if any? patches with [act1 = 1] [set PatchA1 one-of patches with [act1 = 1]]
    if any? patches with [act2 = 1] [set PatchA2 one-of patches with [act2 = 1]]



    if sex = 0 [set opinion random-float probaOpinionHealthyMale]
    if sex = 1 [set opinion random-float probaOpinionHealthyFemale]

 set color white
 ]

 ask parisians with [Healthy? = TRUE] [set color green]
 ask parisians with [Healthy? = FALSE] [set color white]

end



to initialise-Switches
  set switch-To-Healthy 0
  set switch-To-Unhealthy 0
end

to GetActive
  doA1
  doA2
  comeBack
end

to doA1
  ask parisians [ set partner 0 ]
  ask parisians with [act1? = 1] [move-to PatchA1]
  count-minority
  ask parisians  [Identify-if-interactive]
  ask parisians  [Update-interaction]
   ask parisians  [Update-opinion-behaviour]
end

to doA2
    ask parisians [ set partner 0 ]
  ask parisians with [act2? = 1] [move-to PatchA2]
  count-minority

   ask parisians  [Identify-if-interactive]
  ask parisians [Update-interaction]
   ask parisians [Update-opinion-behaviour]
  ask parisians with [act2? = 1] [
    if any? patches with [act2 = 1] [set PatchA2 one-of patches with [act2 = 1]]
  ]
end

to Update-opinion-behaviour
  update-opinion
  update-behaviour
end


to Identify-if-interactive
  ifelse interact? [set in-interaction? TRUE] [set in-interaction? FALSE]
end

to Update-interaction

  if in-interaction? = TRUE and partner = 0 [
  let me nobody
    if any? other parisians-here with [in-interaction? = TRUE and partner = 0][

    ask one-of other parisians-here with [in-interaction? = TRUE and partner = 0] [
      set partner myself

      set me self
    ]
      set partner me
  ]
    set in-interaction? FALSE
  ]
  end

to-report  interact?
  let r-global random-float 1
  ifelse r-global < percentInteraction [report TRUE] [ report FALSE]
end


to update-opinion
  if  Healthy? = TRUE [set opinion min (list ((1 + healthy-diet-reward) * opinion) 1)]
  let newOpinion 0
  scan-partner
  scan-behaviour-in-cell
  let o opinion
  if partner = nobody and count other parisians-here = 0 [ set newOpinion opinion]
  if partner = nobody and count other parisians-here > 0 [
    set newOpinion 0.75 * opinion + 0.25 * influenceCell
  ]
  if partner != nobody and count other parisians-here > 0 [
    set newOpinion 0.5 * opinion + 0.25 * influencePartner + 0.25 * influenceCell
  ]
  let nbvoisin count other parisians-here
  let nbpartenaire count (parisians-here with [partner != 0])
  set opinion inertiaCoeff * opinion + (1 - inertiaCoeff) * newOpinion

  set csv_o lput opinion csv_o
  set csv_n lput nbvoisin csv_n
  set csv_p lput nbpartenaire csv_p
end

to update-behaviour
  let p random-float 1
  ifelse  Healthy? = TRUE [
    if p < proba-switch-to-unhealthy opinion constraint-budget constraint-time constraint-habit [
      set Healthy? FALSE
      set switch-to-Unhealthy switch-to-Unhealthy + 1

    ]
    ] [
       if p < proba-switch-to-healthy opinion constraint-budget constraint-time constraint-habit [
      set Healthy? TRUE
      set switch-to-Healthy switch-to-Healthy + 1

    ]
  ]
end


to-report proba-switch-to-healthy [#opinion #budget #time #habit]
  let Y maxProbaToSwitch - contraint-strength * #budget - contraint-strength * #time - contraint-strength * #habit
 report max (list 0 (Y * (2 * #opinion - 1) ) )
end

to-report proba-switch-to-unhealthy [#opinion #budget #time #habit]
  let Y maxProbaToSwitch + contraint-strength * #budget + contraint-strength * #time - contraint-strength * #habit
 report max (list 0 (Y * (-2 * #opinion + 1) ) )
end


to scan-partner
  if partner != 0 [
    let behavPartner 0
    ifelse [Healthy?] of partner = TRUE [set behavPartner 1] [set behavPartner 0]
    set influencePartner ([opinion] of partner + behavPartner) / 2
]
end


to scan-behaviour-in-cell
  if any? other parisians-here [
    let healthyPeople count other parisians-here with [Healthy? = TRUE]
   let unhealthyPeople count other parisians-here with [Healthy? = FALSE]
   set influenceCell  healthyPeople / (healthyPeople + unhealthyPeople)
  ]
end

to comeBack
   ask parisians [ set partner 0 ]
   ask parisians [move-to PatchResidence]
   count-minority
    ask parisians [Identify-if-interactive]
  ask parisians  [Update-interaction]
   ask parisians [Update-opinion-behaviour]

end

to Update-Color
    ask parisians with [Healthy? = TRUE] [set color green]
    ask parisians with [Healthy? = FALSE] [set color white]
    count-minority
 end

to count-minority
  ask patches [
    set propGlobalHealthyPatch (count parisians-here with [Healthy? = TRUE] * 100)/ count parisians with [Healthy? = TRUE]
    set propGlobalUnhealthyPatch (count parisians-here with [Healthy? = FALSE] * 100)/ count parisians with [Healthy? = FALSE]
   ifelse any? parisians-here [
     set propLocalHealthyPatch (count parisians-here with [Healthy? = TRUE] * 100)/ count parisians-here
      set propLocalUnhealthyPatch (count parisians-here with [Healthy? = FALSE] * 100)/ count parisians-here
      ] [
      set propLocalHealthyPatch 0  set propLocalUnhealthyPatch 0
      ]
  ]
end

to Update-globals
 set healthyA1 round((count parisians with [Healthy? = TRUE and act1? = 1 and act2? = 0] * 100)/ count parisians with [act1? = 1 and act2? = 0])
 set healthyA2 round((count parisians with [Healthy? = TRUE and act1? = 0 and act2? = 1] * 100)/ count parisians with [act1? = 0 and act2? = 1])
 set healthyA1A2 round((count parisians with [Healthy? = TRUE and act1? = 1 and act2? = 1] * 100)/ count parisians with [act1? = 1 and act2? = 1])
 set healthyImmo round((count parisians with [Healthy? = TRUE and act1? = 0 and act2? = 0] * 100)/ count parisians with [act1? = 0 and act2? = 0])
 set Edu0 round((count parisians with [Healthy? = TRUE and EduLevel = 0 ] * 100)/ count parisians with [EduLevel = 0 ])
 set Edu1 round((count parisians with [Healthy? = TRUE and EduLevel = 1 ] * 100)/ count parisians with [EduLevel = 1 ])
 set Edu2 round((count parisians with [Healthy? = TRUE and EduLevel = 2 ] * 100)/ count parisians with [EduLevel = 2 ])
end
@#$#@#$#@
GRAPHICS-WINDOW
477
20
894
438
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
20
10
86
43
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
91
11
154
44
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
910
23
1082
56
nPeople
nPeople
0
16000
1000.0
100
1
NIL
HORIZONTAL

MONITOR
27
257
179
302
% healthy-eaters A1A2
round((count parisians with [Healthy? = TRUE and act1? = 1 and act2? = 1] * 100)/ count parisians with [act1? = 1 and act2? = 1])
17
1
11

MONITOR
37
161
175
206
% healthy-eaters A1
healthyA1
17
1
11

MONITOR
28
306
166
351
% healthy-eaters A2
round((count parisians with [Healthy? = TRUE and act1? = 0 and act2? = 1] * 100)/ count parisians with [act1? = 0 and act2? = 1])
17
1
11

BUTTON
159
12
223
45
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
185
319
256
352
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
906
100
1083
133
probaHealthyEdu1
probaHealthyEdu1
0
1
0.43
0.01
1
NIL
HORIZONTAL

MONITOR
37
209
176
254
% healthy-eaters Immobile
round((count parisians with [Healthy? = TRUE and act1? = 0 and act2? = 0] * 100)/ count parisians with [act1? = 0 and act2? = 0])
17
1
11

MONITOR
264
314
338
359
% healthy
round((count parisians with [Healthy? = TRUE ] * 100)/ count parisians)
17
1
11

PLOT
28
364
286
514
% Healthy Eaters by Mobility
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
"A1" 1.0 0 -10873583 true "" "plot healthyA1"
"A2" 1.0 0 -14070903 true "" "plot healthyA2"
"A1A2" 1.0 0 -15302303 true "" "plot healthyA1A2\n"
"Immo" 1.0 0 -955883 true "" "plot healthyImmo"

SLIDER
754
454
926
487
%PatchA2
%PatchA2
0
100
33.0
1
1
NIL
HORIZONTAL

SLIDER
897
179
1090
212
ProbabilityOfActivityEdu0
ProbabilityOfActivityEdu0
0
1
0.17
0.01
1
NIL
HORIZONTAL

SLIDER
897
212
1090
245
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
897
245
1090
278
ProbabilityOfActivityEdu2
ProbabilityOfActivityEdu2
0
1
0.9
0.01
1
NIL
HORIZONTAL

PLOT
289
364
535
514
% Healthy eaters by Edu Level
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

BUTTON
228
13
300
46
go100
repeat 100 [go]
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
186
156
465
306
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
"toHealthy" 1.0 0 -15637942 true "" "plot switch-To-Healthy"
"toUnhealthy" 1.0 0 -408670 true "" "plot switch-To-Unhealthy"

SLIDER
907
63
1079
96
probaHealthyEdu0
probaHealthyEdu0
0
1
0.25
0.01
1
NIL
HORIZONTAL

CHOOSER
34
536
172
581
xyParisians
xyParisians
"gaussian" "invGaussian" "uniform"
1

CHOOSER
35
586
173
631
xyA1
xyA1
"gaussian" "invGaussian" "uniform"
0

SLIDER
188
536
360
569
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
374
537
546
570
stdPGaussian
stdPGaussian
0.0
1.0
0.66
0.01
1
NIL
HORIZONTAL

SLIDER
755
489
927
522
%PatchA1
%PatchA1
0
100
69.0
1
1
NIL
HORIZONTAL

BUTTON
347
320
468
353
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
189
593
371
626
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
382
593
554
626
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
586
470
607
488
A1
12
5.0
1

TEXTBOX
565
586
715
604
Parisians
12
25.0
1

SWITCH
628
458
739
491
colorA1
colorA1
0
1
-1000

SWITCH
628
499
740
532
ColorA2
ColorA2
0
1
-1000

SWITCH
628
537
759
570
ColorA1A2
ColorA1A2
0
1
-1000

SWITCH
629
577
732
610
ColorP
ColorP
0
1
-1000

TEXTBOX
584
511
610
529
A2
12
0.0
1

TEXTBOX
575
551
610
569
A1A2
12
15.0
1

SLIDER
905
137
1077
170
ProbaHealthyEdu2
ProbaHealthyEdu2
0
1
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
901
315
1097
348
probaOpinionHealthyMale
probaOpinionHealthyMale
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
901
280
1093
313
probaOpinionHealthyFemale
probaOpinionHealthyFemale
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
908
353
1080
386
percentInteraction
percentInteraction
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
201
55
373
88
maxProbaToSwitch
maxProbaToSwitch
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
21
56
199
89
inertiaCoeff
inertiaCoeff
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
202
91
374
124
contraint-strength
contraint-strength
0
1
0.153
0.001
1
NIL
HORIZONTAL

SLIDER
21
90
199
123
healthy-diet-reward
healthy-diet-reward
0
1
0.25
0.01
1
NIL
HORIZONTAL

BUTTON
305
14
398
47
NIL
save-csv
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

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
NetLogo 6.0.1
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
