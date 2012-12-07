#|
Problem 66
26 March 2004

Consider quadratic Diophantine equations of the form:

x2 – Dy2 = 1

For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.

It can be assumed that there are no solutions in positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

32 – 2×22 = 1
22 – 3×12 = 1
92 – 5×42 = 1
52 – 6×22 = 1
82 – 7×32 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.


|#

(defun pell (d)
  "give non-negative x y satisfying x^2 - dy^2 = 1, Pells Equation"
  ;; copied from http://math.fau.edu/richman/pell-m.htm, this was function tess(d)
  ;; what?
  (let* ((w (isqrt d))
	 (q (- d (square w))))
    (if (zerop q)
	(values 1 0)
	(let* ((dl d)
	       (q0 1)
	       (m w)
	       (u0 1)
	       (u0L 1)
	       (u m)
	       (uL w)
	       (v0 0)
	       (v0L 0)
	       (v 1)
	       (vL 1)
	       (sign -1))
	  (while (/= 1 (* sign q))
	    (let* ((a (floor (/ (+ m w) q)))
		   (m1 (- (* a q) m))
		   (q1 (+ q0 (* a (- m m1))))
		   (u1 (+ u0 (* a u)))
		   (u1L (+ u0L (* uL a)))
		   (v1 (+ v0 (* a v)))
		   (v1L (+ v0L (* a vL))))
	      (setf m m1)
	      (setf q0 q)
	      (setf q q1)
	      (setf u0 u)
	      (setf u0L uL)
	      (setf u u1)
	      (setf uL u1L)
	      (setf v0 v)
	      (setf v0L vL)
	      (setf v v1)
	      (setf vL v1L)
	      (setf sign (* -1 sign))
	      (let* 
		  ((e 10000000)
		   (g (mod (- (square (mod u e)) (* d (square (mod v e)))) e)))
		(when (> g (/ e 2)
			 (decf g e)))
		  (let*
		      ((bL (* -1 vL vL))
		       (gL (+ bL (* uL uL))))
		   ; (when (< (abs gL) (abs dL))
		      ; (format t "~a^2 - ~a * ~a^2 = ~a~%"
			;      uL d vL gL))))))
		    ))))
	; (format t "~a^2 - ~a * ~a^2 = ~a~%"
		      ; uL d vL 1)
	  (values uL vL)))))

		   
	       
	

(defun quad-diophantine (d)
  "solve the quadratic diophantine equation x^2 - dy^2 =1"
  ;; we know that y^2 = (x + 1)(x - 1)/d ; an integer
  ;; so d cannot divide x, since it divides either x+1 or x-1
  (if (is-square  d)
      (values 1 0)
      (pell d)))
	   
(defvar *hard-multiples* 
  (list 61 109 149 151 157 166 
	181 193 199 211 214 233
	241 244 253 261 262 268 
	271 277 281 283 298 301
	309 )
  "d such that x > 100M")
#|

(defun quad-diophantine (D)
  (if (is-square D) 
      (values 1 0)
      (loop for x from 1 
	 do
	   (loop for y from 1 to x
	      when (= 1 (- (square x) (* D (square y))))
	      do 
		(return-from quad-diophantine (values x y))))))
|#

(defun p66 (&optional (start 2))
  (do* 
   ((d start (print (1+ d)))
    (x (quad-diophantine d) (quad-diophantine d))
    (max x (max max x))
    (dmax max (if (= x max) d dmax)))
   ((= d 1000) (values dmax max))))


#|
Using OEIS :  Euler doesn't accept 990 as a solution?
1 3
2 2
3 9
4 5
5 8
6 3
7 19
8 10
9 7
10 649
11 15
12 4
13 33
14 17
15 170
16 9
17 55
18 197
19 24
20 5
21 51
22 26
23 127
24 9801
25 11
26 1520
27 17
28 23
29 35
30 6
31 73
32 37
33 25
34 19
35 2049
36 13
37 3482
38 199
39 161
40 24335
41 48
42 7
43 99
44 50
45 649
46 66249
47 485
48 89
49 15
50 151
51 19603
52 530
53 31
54 1766319049
55 63
56 8
57 129
58 65
59 48842
60 33
61 7775
62 251
63 3480
64 17
65 2281249
66 3699
67 26
68 57799
69 351
70 53
71 80
72 9
73 163
74 82
75 55
76 285769
77 10405
78 28
79 197
80 500001
81 19
82 1574
83 1151
84 12151
85 2143295
86 39
87 49
88 62809633
89 99
90 10
91 201
92 101
93 227528
94 51
95 41
96 32080051
97 962
98 1351
99 158070671986249
100 21
101 295
102 127
103 1204353
104 1025
105 1126
106 9801
107 649
108 306917
109 120
110 11
111 243
112 122
113 4620799
114 930249
115 449
116 4730624
117 577
118 16855
119 6499
120 10610
121 23
122 2588599
123 145925
124 244
125 35
126 6083073
127 47
128 77563250
129 71
130 95
131 143
132 12
133 289
134 145
135 97
136 73
137 25801741449
138 49
139 1728148040
140 37
141 2177
142 21295
143 249
144 25
145 46698728731849
146 7743
147 1324
148 721
149 11775
150 19601
151 64080026
152 2049
153 1079
154 1700902565
155 168
156 13
157 339
158 170
159 24248647
160 2499849
161 1451
162 2024
163 199
164 62423
165 1601
166 4190210
167 161
168 2469645423824185801
169 27
170 487
171 24335
172 9249
173 7501
174 1682
175 4607
176 55
177 52021
178 8994000
179 97
180 6224323426849
181 195
182 14
183 393
184 197
185 16266196520
186 99
187 515095
188 19731763
189 57
190 4999
191 39689
192 59535
193 1151
194 649
195 46551
196 29
197 278354373650
198 66249
199 194399
200 695359189925
201 44
202 485
203 3844063
204 126003
205 74
206 89
207 1665
208 149
209 224
210 15
211 451
212 226
213 151
214 5848201
215 91
216 76
217 19603
218 1072400673
219 5201
220 46
221 561799
222 228151
223 11663
224 6195120
225 31
226 10085143557001249
227 19601
228 70226
229 1766319049
230 51841
231 88805
232 85292
233 63
234 8553815
235 39480499
236 3674890
237 127
238 3222617399
239 255
240 16
241 513
242 257
243 847225
244 129
245 192119201
246 104980517
247 139128
248 65
249 73738369
250 685
251 2402
252 4771081927
253 13449
254 5291
255 115974983600
256 33
257 727
258 3959299
259 199
260 7775
261 159150073798980475849
262 2501
263 1520
264 251
265 2262200630049
266 2351
267 138274082
268 24220799
269 2431
270 561835
271 288
272 17
273 579
274 290
275 2281249
276 12320649
277 4801
278 2024999
279 3699
280 48599
281 335473872499
282 415
283 1351
284 5883392537695
285 4276623
286 2524
287 57799
288 489
289 35
290 88529282
291 351
292 64202725495
293 848719
294 16883880
295 53
296 32188120829134849
297 392499
298 71
299 12799
300 248678907849
301 107
302 12901780
303 161
304 215
305 323
306 18
307 649
308 325
309 217
310 163
311 2376415
312 109
313 2785589801443970
314 13447
315 73
316 63804373719695
317 604
318 55
319 2063810353129713793
320 114243
321 97970
322 285769
323 10626551
324 37
325 130576328
326 10405
327 6761
328 17299
329 641602
330 1567
331 169648201
332 449
333 62425
334 77617
335 10157115393
336 258065
337 954809
338 500001
339 3401
340 176579805797
341 360
342 19
343 723
344 362
345 4954951
346 23915529
347 907925
348 19019995568
349 1151
350 8396801
351 213859
352 1695
353 12151
354 52387849
355 3365
356 15124
357 2143295
358 233
359 8749
360 12941197220540690
361 39
362 1015
363 164998439999
364 18768
365 4801
366 95831
367 111555
368 3482
369 62809633
370 3287049
371 79
372 7338680
373 99
374 46437143
375 312086396361222451
376 159
377 199
378 838721786045180184649
379 399
380 20
381 801
382 401
383 669878
384 201
385 161
386 59468095
387 2663
388 101
389 25052977273092427986049
390 81
391 49730
392 103537981567
393 113399
394 24335
395 18412804
396 5201
397 85322647
398 33857
399 270174970
400 41
401 3879474045914926879468217167061449
402 7022501
403 4607
404 32080051
405 143649
406 88751
407 62
408 1850887
409 1524095
410 2862251
411 151560720
412 1351
413 104564907854286695713
414 125
415 146
416 158070671986249
417 4599
418 293
419 440
420 21
421 883
422 442
423 295
424 43468489
425 110166015
426 148
427 127
428 71798771299708449
429 19601
430 46471490
431 1204353
432 1653751
433 16916040084175685
434 64
435 1025
436 6983244756398928218113
437 22899
438 499850
439 2535751
440 1182351890184201
441 43
442 247512720456368
443 9801
444 15871
445 938319425
446 1625626
447 649
448 137215
449 1691
450 7838695
451 306917
452 87
453 193549
454 57799
455 28799
456 8777860001
457 1617319577991743
458 2989440
459 241
460 1859131879201
461 483
462 22
463 969
464 485
465 51906073840568
466 243
467 7592629975
468 1039681
469 93628044170
470 29767
471 935662752649
472 73035
473 89
474 4620799
475 1201887
476 179777
477 4490
478 930249
479 11242731902975
480 3832352837
481 24648
482 449
483 809
484 45
485 1351
486 44757606858751
487 313201220822405001
488 271
489 4188548960
490 665857
491 13771351
492 4625
493 17406
494 16855
495 590968985399
496 2367
497 14851876
498 6499
499 32961431500035201
500 19603
501 81810300626
502 225144199
503 6049
504 84056091546952933775
505 528
506 23
507 1059
508 530
509 2588599
510 74859849
511 3678725
512 1618804
513 145925
514 192349463
515 9536081203
516 3970
517 119071
518 3707453360023867028800645599667005001
519 4293183
520 669337
521 2449
522 1961
523 701
524 160177601264642
525 6083073
526 1766319049
527 30580901
528 8380
529 47
530 624635837407
531 60756099699
532 1814
533 12032115501124999
534 27849
535 7937
536 506568295
537 71
538 522785
539 220938497
540 68122
541 95
542 435259412378569
543 95609285
544 2024
545 143
546 16760473211643448449
547 191
548 181124355061630786130
549 287
550 383
551 575
552 24
553 1153
554 577
555 385
556 289
557 152071153975
558 193
559 8429543
560 145
561 33281
562 33867877212256207699
563 1907162
564 97
565 41423166067036218751
566 5781
567 165676
568 73
569 721517598849
570 1098305
571 18514
572 25801741449
573 463287093751
574 1574351
575 24686379794520
576 49
577 38902815462492318420311478049
578 687
579 48842
580 5972991296311683199
581 930249
582 42187499
583 164076033968
584 2737
585 605695
586 10323982819
587 236926
588 2177
589 464018873584078278910994299849
590 348291186245
591 124
592 21295
593 3363593612801313
594 10093
595 517213510553282930
596 249
597 7775
598 13804370063
599 624
600 25
601 1251
602 626
603 46698728731849
604 123245001
605 251
606 48961575312998650035560
607 7743
608 440772247
609 8711856945587257031251
610 126
611 3505951
612 1419278889601
613 42283
614 24220799
615 1039681
616 2609429220845977814049
617 5777
618 1988960193026
619 11775
620 1024001
621 305
622 120187368
623 19601
624 1123593226162199
625 51
626 1735
627 8212499464321351
628 10499986568677299849
629 8915765
630 737709209
631 2049
632 2281249
633 1693
634 5930
635 1079
636 16421658242965910275055840472270471049
637 1718102501
638 103
639 1700902565
640 13719
641 27365201
642 107119097
643 56447
644 14226117859054135
645 5791211
646 58620
647 337
648 4765506835465395993032041249
649 675
650 26
651 1353
652 677
653 17792625320
654 339
655 10743166003415
656 1197901
657 170067682
658 57799
659 95592800063517769
660 10850138895
661 165337
662 24248647
663 105
664 1471
665 31138100617500578690
666 2499849
667 246401
668 38782105445014642382885
669 33639
670 1451
671 34849
672 51999603
673 2271050
674 8193151
675 277631049
676 53
677 1159172
678 79201
679 237161
680 34595
681 2526
682 62423
683 665782673992201
684 1279
685 80
686 1601
687 5286367
688 4115
689 75646
690 35115719688199
691 6998399
692 8933399183036079503
693 403480310400
694 161
695 18632176943292415
696 22619537
697 242
698 2469645423824185801
699 9801
700 485
701 728
702 27
703 1459
704 730
705 487
706 195307849
707 10394175
708 244
709 24335
710 252975383
711 163
712 98015661073616742153890
713 9249
714 7352695
715 263091151
716 714024
717 7501
718 12769001
719 61268974069299
720 82
721 5658247
722 1084616384895
723 2550251
724 7293318466794882424418960
725 4607
726 308526027863
727 836977699
728 1209
729 55
730 3750107388553
731 413959717
732 551
733 52021
734 1280001
735 6349
736 719724601
737 161784071999999
738 285769
739 145933611945744638015
740 31212
741 18817
742 535781868388881310859702308423201
743 111
744 2989136930
745 6224323426849
746 3607394696649
747 10405
748 4620799
749 195
750 223
751 5964562960504723
752 11785490
753 391
754 67606199
755 783
756 28
757 1569
758 785
759 34625394242
760 393
761 16116667272575
762 6616066879
763 225
764 197
765 4393
766 1828310451
767 6626
768 529178298454520220799
769 1221759532448649
770 113
771 424
772 19601
773 500002000001
774 295496099
775 7226
776 515095
777 1514868641
778 6166395
779 51841948
780 19731763
781 376455160998025676163201
782 27379
783 1382072163578616410
784 57
785 2167
786 4206992174549
787 156644
788 4999
789 343
790 40899
791 1574
792 39689
793 9000987377460935993101449
794 7397
795 235170474903644006168
796 59535
797 48599
798 222239304685
799 900602
800 1151
801 479835713751049
802 146411
803 9799705
804 842401
805 9478657
806 6552578705
807 34336355806
808 46551
809 12151
810 42112785797
811 840
812 29
813 1683
814 842
815 154962314660167628644999
816 299537289
817 2143295
818 8193151
819 66249
820 1501654712948695
821 2449
822 8418574
823 194399
824 215454135724113414336120649
825 1294299
826 3041
827 695359189925
828 131822292741249
829 703
830 2058844771979643060124010
831 3871
832 541601801
833 358022566147312125503
834 18524026608
835 470449
836 242688628535063329
837 42435
838 70226
839 3844063
840 60192738698751
841 59
842 19442812076
843 126003
844 62809633
845 3725
846 120126
847 10951
848 116476476553
849 9314703
850 107245324
851 89
852 22606256615916825861249
853 19601
854 34878475759617272473442
855 1665
856 119
857 7743524593057655851637765
858 469224
859 149
860 13231974717803657215
861 179
862 3970
863 100351
864 6091434999
865 299
866 359
867 449
868 599
869 899
870 30
871 1801
872 901
873 601
874 451
875 361
876 301
877 123823410343073497682
878 102151
879 80801
880 181
881 371832584927520
882 151
883 515734243080407
884 62563299
885 121
886 5848201
887 823604599
888 4120901
889 4481603010937119451551263720
890 91
891 2522057712835735
892 351605368773852499
893 638
894 11551
895 1555849
896 304560297142335
897 227528
898 768555217
899 13224937103288377430049
900 61
901 6681448801
902 1072400673
903 75263
904 3034565
905 1376
906 5201
907 480644425002415999597113107233
908 17151
909 122695
910 4231
911 1068924905989944201
912 106133
913 737
914 561799
915 275561
916 45225786400145
917 13509645362
918 228151
919 609622436806639069525576201
920 202501
921 224208076
922 11663
923 15090531843660371073
924 32080051
925 2095256249
926 76759023628799
927 14849
928 16762522330425599
929 960
930 31
931 1923
932 962
933 10085143557001249
934 446526729
935 57499
936 4649532557817485528
937 19601
938 13588951
939 215395035859
940 12479806786330
941 9863382151
942 903223
943 488825745235215
944 1249
945 1766319049
946 108832847723078562849
947 118337
948 360449
949 51841
950 158070671986249
951 8837
952 284088
953 88805
954 332929
955 49299
956 377
957 14549450527
958 550271588560695
959 881
960 379516400906811930638014896080
961 63
962 2647
963 1135
964 8835999
965 8553815
966 14418057673
967 984076901
968 102688615
969 39480499
970 1060905
971 206869247
972 9026
973 27009633024199
974 2950149761
975 14125267563780214605455
976 476
977 127
978 583201
979 3395619
980 8426
981 3222617399
982 309159979019115849
983 4656965
984 352871
985 255
986 2900932297217
987 15155578752298003
988 6089923321730
989 511
990 198723867690977573219668252231077415636351801801
991 1023
992 32
993 2049
994 1025
995 133150393
996 513
997 97379496466615
998 884307011291
999 651737448664200
1000 257

|#



#|
(defun cont-fract-sqrt (d)
  (let (astack pstack ppstack qqstack (a0 (floor (sqrt d))))
    (push a0 astack)
    (push a0 pstack)
    (push 0 ppstack) (push a0 ppstack)
    (push 1 qqstack) (push (- d (* a0 a0)) qqstack)
    (let ((a1 (floor (/ (+ a0 (car ppstack)) (car qqstack)))))
      (push a1 astack) (push (1+ (* a0 a1)) pstack))
    (loop for r from 0
          when (= (car astack) (* 2 a0))
               do (return (values r astack pstack))
          do (push (- (* (car astack) (car qqstack)) (car ppstack)) ppstack)
             (push (/ (- d (expt (car ppstack) 2)) (car qqstack)) qqstack)
             (push (floor (/ (+ a0 (car ppstack)) (car qqstack))) astack)
             (push (+ (* (car astack) (car pstack)) (cadr pstack)) pstack))))
|#
