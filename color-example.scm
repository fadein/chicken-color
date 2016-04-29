#!/usr/bin/csi -s
; vim: set filetype=scheme:
; autocmd BufWritePost <buffer> silent !csi -s %

;; Create an HTML file named color-eggsample.html demonstrating
;; some of the possibilities for high-level color manipulations

(use color)

(define the-colors
  (list (cons (sRGB->color '(#xff #x00 #x00)) "Red")
		(cons (sRGB->color '(#xb2 #x22 #x22)) "Firebrick")
		(cons (sRGB->color '(#x00 #xff #x00)) "Green")
		(cons (sRGB->color '(#x2e #x8b #x57)) "Sea Green")
		(cons (sRGB->color '(#x00 #x00 #xff)) "Blue")
		(cons (sRGB->color '(#x00 #x00 #x80)) "Navy Blue")
		(cons (sRGB->color '(#xff #x00 #xff)) "Magenta")
		(cons (sRGB->color '(#x66 #x33 #x99)) "Rebecca Purple")
		(cons (sRGB->color '(#xff #xff #x00)) "Yellow")
		(cons (sRGB->color '(#x8b #x45 #x13)) "Saddle Brown")))

; print a color object in #rrggbb format
(define (color->RGBhex c)
  (let ((srgb (color->sRGB c)))
	(apply conc `("#" ,@(map (lambda (s) (string-pad (number->string s 16) 2 #\0)) srgb)))))

; scale a color's Lightness in L*C*h space by the given factor
; L*C*h lightness values are clamped in the range 0 <= L* <= 100
(define (color:scale-lightness color factor)
  (let* ((LCh (color->L*C*h color))
		 (lightness (* (car LCh) factor)))
	(L*C*h->color (cons 
					(cond
					  ((< lightness   0.0) 0.0)
					  ((> lightness 100.0) 100.0)
					  (else lightness))
					(cdr LCh)))))

; rotate a color's hue in L*C*h space by the given number of degrees
; L*C*h h values are constrained to lie in the range 0 <= h <= 360
(define (color:rotate-hue color degrees)
  (let* ((LCh (color->L*C*h color))
		 (hue (+ (caddr LCh) degrees)))
	(L*C*h->color 
	  (append (take LCh 2)
			  (list (cond
					  ((< hue 0.0)
					   (let incr ((h hue))
						 (if (>= h 0.0) h (incr (+ 360.0 h)))))
					  ((> hue 360.0)
					   (let reduce ((h hue))
						 (if (<= h 360.0) h (reduce (- h 360.0)))))
					  (else
						hue)))))))

; scale a color's Chroma in L*C*h space by the given factor
; L*C*h chroma values are not clamped
(define (color:scale-chroma color factor)
  (let* ((LCh (color->L*C*h color))
		 (chroma (* (cadr LCh) factor)))
	(L*C*h->color (cons (car LCh) (cons chroma (cddr LCh))))))

;; triadic harmony
;; returns 3 colors equally spread about the color wheel
;; i.e. hue rotated +/- 120°
(define (color:triad color)
  (list (color:rotate-hue color -120) color (color:rotate-hue color 120)))

;; complementary color
;; returns the given color's complement
;; i.e. hue rotated by 180°
(define (color:complement color)
  (color:rotate-hue color 180))

(define distance-from-black
  (let ((lightness-of-black (car (color->L*C*h (make-color 'RGB709 '(0 0 0))))))
	(lambda (color)
	  (abs (- (car (color->L*C*h color)) lightness-of-black)))))

; write an HTML table row with n elements, displaying the color hex triplet as the table value
(define (hex-triplet-tr . c)
  (print "<tr>")
  (for-each
	(lambda (color)
	  (let ((hex (color->RGBhex color)))
		(printf "  <td style=\"background:~a; color:~a; font-weight:bold; font-family:courier; text-align:center; height:50px;\">~a</td> "
				hex (if (> 28.0 (distance-from-black color)) "#a0a0a0" "#000000") hex)))
	c)
  (print "</tr>\n"))

; Print a captioned HTML table by applying a scaling function with a range of scaling factors
(define (scale-table scale scaling-function html-tr-proc caption)
  (print "<div align=\"center\">")
  (print "<table style=\"width:95%; height:30px; border:1px; border-spacing:3px; text-align:center; padding:10px\">")
  (print "<caption>" caption "</caption>")
  (apply print `("<tr>" 
				 ,@(map (lambda (factor) (string-join `("<th>" ,(number->string factor) "</th>"))) scale)
				 "</tr>"))
  (apply map html-tr-proc
		 (map (lambda (f)
				(map (lambda (c) (scaling-function c f)) (map car the-colors)))
			  scale))
  (print "</table></div>"))

; Print a captioned HTML table by applying a harmony function to the colors
(define (harmony-table harmony-function caption)
  (print "<div align=\"center\">")
  (print "<table style=\"width:55%; height:30px; border:1px; text-align:center; border-spacing:3px; padding:10px\">")
  (print "<caption>" caption "</caption>")
  (map (lambda (lst) (apply hex-triplet-tr lst))
	   (map (lambda (c) (harmony-function c)) (map car the-colors)))
  (print "</table></div>"))

(define (keyframe start end count)
  (let* ((count (sub1 count))
		 (diff (- start end))
		 (distance (abs diff))
		 (compare? (if (< start end) >= <=))
		 (stepsize (if (eq? compare? >=)
					 (/ distance count)
					 (- 0 (/ distance count)))))
	(let loop ((here start))
	  (if (compare? here end)
		(cons here '())
		(cons here (loop (+ stepsize here)))))))

(print "<html><head><meta charset=\"UTF-8\"/></head><body>")
(print "<div align=\"center\">")
(print "<table style=\"width:95%; height:30px; border:1px; text-align:center; border-spacing:3px; padding:10px\">")
(print "<caption>Meet the colors</caption>")
(print "<tr>")
(for-each
  (lambda (color-name)
	(printf "  <td style=\"text-align:center;\">~a</td> "
			color-name ))
  (map cdr the-colors))
(print "</tr>\n")
(map (lambda (c) (apply hex-triplet-tr c)) (list (map car the-colors)))
(print "</table></div>")
(harmony-table (lambda (c) (let ((comp (color:complement c))) (list c comp)))    "Complementary color pairs")
(harmony-table color:triad                                                       "Triadic colors (color ±120°)")
(scale-table (keyframe 3.0 0.5 11)  color:scale-lightness      hex-triplet-tr    "Scale lightness value of colors")
(scale-table (keyframe 3.0 0.5 11)  color:scale-chroma         hex-triplet-tr    "Scale chroma value of colors")
(scale-table (keyframe -30 30 11)   color:rotate-hue           hex-triplet-tr    "Rotate hues through 60°")
(scale-table (keyframe -45 45 11)   color:rotate-hue           hex-triplet-tr    "Rotate hues through 90°")
(scale-table (keyframe -90 90 11)   color:rotate-hue           hex-triplet-tr    "Rotate hues through 180°")
(print "</body></html>")
