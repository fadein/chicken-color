(use color)

(define forestgreen (sRGB->color '(34 139 34)))
(assert (color? forestgreen ))
(assert (equal? (color->string forestgreen) "sRGB:34/139/34"))

(define lightcyan4 (sRGB->color '(122 139 139)))
(assert (equal? (color->string lightcyan4) "sRGB:122/139/139"))

(define peachpuff (sRGB->color '(255 218 185)))
(assert (equal? (color->string peachpuff) "sRGB:255/218/185"))

(assert (< 64.360 (CIE:DE* lightcyan4 forestgreen) 64.370))
(assert (< 57.266 (CIE:DE*94 lightcyan4 forestgreen) 57.300))
(assert (< 71.850 (CMC:DE* lightcyan4 forestgreen) 72.100))


(assert (equal? (color->string D65) "CIEXYZ:0.950456/1.0/1.088754"))

(let ((forestgreen-L*u*v* (L*u*v*->color (color->L*u*v* forestgreen)))
	  (lightcyan4-L*u*v*  (L*u*v*->color (color->L*u*v* lightcyan4))))
  (assert (equal? (color->string forestgreen-L*u*v*) "CIELuv:50.59/-43.12/55.74"))
  (assert (equal? (color->string lightcyan4-L*u*v*) "CIELuv:56.55/-9.15/-1.97")))

(for-each (lambda (str) (print str ", a.k.a. lightcyan4"))
		  (map (lambda (enc)
				 (color->string (convert-color lightcyan4 enc)))
			   '(CIEXYZ RGB709 sRGB L*a*b* L*u*v* L*C*h)))
