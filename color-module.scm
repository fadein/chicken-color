;;;; color-module.scm
;;;; It is enough to
;;;  > ,l color-module.scm
;;;  and then
;;;  > (import color)


(module color (
			   ; 5.11.1 Color Data-Type
			   color?
			   make-color
			   color-space
			   color-precision
			   color-white-point
			   convert-color
			   color->string
			   string->color
			   D65
			   D50

			   ; 5.11.2 Color Spaces
			   CIEXYZ->color
			   color:CIEXYZ
			   color->CIEXYZ
			   RGB709->color
			   color:RGB709
			   color->RGB709
			   L*a*b*->color
			   color:L*a*b*
			   color->L*a*b*
			   L*u*v*->color
			   color:L*u*v*
			   color->L*u*v*
			   L*C*h->color
			   color:L*C*h
			   color->L*C*h
			   sRGB->color
			   color:sRGB
			   color->sRGB
			   xRGB->color
			   color->xRGB
			   e-sRGB->color
			   color:e-sRGB
			   color->e-sRGB

			   ; 5.11.3 Spectra
			   ;; (require 'cie1931)  ; I simply hard-coded
			   ;; (require 'cie1964)  ; the csi1931 data into this egg
			   ;; (require 'ciexyz)
			   read-cie-illuminant
			   read-normalized-illuminant
			   illuminant-map
			   illuminant-map->XYZ
			   spectrum->XYZ
			   spectrum->chromaticity
			   wavelength->XYZ
			   wavelength->chromaticity
			   blackbody-spectrum
			   temperature->XYZ
			   temperature->chromaticity
			   XYZ->chromaticity
			   chromaticity->CIEXYZ
			   chromaticity->whitepoint
			   XYZ->xyY
			   xyY->XYZ
			   xyY:normalize-colors

			   ; 5.11.4 Color Difference Metrics
			   L*a*b*:DE*
			   CIE:DE*
			   L*a*b*:DE*94
			   CIE:DE*94
			   CMC-DE
			   CMC:DE*

			   ; 5.11.5 Color Conversions (low level)
			   CIEXYZ:D65
			   CIEXYZ:D50
			   CIEXYZ:A
			   CIEXYZ:B
			   CIEXYZ:C
			   CIEXYZ:E
			   color:linear-transform
			   CIEXYZ->RGB709
			   RGB709->CIEXYZ
			   CIEXYZ->L*u*v*
			   CIEXYZ->L*u*v*
			   L*u*v*->CIEXYZ
			   L*u*v*->CIEXYZ
			   CIEXYZ->L*a*b*
			   CIEXYZ->L*a*b*
			   L*a*b*->CIEXYZ
			   L*a*b*->CIEXYZ
			   L*a*b*->L*C*h
			   L*C*h->L*a*b*
			   CIEXYZ->sRGB
			   sRGB->CIEXYZ
			   CIEXYZ->xRGB
			   xRGB->CIEXYZ
			   sRGB->xRGB
			   xRGB->sRGB
			   CIEXYZ->e-sRGB
			   e-sRGB->CIEXYZ
			   sRGB->e-sRGB
			   e-sRGB->sRGB
			   e-sRGB->e-sRGB

			   ; 5.11.6 Color Names
			   ;; not implemented - requires his database extension
			   ;; but this would be really nice
			   ;; look at the x11-colors egg which exposes RGB values
			   ;; from rgb.txt as vectors. It provides a function called
			   ;; (color)

			   ; 5.11.7 Daylight
			   solar-hour
			   solar-declination
			   solar-polar
			   sunlight-spectrum
			   sunlight-chromaticity
			   zenith-xyY
			   overcast-sky-color-xyY
			   clear-sky-color-xyY
			   sky-color-xyY
			   )

  (import scheme chicken)

  (use fmt records data-structures srfi-13 ports)

  (include "slib-compat.scm")
  (include "colorspc.scm")
  (include "daylight.scm")
  (include "color.scm"))
