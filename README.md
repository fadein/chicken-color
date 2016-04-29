# chicken-color
CHICKEN Scheme extension - a port of Aubrey Jaffer's SLIB color module

This is a module suitable for converting between color spaces which
are designed around the human sense of vision instead of the
peculiarities and limitations of display hardware. These are color
spaces with names such as XYZ, L*a*b*, L*u*v*, L*C*h (if you are 
familiar with the HSV and HSL color spaces, you're close to the idea).

Quoting http://people.csail.mit.edu/jaffer/slib/Color.html#Color

```
 _ _ 
( | )  The goals of this package are to provide methods to specify,
 \|\|  compute, and transform colors in a core set of additive color
       spaces. The color spaces supported should be sufficient for 
       working with the color data encountered in practice and the 
       literature."
```
