/*! \file structs.h
 * \brief provide useful structs for pgplotxx (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/03/2015
 * 
 * provide useful structs for pgplotxx (prototypes)
 * 
 * Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 17/03/2015   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_PGPLOTSTRUCTS_H_VERSION

#define TF_PGPLOTSTRUCTS_H_VERSION \
  "TF_PGPLOTSTRUCTS_H   V1.0   (17-03-2015)"

namespace pgplot {

/*! \brief Interface provided through structs.h
 *
 * \defgroup pgplotcpp_h Interface provided through structs.h
 */
/*@{*/

/*! \brief hold any parameter range
 * \sa basic_device::rnge
 * \sa Trect
 * \warning
 *   The definition of total() and absrange() was counter-intuitive!
 *   Both functions are renamed (18.1.2011):
 *    - total() --> abs()
 *    - absrange() --> fullrange()
 */
struct Trange {
  //! range \a [min....max]
  float min,max;
  //! set constgructor
  Trange(const float& themin, const float& themax):
    min(themin), max(themax) { }
  //! copy constgructor
  Trange(const Trange& range):
    min(range.min), max(range.max) { }
  //! default constructor
  Trange(): min(0.), max(0.) { }
  //! extend this range if argument range is larger
  Trange& extend(const Trange& range) {
    min=(min < range.min ? min : range.min);
    max=(max > range.max ? max : range.max);
    return(*this);
  }
  //! extend at both ends by a fraction of total range
  Trange& extendf(const float& f) {
    float d=f*this->abs();
    min -= d;
    max += d;
    return(*this);
  }
  //! shrink this range if argument range is smaller
  Trange& shrink(const Trange& range) {
    min=(min > range.min ? min : range.min);
    max=(max < range.max ? max : range.max);
    return(*this);
  }
  //! fractional shrink/extend 
  Trange& shrinkf(const Trange& range) {
    float ar=this->fullrange();
    max=min+ar*range.max;
    min=min+ar*range.min;
    return(*this);
  }
  //! return absolute maximum
  float absmax() const {
    float amin=(min < 0 ? -min : min);
    float amax=(max < 0 ? -max : max);
    return(amax > amin ? amax : amin);
  }
  //! return total range
  float abs() const {
    float absrange=this->fullrange();
    return(absrange > 0 ? absrange : -absrange);
  }
  //! return absolute range
  float fullrange() const { return(max-min); }
  //! fractional cut at lower edge
  Trange& lcutf(const float& fraction)
  { min=min+fraction*(max-min); return(*this); }
  //! fractional cut at higher edge
  Trange& hcutf(const float& fraction)
  { max=max-fraction*(max-min); return(*this); }
  //! absolute cut at lower edge
  Trange& lcut(const float& value)
  { min+=value; return(*this); }
  //! absolute cut at higher edge
  Trange& hcut(const float& value)
  { max-=value; return(*this); }
  //! scale both values
  template<typename T>
  Trange& operator*=(const T& value)
  { min*=value; max*=value; return(*this); }
  //! scale relative to other range
  Trange& operator*=(const Trange& range)
  { 
    // order matters!
    float d=this->fullrange();
    max=min+range.max*d; 
    min+=range.min*d; 
    return(*this); 
  }
  //! swap range
  Trange& swap() { float junk=min; min=max; max=junk; return(*this); }
  //! check if value is inside range
  bool contains(const float& v) const { return((v>=this->min) &&
                                             (v<=this->max)); }
}; // struct Trange

/*----------------------------------------------------------------------*/

/*! \brief hold any rectangle
 * \sa basic_device::rect
 * \sa basic_device::svp
 */
struct Trect {
  //! coordinate ranges
  Trange x,y;
  //! set constructor
  Trect(const float& xmin, const float& xmax,
        const float& ymin, const float& ymax):
    x(Trange(xmin,xmax)), y(Trange(ymin,ymax)) { }
  //! copy constructor
  Trect(const Trect& rect):
    x(rect.x), y(rect.y) { }
  //! set constructor
  Trect(const Trange& thex, const Trange& they): x(thex), y(they) { }
  //! default constructor
  Trect(): x(0.,0.), y(0.,0.) { }
  //! fractional cut at left edge
  Trect& lcutf(const float& fraction)
  { x.lcutf(fraction); return(*this); }
  //! fractional cut at right edge
  Trect& rcutf(const float& fraction)
  { x.hcutf(fraction); return(*this); }
  //! fractional cut at top edge
  Trect& tcutf(const float& fraction)
  { y.hcutf(fraction); return(*this); }
  //! fractional cut at bottom edge
  Trect& bcutf(const float& fraction)
  { y.lcutf(fraction); return(*this); }
  //! absolute cut at left edge
  Trect& lcut(const float& fraction)
  { x.lcut(fraction); return(*this); }
  //! absolute cut at right edge
  Trect& rcut(const float& fraction)
  { x.hcut(fraction); return(*this); }
  //! absolute cut at top edge
  Trect& tcut(const float& fraction)
  { y.hcut(fraction); return(*this); }
  //! absolute cut at bottom edge
  Trect& bcut(const float& fraction)
  { y.lcut(fraction); return(*this); }
  //! fractional shrink/expand 
  Trect& shrinkf(const Trect& rect)
  { x.shrinkf(rect.x); y.shrinkf(rect.y); return(*this); }
  //! shift rectangle by dx and dy
  Trect& shift(const float& dx, const float& dy)
  { x.min+=dx; x.max+=dx; y.min+=dy; y.max+=dy; return(*this); }
  //! shift rectangle - uses off.min=dx and off.max=dy
  Trect& shift(const Trange& off)
  { this->shift(off.min, off.max); return(*this); }
  //! scale relative to other rect (for viewport calculations)
  Trect& operator*=(const Trect& rect)
  { x*=rect.x; y*=rect.y; return(*this); }
}; // struct Trect

/*----------------------------------------------------------------------*/

/*! \brief hold coordinates
 */
struct Tcoor {
  float x; //!< x-coordinate
  float y; //!< y-coordinate
  //! shift coordinate by a vector \p c
  Tcoor& operator+=(const Tcoor& c)
  { this->x+=c.x; this->y+=c.y; return(*this); }
  //! shift coordinate by a vector \p c
  Tcoor operator+(const Tcoor& c)
  { Tcoor r=(*this); return(r += c); }
  //! return true if coordinate is inside rect \p r
  bool inside(const Trect& r) const
  { return(r.x.contains(this->x) && r.y.contains(this->y)); }
}; // struct Tcoor

/*----------------------------------------------------------------------*/

/*! \brief hold bounding box for text etc.
 */
struct Tbbox {
  Tcoor coor[4]; //!< coordinates of four corners
  //! default constructor
  Tbbox();
  //! initialize from C arrays \p x and \p y
  Tbbox(const float x[4], const float y[4]);
}; // struct Tbbox

/*----------------------------------------------------------------------*/

/*! \brief a struct to hold colour triples
 * i.e. rgb or hls values
 *
 * \note all values should be in the range between 0. and 1., except hue,
 * which can be in the range 0. to 360.
 */
struct Tcol {
  //! red or hue
  union { float r,h; };
  //! red or lightness
  union { float g,l; };
  //! blue or saturation
  union { float b,s; };
}; // struct Tcol

/*@}*/

} // namespace pgplot

#endif // TF_PGPLOTSTRUCTS_H_VERSION (includeguard)

/* ----- END OF structs.h ----- */
