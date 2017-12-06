/**
 * \file smoothsort.cpp
 * \brief This file contains code for smoothsort
 *
 * Smoothsort is used to sort the locations in the direction of the wind.
 * Source: http://en.wikibooks.org/wiki/Algorithm_Implementation/Sorting/Smoothsort#C.2B.2B
 *
 */

/**
**  SmoothSort function template + helper functions.
**
**    Formal type T should have a comparison operator >= with prototype:
**
**      bool T::operator >= (const T &) const throw ();
**
**    which should compare its arguments by the given relation
**     (possibly taking advantage of the type itself).
**
**
**/



/**  Sort an array in ascending order.  **/
template <typename T>
void smoothsort (T *, unsigned) throw ();



namespace

/**
**  Helper function's local namespace (declarations).
**
**/

{
  class LeonardoNumber

   /**
	**  Helper class for manipulation of Leonardo numbers
	**
	**/

	{
	  public:
		/**  Default ctor.  **/
		LeonardoNumber (void) throw () : b (1), c (1)
		  { return; }

		/**  Copy ctor.  **/
		LeonardoNumber (const LeonardoNumber & _l) throw () : b (_l.b), c (_l.c)
		  { return; }

		/**  
		 **  Return the "gap" between the actual Leonardo number and the
		 **  preceding one.
		 **/
		unsigned gap (void) const throw ()
		  { return b - c; }


		/**  Perform an "up" operation on the actual number.  **/
		LeonardoNumber & operator ++ (void) throw ()
		  { unsigned s = b; b = b + c + 1; c = s; return * this; }

		/**  Perform a "down" operation on the actual number.  **/
		LeonardoNumber & operator -- (void) throw ()
		  { unsigned s = c; c = b - c - 1; b = s; return * this; }

		/**  Return "companion" value.  **/
		unsigned operator ~ (void) const throw ()
		  { return c; }

		/**  Return "actual" value.  **/
		operator unsigned (void) const throw ()
		  { return b; }


	  private:
		unsigned b;   /**  Actual number.  **/
		unsigned c;   /**  Companion number.  **/
	};


  /**  Perform a "sift up" operation.  **/
  template <typename T>
  inline void sift (T *, unsigned, LeonardoNumber) throw ();

  /**  Perform a "semi-trinkle" operation.  **/
  template <typename T>
  inline void semitrinkle (T *, unsigned, unsigned long long, LeonardoNumber) throw ();

  /**  Perform a "trinkle" operation.  **/
  template <typename T>
  inline void trinkle (T *, unsigned, unsigned long long, LeonardoNumber) throw ();
}


template <typename T>
void smoothsort (T * _m, unsigned _n) throw ()

/**
**  Sorts the given array in ascending order.
**
**    Usage: smoothsort (<array>, <size>)
**
**    Where: <array> pointer to the first element of the array in question.
**            <size> length of the array to be sorted.
**
**
**/

{
  if (!(_m && _n)) return;

  unsigned long long p = 1;
  LeonardoNumber b;

  for (unsigned q = 0; ++q < _n ; ++p)
	if (p % 8 == 3)
	  {
		sift<T> (_m, q - 1, b);

		++++b; p >>= 2;
	  }

	else if (p % 4 == 1)
	  {
		if (q + ~b < _n)  sift<T> (_m, q - 1, b);
		else  trinkle<T> (_m, q - 1, p, b);

		for (p <<= 1; --b > 1; p <<= 1)  ;
	  }

  trinkle<T> (_m, _n - 1, p, b);

  for (--p; _n-- > 1; --p)
	if (b == 1)
	  for ( ; !(p % 2); p >>= 1)  ++b;

	else if (b >= 3)
	  {
		if (p)  semitrinkle<T> (_m, _n - b.gap (), p, b);

		--b; p <<= 1; ++p;
		semitrinkle<T> (_m, _n - 1, p, b);
		--b; p <<= 1; ++p;
	  }


  return;
}

namespace

/**
**  Helper function's local namespace (definitions).
**
**/

{
  template <typename T>
  inline void sift (T * _m, unsigned _r, LeonardoNumber _b) throw ()

   /**
	**  Sifts up the root of the stretch in question.
	**
	**    Usage: sift (<array>, <root>, <number>)
	**
	**    Where:     <array> Pointer to the first element of the array in
	**                       question.
	**                <root> Index of the root of the array in question.
	**              <number> Current Leonardo number.
	**
	**
	**/

	{
	  unsigned r2;

	  while (_b >= 3)
		{
		  if (_m [_r - _b.gap ()] >= _m [_r - 1])
			r2 = _r - _b.gap ();
		  else
			{ r2 = _r - 1; --_b; }

		  if (_m [_r] >= _m [r2])  break;
		  else
			{ swap(_m [_r], _m [r2]); _r = r2; --_b; }
		}


	  return;
	}


  template <typename T>
  inline void semitrinkle (T * _m, unsigned _r, unsigned long long _p,
										   LeonardoNumber _b) throw ()

   /**
	**  Trinkles the roots of the stretches of a given array and root when the
	**  adjacent stretches are trusty.
	**
	**    Usage: semitrinkle (<array>, <root>, <standard_concat>, <number>)
	**
	**    Where:           <array> Pointer to the first element of the array in
	**                             question.
	**                      <root> Index of the root of the array in question.
	**           <standard_concat> Standard concatenation's codification.
	**                    <number> Current Leonardo number.
	**
	**
	**/

	{
	  if (_m [_r - ~_b] >= _m [_r])
		{
		  swap(_m [_r], _m [_r - ~_b]);
		  trinkle<T> (_m, _r - ~_b, _p, _b);
		}


	  return;
	}


  template <typename T>
  inline void trinkle (T * _m, unsigned _r, unsigned long long _p,
										LeonardoNumber _b) throw ()

   /**
	**  Trinkles the roots of the stretches of a given array and root.
	**
	**    Usage: trinkle (<array>, <root>, <standard_concat>, <number>)
	**
	**    Where:           <array> Pointer to the first element of the array in
	**                             question.
	**                      <root> Index of the root of the array in question.
	**           <standard_concat> Standard concatenation's codification.
	**                    <number> Current Leonardo number.
	**
	**
	**/

	{
	  while (_p)
		{
		  for ( ; !(_p % 2); _p >>= 1)  ++_b;

		  if (!--_p || (_m [_r] >= _m [_r - _b]))  break;
		  else
			if (_b == 1)
			  { swap(_m [_r], _m [_r - _b]); _r -= _b; }

			else if (_b >= 3)
			  {
				unsigned r2 = _r - _b.gap (), r3 = _r - _b;

				if (_m [_r - 1] >= _m [r2])
				  { r2 = _r - 1; _p <<= 1; --_b; }

				if (_m [r3] >= _m [r2])
				  { swap(_m [_r], _m [r3]); _r = r3; }

				else
				  { swap(_m [_r], _m [r2]); _r = r2; --_b; break; }
			  }
		}

	  sift<T> (_m, _r, _b);


	  return;
	}
}
