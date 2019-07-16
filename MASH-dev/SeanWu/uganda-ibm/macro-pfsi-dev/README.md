# bugs

  * SOLVED: for some reason people are getting infectious bites on day 0 when Z = 0, check why this is happening.
    * this wasn't happening, it was just logging error, need to have initial infections start at time = -1. it was also partially due to kappa not being renormalized after initializing course of infection algorithms. Both issues are fixed
  * SOLVED: initial pfsi infection at t=0 doesn't require another call to set_state.
    * fixed.
  * SOLVED: check why initially infected individuals on t=0 report incidence on day t=1.
    * see above note about -1.0 as time for initial infection
  * SOLVED: for some reason people who are infected at t=0 get events added that never fire.
    * was because we use the "state" string, which is I, and PfSI protects from infection when you are in I)

# optimizations

  * pass ref to string in log SIP for patches

# dev thoughts

  * use simultaneous definition of pure virtual and default destructor behavior for base class Event
  * check proper use of braces versus parentheses for member initializer lists (see Item 7: Meyers Modern C++)
  * dont allow copying of polymorphic base classes http://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#c67-a-polymorphic-class-should-suppress-copying
    * check what the rules are for move.
  * derived classes cannot call derived class virtual overrides in their constructor (even the body) until the constructor returns, so if you want to use a base class method, specifiy you want that one: http://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#c82-dont-call-virtual-functions-in-constructors-and-destructors
  * It's possible the subtle bugs are cropping up in the Event hierarchy, this needs to be addressed asap
