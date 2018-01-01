# MICRO v2.0

## Code Conventions

  * Follow proper OOP design philosophy
    * "is-a" relationships: use abstract base classes properly with polymorphism for consistent interface between different implementations (eg; PfMOI, PfSI, PfLOME should share an interface)
    * "has-a" relationships: use composition (eg; an human 'has' pathogens and immune system inside him/her)

  * Nitpicky things
    * don't store objects in things; store references/pointers to objects in containers (eg; call object$new() 'outside' the container, then put its reference/pointer inside the container)

## Software Design

  * Tile:
    * Contains all the necessary bits needed for MICRO to function
    * Every object in a tile has a pointer to the tile, and only to the tile

  * Naming:
    * R code:
      * XX-Class.R provides the declarations (sort of like .hpp)
      * XX-Methods.R provides the definitions (sort of like .cpp)
    * C++ code:

  * Inheritance & Polymorphism:
    * R Code:
      * R code conventions should mimic as much as possible standard OOP conventions
      * Abstract base classes (ABC) should declare all the 'virtual' functions that inheriting classes need to implement, but should fail by `stop` if they are ever called in the ABC.
    * C++ code:
      * Follow C++11/14 standards, if you don't want something to have a copy constructor/copy assignment operator, `delete` those methods!
      * Follow item 17 (Effective Modern C++): if you want something to have move constructor/move assignment operator, either give them definitions or use `default`!

## Ramblings

Why is this strict design philosophy necessary?
  1. to keep developers sane, without it, code complexity explodes and new people cannot understand what is happening.
  2. to have a reasonable efficiency/understand-ability tradeoff, by keeping standards, we understand how implementations interact, and by keeping standards reasonably efficient, we keep implementations reasonably efficient while still being relatively transparent. We do not want opaque code and obfuscated implementations.
  3. to make porting the code to C++ easy, if it ever happens.
