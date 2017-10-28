# Object-Oriented Programming Quick Guide

_Héctor M. Sánchez C. :: November 2017_

## Introduction

### Paradigm

Data-structure driven, instead of instructions-flow driven (procedural).

### Class VS Object


## Principles

### Encapsulation

#### Members' types

* public
* private
* protected

### Composition

### Inheritance

### Polymorphism


## Good Practices

1. Classes variables are generally stored as private (or protected)

## OOP Basic Scheme in R

Class definition header:

```R
Mosquito = R6::R6Class(
  # R6 definitions
  classname = "Mosquito",
  portable      = TRUE,
  cloneable     = FALSE,
  lock_class    = FALSE,
  lock_objects  = FALSE,
  # Private members
  private = list(
    age       = NULL,
    isFemale  = NULL,
    ageOneDay = function(){self$age=self$age + 1}
  )
  public = list(
    # Constructors
    initialize=function(){
      self$age      = 0
      self$isFemale = as.logical(round(runif(1,0,1)))
    }
    # Accessors
    get_age=function(){self$age}
    get_femaleSex=function(){self$femaleSex}
    # Mutators
  )
)
```

### The R6 header

* portable: Cross-package inheritance is allowed if set to *TRUE*.
* cloneable: If set to *TRUE*, classes contain a _clone()_ method that creates an exact copy of the object.
* lock_class: If set to *FALSE*, the class can be extended and modified _on the fly_.
* lock_objects: Similar to _lock_class()_, if set to *TRUE*, no new methods can be added to objects after instantiation.

### Private Members

In general, all the variables that form part of a class' definition should be defined as private.

### Public Members

Functions are usually defined as public if they are meant to interact with other classes of objects.

#### Initializer

This function gets called each time an object of this class is instantiated. In other OOP languages this is called the "constructor".

#### Destructor

Gets called when an instance of a class gets destroyed (when it no longer in use and gets cleaned up by the program or the garbage collector).