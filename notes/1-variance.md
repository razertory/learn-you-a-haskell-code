COVARIANCE AND CONTRAVARIANCE
=============================

- Generic types

- if A subclass of B => T<A> subclass of T<B>
  then T covariant in the argument type

- if A subclass of B => T<B> subclass of T<A>
  then T contravariant in the argument type

Java
----

- Arrays in Java are covariant: T[] subclass of Object[]

- In Java types such as lists are INvariant.

  public interface List<A> { ... }

  A subclass of B =/=> List<A> subclass of List<B>
                  =/=> List<B> subclass of List<A>

  (Due to Liskov Substitution Principle)

- But the programmer, in the USE SITE, can use wildcards to play with variance:

    Inheritance: E -|> A -|> S

      invariant(List<A> l): only A
      covariant(List<? extends A> l): A or E
      contravariant(List<? super A> l): A or S

    But, because lists are mutable in Java:

      invariant     => reads and writes
      covariant     => only read
      contravariant => only write

Scala
-----

- Variance annotation on DEFINITION SITE

  sealed abstract class List[+A] ......

  +A means covariant on the type argument
  (-A would have meant contravariant)

  Why is this possible?

    ==> Because these lists are scala.collection.immutable.List

    No mutation => No writes => Only reads => covariant (such as in Java)

  Being immutable, the Scala compiler can choose always the best type:

    scala> class A {}
    defined class A

    scala> class B extends A {}
    defined class B

    scala> class C extends A {}
    defined class C

    scala> val as = List(new A, new A)
    as: List[A] = List(A@3069a360, A@7235f92b)

    scala> val bs = List(new B, new B)
    bs: List[B] = List(B@b14b60a, B@1a7cb3a4)

    scala> val cs = List(new C, new C)
    cs: List[C] = List(C@13ca16bf, C@5be4be74)

    scala> as ++ bs
    res0: List[A] = List(A@3069a360, A@7235f92b, B@b14b60a, B@1a7cb3a4)

    scala> bs ++ bs
    res1: List[B] = List(B@b14b60a, B@1a7cb3a4, B@b14b60a, B@1a7cb3a4)

    scala> cs ++ bs
    res2: List[A] = List(C@13ca16bf, C@5be4be74, B@b14b60a, B@1a7cb3a4)

- The mutable version of Lists in Scala in scala.collection.mutable is defined
as:

  class List[A] ...

  no + nor - means INvariant, why? Because clients can read and write.
