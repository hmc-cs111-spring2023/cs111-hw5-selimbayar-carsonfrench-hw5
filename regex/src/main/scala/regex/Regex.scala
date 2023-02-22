package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

// Add your definitions here

trait RegularLanguage

case object Empty extends RegularLanguage

case object Epsilon extends RegularLanguage

case class Character(char: Char) extends RegularLanguage

case class Union(l1: RegularLanguage, l2: RegularLanguage) extends RegularLanguage

case class Concat(l1: RegularLanguage, l2: RegularLanguage) extends RegularLanguage

case class Star(l: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
  case Concat(Epsilon, lang) => simplify(lang)
  case Concat(lang, Epsilon) => simplify(lang)
  case Concat(Empty, lang) => simplify(Empty)
  case Concat(lang, Empty) => simplify(Empty)
  case Concat(l1, l2) => Concat(simplify(l1), simplify(l2))

  case Union(Empty, lang) => simplify(lang)
  case Union(lang, Empty) => simplify(lang)
  case Union(l1, l2) => Union(simplify(l1), simplify(l2))

  case Star(Epsilon) => Epsilon
  case Star(Empty) => Empty
  case Star(lang) => Star(simplify(lang)) 

  case _ => lang

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = simplify(lang) match
  case Epsilon => true
  case Star(l) => true
  case _ => false


/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match
  case Empty => Empty
  case Epsilon => Empty

  case Character(d) if (c == d) => Epsilon 
  case Character(d) => Empty

  case Union(l1, l2) => Union(derivative(l1)(c), derivative(l2)(c))

  case Concat(l1, l2) if (!nullable(l1)) => Concat(derivative(l1)(c), l2)
  case Concat(l1, l2) => Union(Concat(derivative(l1)(c), l2), derivative(l2)(c))

  case Star(l) => Concat(derivative(l)(c), Star(l))

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
