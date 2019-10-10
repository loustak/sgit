package com.sardois.sgit

/** Helper functions to chain Either calls  */
class Chain[A, B](e: Either[A, B]) {

    def chain(func: (B) => Option[A]): Option[A] = {
        e match {
            case Left(error) => Some(error)
            case Right(value) => func(value)
        }
    }
}

object Chain {
    def apply[A, B](e: Either[A, B], func: (B) => Option[A]): Option[A] = {
        new Chain(e).chain(func)
    }
}

