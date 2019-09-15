open Async

module Log : sig
  type t

  exception Attempt_to_write_to_closed_log_channel
  (** This can happen if either [info] or [debug] is attempted outside of a
    * test case's scope. Which could only happen if user deliberatley leaks [t]
    * outside the test case thunk. *)

  val info : t -> string -> unit
  (** Info messages are always reported. *)

  val debug : t -> string -> unit
  (** Debug messages are only reported for failed test cases. *)
end

module Test : sig
  module Id : sig
    type t = string
  end

  type 'state t =
    { id       : Id.t
    ; case     : 'state -> log:Log.t -> 'state Deferred.t
    ; children : 'state t list
    }

  type 'state add_child =
       'state t
    -> 'state t
    -> 'state t

  type 'state add_children =
       'state t
    -> 'state t list
    -> 'state t

  val add_child    : 'state add_child
  val add_children : 'state add_children
end

module Test_infix : sig
  val (-->) : 'state Test.add_child
  val (>>>) : 'state Test.add_children
end

val run
  :  tests      : 'state Test.t list
  -> init_state : 'state
  -> unit Deferred.t
