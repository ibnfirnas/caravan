open Core
open Async


module Log : sig
  module Level : sig
    type t =
      | Info
      | Debug
  end

  module Msg : sig
    type t
  end

  type t

  exception Attempt_to_write_to_closed_log_channel

  val initialize : unit -> t

  val info : t -> string -> unit

  val debug : t -> string -> unit

  val finalize : t -> Msg.t list Deferred.t
  (** [finalize] is idempotent. *)

  val msgs_to_string : ?filter:Level.t list option -> Msg.t list -> string
  (** [filter] defaults to None, which means messages will not be filtered and
    * all levels will be included. Otherwise specify a list of levels to
    * include. *)
end = struct
  module Level = struct
    type t =
      | Info
      | Debug

    let to_string = function
      | Info  -> "INFO"
      | Debug -> "DEBUG"
  end

  module Msg = struct
    type t =
      { timestamp : Time.t
      ; level     : Level.t
      ; payload   : string
      }

    let to_string {timestamp; level; payload} =
      let level = Level.to_string level in
      sprintf "%s %s => %s" (Time.to_string timestamp) level payload
  end

  type channel =
    { r : Msg.t Pipe.Reader.t
    ; w : Msg.t Pipe.Writer.t
    }

  type state =
    | Open   of channel
    | Closed of Msg.t list

  type t =
    state ref

  exception Attempt_to_write_to_closed_log_channel

  let initialize () =
    let r, w = Pipe.create () in
    ref (Open {r; w})

  let post t level payload =
    match !t with
    | Open {w; _} ->
        let msg =
          { Msg.timestamp = Time.now ()
          ;     level
          ;     payload
          }
        in
        Pipe.write_without_pushback w msg
    | Closed _ ->
        raise Attempt_to_write_to_closed_log_channel

  let info  t payload = post t Level.Info  payload
  let debug t payload = post t Level.Debug payload

  let finalize t =
    match !t with
    | Open {r; w} ->
        Pipe.close w;
        Pipe.to_list r >>= fun msgs ->
        t := Closed msgs;
        return msgs
    | Closed msgs ->
        return msgs

  let msgs_to_string ?(filter=None) msgs =
    let msgs =
      let module M = Msg in
      match filter with
      | None        -> msgs
      | Some levels -> List.filter msgs ~f:(fun m -> List.mem levels m.M.level ~equal:(=) )
    in
    String.concat ~sep:"\n" (List.map msgs ~f:Msg.to_string)
end

module Test = struct
  module Id = struct
    type t = string
  end

  module Result = struct
    type 'state result =
      { id     : Id.t
      ; time   : Time.Span.t
      ; output : ('state, exn) Result.t
      ; log    : Log.Msg.t list
      }

    type 'state t =
      | Ran     of 'state result
      | Skipped of Id.t
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

  let add_child ({children; _} as t1) t2 =
    {t1 with children = t2 :: children}

  let add_children ({children; _} as t) ts =
    {t with children = children @ ts}

  let iter ts ~f =
    let rec iter_parent t ~f =
      f t;
      iter_children t.children ~f
    and iter_children ts ~f =
      List.iter ts ~f:(iter_parent ~f)
    in
    iter_children ts ~f

  let get_ids ts =
    let ids = ref [] in
    iter ts ~f:(fun t -> ids := t.id :: !ids);
    !ids
end

module Test_infix = struct
  let (-->) = Test.add_child
  let (>>>) = Test.add_children
end


let reporter ~results_r =
  let report_of_results results =
    let module C = Ascii_table.Column in
    let module R = Test.Result in
    let time_span_to_string ts = sprintf "%.2f" (Time.Span.to_proportional_float ts) in
    let na = "N/A" in
    let get_id = function
      | R.Ran {R.id; _} -> id
      | R.Skipped id    -> id
    in
    let get_status = function
      | R.Ran {R.output = Ok    _; _} -> [`Bright; `White; `Bg `Green], " PASS "
      | R.Ran {R.output = Error _; _} -> [`Bright; `White; `Bg `Red  ], " FAIL "
      | R.Skipped _                   -> [`Reverse                   ], " SKIP "
    in
    let get_time = function
      | R.Ran {R.time; _} -> time_span_to_string time
      | R.Skipped _       -> na
    in
    let get_error = function
      | R.Ran {R.output = Ok    _; _} -> ""
      | R.Ran {R.output = Error e; _} -> Exn.to_string e
      | R.Skipped _                   -> na
    in
    let get_log = function
      | R.Ran {R.log; output; _} ->
          let filter =
            match output with
            | Ok _    -> Some [Log.Level.Info]
            | Error _ -> None
          in
          Log.msgs_to_string log ~filter
      | R.Skipped _      -> na
    in
    let rows = List.rev results in
    let columns =
      [ C.create_attr "Status" get_status
      ; C.create      "ID"     get_id
      ; C.create      "Time"   get_time
      ; C.create      "Error"  get_error ~show:`If_not_empty
      ; C.create      "Log"    get_log   ~show:`If_not_empty
      ]
    in
    Ascii_table.to_string
      ~display:Ascii_table.Display.tall_box
      ~bars:`Unicode
      ~limit_width_to:300   (* TODO: Should be configurable *)
      columns
      rows
  in
  let rec gather results total_failures =
    Pipe.read results_r >>= function
    | `Eof  ->
        printf "\n\n%!";
        return (results, total_failures)
    | `Ok r ->
        let module R = Test.Result in
        let post_progress = function
          | R.Ran {R.output = Ok    _; _} -> printf "."
          | R.Ran {R.output = Error _; _} -> printf "F"
          | R.Skipped _                   -> printf "-"
        in
        post_progress r;
        let total_failures =
          match r with
          | R.Skipped _                 ->      total_failures
          | R.Ran {R.output=Ok    _; _} ->      total_failures
          | R.Ran {R.output=Error _; _} -> succ total_failures
        in
        gather (r :: results) total_failures
  in
  gather [] 0 >>= fun (results, total_failures) ->
  print_endline (report_of_results results);
  return total_failures

let runner ~tests ~init_state ~results_w =
  let rec skip_parent {Test.id; children; _} =
    let result = Test.Result.(Skipped id) in
    Pipe.write_without_pushback results_w result;
    skip_children children
  and skip_children tests =
    Deferred.List.iter
      tests
      ~how:`Parallel
      ~f:skip_parent
  in
  let rec run_parent {Test.id; case; children} ~state =
    let log_channel = Log.initialize () in
    let time_started = Time.now () in
    try_with ~extract_exn:false (fun () -> case state ~log:log_channel)
    >>= fun output ->
    let time_finished = Time.now () in
    let time_elapsed = Time.diff time_finished time_started in
    Log.finalize log_channel
    >>= fun log_msgs ->
    let result =
      Test.Result.(Ran {id; time=time_elapsed; output; log=log_msgs})
    in
    Pipe.write_without_pushback results_w result;
    match output with
    | Ok state ->  run_children children ~state
    | Error _  -> skip_children children
  and run_children tests ~state =
    Deferred.List.iter
      tests
      ~how:`Parallel
      ~f:(run_parent ~state)
  in
  run_children tests ~state:init_state >>| fun () ->
  Pipe.close results_w

let assert_unique_ids tests =
  let id_unit_pairs = List.map (Test.get_ids tests) ~f:(fun id -> id, ()) in
  match String.Table.of_alist_report_all_dups id_unit_pairs with
  | `Ok _ -> return ()
  | `Duplicate_keys ids ->
      eprintf "Duplicate test IDs: %s\n" (List.to_string ids ~f:Fn.id);
      exit 1

let run ~tests ~init_state =
  assert_unique_ids tests
  >>= fun () ->
  let results_r, results_w = Pipe.create () in
  don't_wait_for (runner   ~results_w ~tests ~init_state);
                  reporter ~results_r
  >>= fun total_failures ->
  exit total_failures
