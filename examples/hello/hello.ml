open Core
open Async

module Log  = Caravan.Log
module Test = Caravan.Test

let test_hello_pass =
  let case () ~log =
    Log.info  log "Info level is always reported.";
    Log.debug log "Asserting 2 + 2 = 4";
    assert (2 + 2 = 4);
    return ()
  in
  {Test.id = "test_hello_pass"; case; children = []}

let test_hello_fail =
  let case () ~log =
    Log.debug log "Asserting 2 + 2 = 5";
    assert (2 + 2 = 5);
    return ()
  in
  {Test.id = "test_hello_fail"; case; children = []}

let main () =
  let tests =
    [ test_hello_pass
    ; test_hello_fail
    ]
  in
  Caravan.run ~tests ~init_state:()

let () =
  don't_wait_for (main ());
  never_returns (Scheduler.go ())
