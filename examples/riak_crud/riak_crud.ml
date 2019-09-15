open Core
open Async

module Log      = Caravan.Log
module Test     = Caravan.Test

module Config = struct
  type t =
    { host    : string
    ; port    : int
    ; data1   : string
    ; data2   : string
    ; headers : (string * string) list
    }

  let default =
    { host    = "localhost"
    ; port    = 8098
    ; data1   = "foo"
    ; data2   = "bar"
    ; headers = ["content-type", "text/plain"]
    }
end

module State = struct
  type t =
    { config : Config.t
    ; uri    : string option
    }

  let init ~config =
    { config
    ; uri = None
    }
end

module Request = struct
  type t =
    { uri     : string
    ; method' : [`GET | `PUT | `DELETE]
    ; headers : (string * string) list
    ; body    : string
    }
end

module Response = struct
  type t =
    { status  : int
    ; headers : (string * string) list
    ; body    : string
    }

  let of_cohttp ~resp ~body =
    let module R = Cohttp_async.Response in
    let
      { R.encoding = _
      ;   version  = _
      ;   flush    = _
      ;   status
      ;   headers
      } = resp
    in
    Cohttp_async.Body.to_string body
    >>| fun body ->
    { status  = Cohttp.Code.code_of_status status
    ; headers = Cohttp.Header.to_list      headers
    ; body
    }

  let to_string {status; headers; body} =
    let status  = Int.to_string status in
    let headers = List.map headers ~f:(fun (k, v) -> sprintf "%s: %s" k v) in
    String.concat ~sep:"\n" (status :: headers @ [" "; body])
end

module Http_client = struct
  let exec {Request.uri; method'; headers; body} =
    let uri     = Uri.of_string uri in
    let headers = Cohttp.Header.of_list headers in
    let body    = Cohttp_async.Body.of_pipe (Pipe.of_list [body]) in
    let exec () =
      match method' with
      | `GET    -> Cohttp_async.Client.get    uri ~headers
      | `PUT    -> Cohttp_async.Client.put    uri ~headers ~body
      | `DELETE -> Cohttp_async.Client.delete uri ~headers
    in
    exec () >>= fun (resp, body) ->
    Response.of_cohttp ~resp ~body
end


let t_create =
  let case state ~log =
    let {State.config={Config.data1=body; host; port; headers;_}; _} = state in
    let uri =
      let key = Uuid_unix.create () |> Uuid.to_string in
      sprintf "http://%s:%d/buckets/caravan_examples/keys/%s" host port key
    in
    let req =
      let open Request in
      { uri
      ; method' = `PUT
      ; headers
      ; body
      }
    in
    Http_client.exec req >>= fun resp ->
    let {Response.status; _} = resp in
    Log.debug log (sprintf "Response:\n%s" (Response.to_string resp));
    assert (204 = status);
    return {state with State.uri = Some uri}
  in
  {Test.id = "t_create"; case; children=[]}

let t_read =
  let case state ~log =
    let uri = Option.value_exn state.State.uri in
    let req =
      let open Request in
      { uri
      ; method' = `GET
      ; headers = []
      ; body    = ""
      }
    in
    Http_client.exec req >>= fun resp ->
    let {Response.status; body; _} = resp in
    Log.debug log (sprintf "Response:\n%s" (Response.to_string resp));
    assert (200 = status);
    assert (body = state.State.config.Config.data1);
    return state
  in
  {Test.id = "t_read"; case; children=[]}

let t_update =
  let case state ~log =
    let {State.uri; config={Config.data2; headers; _}; _} = state in
    let uri = Option.value_exn uri in
    (* Update: *)
    let req =
      let open Request in
      { uri
      ; method' = `PUT
      ; headers
      ; body    = data2
      }
    in
    Http_client.exec req >>= fun resp ->
    let {Response.status; _} = resp in
    Log.debug log (sprintf "PUT response:\n%s" (Response.to_string resp));
    assert (204 = status);
    (* Check updated: *)
    let req =
      let open Request in
      { uri
      ; method' = `GET
      ; headers = []
      ; body    = ""
      }
    in
    Http_client.exec req >>= fun resp ->
    let {Response.status; body; _} = resp in
    Log.debug log (sprintf "GET response:\n%s" (Response.to_string resp));
    assert (200 = status);
    assert (body = data2);
    return state
  in
  {Test.id = "t_update"; case; children=[]}

let t_delete =
  let case state ~log =
    let uri = Option.value_exn state.State.uri in
    (* Delete: *)
    let req =
      let open Request in
      { uri
      ; method' = `DELETE
      ; headers = []
      ; body    = ""
      }
    in
    Http_client.exec req >>= fun resp ->
    let {Response.status; _} = resp in
    Log.debug log (sprintf "DELETE response:\n%s" (Response.to_string resp));
    assert (204 = status);
    (* Check its gone: *)
    let req =
      let open Request in
      { uri
      ; method' = `GET
      ; headers = []
      ; body    = ""
      }
    in
    Http_client.exec req >>= fun resp ->
    let {Response.status; _} = resp in
    Log.debug log (sprintf "GET response:\n%s" (Response.to_string resp));
    assert (404 = status);
    return state
  in
  {Test.id = "t_delete"; case; children=[]}

let main ~config =
  let tests =
    let open Caravan.Test_infix in
    [ t_create --> (t_read --> (t_update --> t_delete))
    ]
  in
  Caravan.run ~tests ~init_state:(State.init ~config)

let () =
  let spec =
    let open Command.Spec in
    let module C = Config in
    let (+) = (+>) in
    let host = C.default.C.host in
    let port = C.default.C.port in
    Command.async
      ~summary:""
      ( empty
	+ flag "-host" (optional_with_default host string) ~doc:" Riak host."
	+ flag "-port" (optional_with_default port int   ) ~doc:" Riak port.")
      (fun host port () -> main ~config:{C.default with C.host; port})
  in
  Command.run spec
