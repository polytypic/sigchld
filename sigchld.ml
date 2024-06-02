let mutex = Mutex.create ()
let condition = Condition.create ()
let sigchld = ref 0

let handle_signal n =
  Printf.printf "signal %d \n%!" n;
  incr sigchld;
  (match Mutex.lock mutex with
  | () -> Mutex.unlock mutex
  | exception Sys_error _ -> ());
  Condition.signal condition

let sh = "/bin/sh"

let () =
  Sys.signal Sys.sigchld (Sys.Signal_handle handle_signal) |> ignore;
  Sys.signal Sys.sigalrm (Sys.Signal_handle handle_signal) |> ignore;
  Unix.create_process sh [| sh; "-c"; "ls -l" |] Unix.stdin Unix.stdout
    Unix.stderr
  |> ignore;
  Unix.alarm 10 |> ignore;
  Mutex.lock mutex;
  match Condition.wait condition mutex with
  | () ->
      Printf.printf "returned\n%!";
      Mutex.unlock mutex
  | exception _ ->
      Printf.printf "raised\n%!";
      Mutex.unlock mutex
