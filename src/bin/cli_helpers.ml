open Cmdliner

let setup_log style_renderer level =
  (match style_renderer with
  | Some style_renderer -> Fmt_tty.setup_std_outputs ~style_renderer ()
  | None -> Fmt_tty.setup_std_outputs ());
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
let setup_log =
  let open Term in
  const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ()
let const_log p =
  let open Term in
  const (fun () -> p) $ setup_log
