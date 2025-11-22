open Printf
open Lwt.Infix
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

let setup_logs () = 
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  
type info = {
  modelId : int;
  username : string;
  streamName : string;
  webRTCAppKey : string;
  status : string;
  snapshotTimestamp : int;
  isLive : bool;
  isMobile : bool;
  isDeleted : bool;
  isBlocked : bool;
  isApprovedModel : bool;
  isNonNude : bool;
  isMicInactive : bool;
  isBadStream : bool;
} [@@deriving yojson {strict=false}]


type response = {
  item : info;
} [@@deriving yojson {strict=false}]

type ext_inf = {
  title : string;
  logo_link : string;
  play_link : string;
}

type ext_m3u = {
  ext_inf_list : ext_inf list;
}

let ext_inf_of_info info = 
  let model_id = (string_of_int info.modelId) in
  let logo_link  =  ("https://adultiptv.net/livecams/image.php?id=sc_" ^ model_id) in
  let play_link = ("https://adultiptv.net/livecams/stream.php?id=sc_" ^ model_id) in
  { title=info.username; logo_link=logo_link;play_link=play_link }

let ext_m3u_of_info_list info_list = 
  let ext_inf_list = List.map ext_inf_of_info info_list in
  {ext_inf_list = ext_inf_list }

let print_ext_inf fmt ei = 
  Format.pp_print_string fmt "#EXTINF:-1 ";
  Format.fprintf fmt "tvg-logo=%S,%s\n" ei.logo_link ei.title;
  Format.pp_print_string fmt ei.play_link;
  Format.pp_print_newline fmt ();
  Format.pp_print_flush fmt ()
  
let print_ext_m3u fmt em = 
  Format.pp_print_string fmt "#EXTM3U\n";
  em.ext_inf_list 
  |> List.iter (print_ext_inf fmt)
 
let decode_response json_str = 
  json_str 
  |> Yojson.Safe.from_string 
  |> response_of_yojson 

let fetch_response name =
  let link = ("https://zh.stripchat.com/api/front/v1/broadcasts/" ^ name) in
  let url = Uri.of_string link in
  try 
    Client.get url >>= fun (resp, body) ->
    let code = resp 
      |> Response.status 
      |> Code.code_of_status in
    (* Logs.debug (fun m -> m "Http Code: %d" code); *)
    if code <> 200 then 
      Lwt.return_error (sprintf "Error fetch response: name=%S http-status-code=%d" name code) 
    else
      (* header *)
      (* let header = resp |> Response.headers in *)
      (* Logs.debug (fun m -> m "Http Header:\n%s" (header |> Header.to_string)); *)
      (* body *)
      Body.to_string body >|= fun data ->
      (* Logs.debug (fun m ->  m "Http Body length: %d" (String.length data)); *)
      Ok data
  with 
    ex -> Lwt.return_error (sprintf "Error fetch response: name=%S ex=%s" name (Printexc.to_string ex))

let fetch_info name = 
  Logs.info (fun m -> m "Fetch info: name=%S" name);
  fetch_response name >|= function
  | Ok str -> decode_response str |> Result.map (fun res -> res.item)
  | Error ex -> Error ex

let fetch_info_list name_list = 
  name_list 
  |> List.map fetch_info 
  |> Lwt.all
  
let read_lines path =
    Lwt_io.with_file ~mode:Lwt_io.Input path (fun in_c ->
      Lwt_io.read_lines in_c |> Lwt_stream.to_list
    )

let main () = 
  read_lines "./st.txt" >>= fun name_list -> 
  fetch_info_list name_list >|= fun result_list ->
  let info_list = result_list |> List.filter_map Result.to_option in
  ext_m3u_of_info_list info_list
  
let with_out_file path f =
    let out_c = open_out path in
    Fun.protect ~finally:(fun () -> close_out out_c)
    (fun () -> f out_c)

let save_ext_m3u file em =
  with_out_file file (fun out_c ->
    let fmt = Format.formatter_of_out_channel out_c in
    print_ext_m3u fmt em;
  )
    
let () = 
  setup_logs ();
  Logs.info (fun m -> m "Begin");
  let main = main () in
  let em =  Lwt_main.run main in
  save_ext_m3u "st.m3u" em
