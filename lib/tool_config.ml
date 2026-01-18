(** Tool configuration utilities - MCP config, budget mode *)

(** Budget mode - opt-in token saving defaults *)
let env_truthy value =
  match String.lowercase_ascii value with
  | "1" | "true" | "yes" | "on" -> true
  | "0" | "false" | "no" | "off" -> false
  | _ -> false

let budget_mode_from_env () =
  match Sys.getenv_opt "LLM_MCP_BUDGET_MODE" with
  | Some value -> env_truthy value
  | None -> false

let budget_mode_value (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  json |> member "budget_mode" |> to_bool_option |> Option.value ~default:(budget_mode_from_env ())

(** External MCP Configuration *)

(* MCP server config type *)
type mcp_server_config = {
  name: string;
  url: string option;
  command: string option;
  args: string list;
  server_type: string;  (* "http" or "stdio" *)
}

(* Parse ~/.mcp.json to get external MCP server configs *)
let parse_mcp_config () : mcp_server_config list =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  let config_paths = [
    Filename.concat home "me/.mcp.json";
    Filename.concat home ".mcp.json";
  ] in
  let rec try_paths = function
    | [] -> []
    | path :: rest ->
        try
          let content = In_channel.with_open_text path In_channel.input_all in
          let json = Yojson.Safe.from_string content in
          let open Yojson.Safe.Util in
          let servers = json |> member "mcpServers" |> to_assoc in
          List.filter_map (fun (name, config) ->
            let url = config |> member "url" |> to_string_option in
            let command = config |> member "command" |> to_string_option in
            let args = match config |> member "args" with
              | `List l -> List.filter_map to_string_option l
              | _ -> []
            in
            let server_type = config |> member "type" |> to_string_option |> Option.value ~default:"stdio" in
            (* Include both HTTP and stdio servers *)
            match server_type, url, command with
            | "http", Some u, _ -> Some { name; url = Some u; command = None; args = []; server_type }
            | "stdio", _, Some cmd -> Some { name; url = None; command = Some cmd; args; server_type }
            | _, _, Some cmd -> Some { name; url = None; command = Some cmd; args; server_type = "stdio" }
            | _ -> None
          ) servers
        with _ -> try_paths rest
  in
  try_paths config_paths

(* Cached MCP server configs *)
let mcp_servers = lazy (parse_mcp_config ())

(* Get URL for a specific MCP server *)
let get_mcp_server_url name =
  let servers = Lazy.force mcp_servers in
  List.find_opt (fun s -> s.name = name) servers
  |> Option.map (fun s -> s.url)
  |> Option.join

(* Get full config for a specific MCP server *)
let get_mcp_server_config name =
  let servers = Lazy.force mcp_servers in
  List.find_opt (fun s -> s.name = name) servers
