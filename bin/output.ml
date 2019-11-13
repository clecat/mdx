(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "cram.pp"
module Log = (val Logs.src_log src : Logs.LOG)

let pp_section ppf (n, title) =
  let pp_n ppf () = Fmt.string ppf (String.make n '#') in
  Fmt.pf ppf "%a %s\n" pp_n () title

let pp_list pp = Fmt.(list ~sep:(unit "") pp)

let pp_text ~html ppf s =
  if html
  then
    let add = function
      | '"' -> Fmt.string ppf "&quot;"
      | '&' -> Fmt.string ppf "&amp;"
      | '<' -> Fmt.string ppf "&lt;"
      | '>' -> Fmt.string ppf "&gt;"
      | c   -> Fmt.char ppf c
    in
    String.iter add s
  else
    Fmt.string ppf s

let pp_output ~html ppf = function
  |`Output s -> Fmt.pf ppf ">%a\n" (pp_text ~html) s
  |`Ellipsis -> Fmt.pf ppf "...\n"

let pp_line ~html ppf l = Fmt.pf ppf "%a\n" (pp_text ~html) l

let pp_toplevel ~html ppf (t:Mdx.Toplevel.t) =
  let cmds = match t.command with [c] -> [c ^ ";;"] | l -> l @ [";;"] in
  Fmt.pf ppf "%a%a" (pp_list (pp_line ~html)) cmds (pp_list (pp_output ~html)) t.output

let pp_contents ~html (t:Mdx.Block.t) ppf =
  Fmt.(list ~sep:(unit "\n") (pp_text ~html)) ppf t.contents

let pp_cram ~html ppf (t:Mdx.Cram.t) =
  let pp_exit ppf = match t.exit_code with
    | 0 -> ()
    | i -> Fmt.pf ppf "[%d]" i
  in
  Fmt.pf ppf "%a%a%t"
    (pp_list (pp_line ~html)) t.command
    (pp_list (pp_output ~html)) t.output pp_exit

let pp_block_html ppf (b:Mdx.Block.t) =
  let lang, pp_code, attrs = match b.value with
    | Toplevel t -> Some "ocaml", (fun ppf -> pp_list (pp_toplevel ~html:true) ppf t), [
        ("class"             , "command-line");
        ("data-prompt"       , "#");
        ("data-filter-output", ">");
      ]
    | OCaml  -> Some "ocaml", (pp_contents ~html:true) b, []
    | Cram t -> Some "bash" , (fun ppf -> pp_list (pp_cram ~html:true) ppf t.tests), [
        ("class"             , "command-line");
        ("data-user"         , "fun");
        ("data-host"         , "lama");
        ("data-filter-output", ">");
      ]
    | Raw     -> b.header, (pp_contents ~html:true) b, []
    | Error s -> Some "error", (fun ppf -> pp_list Fmt.string ppf s), []
  in
  let pp_attr ppf (k, v) = Fmt.pf ppf "%s=%S" k v in
  let pp_lang ppf () = match lang with
    | None   -> Fmt.pf ppf " class=\"language-clike\""
    | Some l -> Fmt.pf ppf " class=\"language-%s\"" l
  in
  let pp_attrs ppf () = match attrs with
    | [] -> ()
    | _  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit " ") pp_attr) attrs
  in
  Fmt.pf ppf "<div class=\"highlight\"><pre%a><code%a>%t</code></pre></div>"
    pp_attrs () pp_lang () pp_code

let pp_block_latex ppf (b:Mdx.Block.t) =
  let lang, pp_code = match b.value with
    | Toplevel t -> Some "ocaml", (fun ppf -> pp_list (pp_toplevel ~html:false) ppf t)
    | OCaml  -> Some "ocaml", (pp_contents ~html:false) b
    | Cram t -> Some "bash" , (fun ppf -> pp_list (pp_cram ~html:false) ppf t.tests)
    | Raw     -> b.header, (pp_contents ~html:false) b
    | Error s -> Some "error", (fun ppf -> pp_list Fmt.string ppf s)
  in
  let lang = match lang with
    | None   -> "clike"
    | Some l -> l
  in
  Fmt.pf ppf "```%s\n%t\n```"
    lang pp_code

let pp_text_html ppf l =
  List.iter (Fmt.pf ppf "%s\n") (List.rev l)

open Astring

let pp_text_latex ppf l =
  let l = List.rev l in
  let t = String.concat ~sep:"\n" l in
  let rec f t acc =
    match String.find_sub ~rev:true ~sub:"]{.idx}" t with
    | None -> t::acc
    | Some i ->
      begin
        match String.find ~rev:true ~start:i (Char.equal '[') t with
        | None -> t::acc
        | Some j ->
          let escape_latex acc c =
            match c with
              | '&' | '%' | '$' | '#' | '_' | '{' | '}' | '~' | '^' |  '\\' ->
                ("\\" ^ (String.of_char c)) :: acc
              | _ -> String.of_char c :: acc
          in
          let b = String.sub ~stop:j t |> String.Sub.to_string in
          let m = String.sub ~start:(j + 1) ~stop:i t |> String.Sub.to_string |> String.fold_left escape_latex [] |> List.rev |> String.concat |> Fmt.strf "\\index{%s}" in
          let e = String.sub ~start:(i + 7) t |> String.Sub.to_string in
          f b (m::e::acc)
      end  in
  let t = f t [] in
  List.iter (Fmt.pf ppf "%s") t;
  Fmt.pf ppf "\n"

type outputs =
    Html
  | Latex

let get_output_infos = function
  | Html -> pp_block_html, pp_text_html, "-t html"
  | Latex -> pp_block_latex, pp_text_latex,  "-t latex --listings"

let run (`Setup ()) (`File file) (`Output output) output_type =
  let (pp_block, pp_text, out_args) = get_output_infos output_type in
  let t = Mdx.parse_file Normal file in
  match t with
  | [] -> 1
  | _  ->
    let tmp = Filename.temp_file "ocaml-mdx" "pandoc" in
    let oc = open_out tmp in
    let ppf = Format.formatter_of_out_channel oc in
    let f acc t =
      match t with
        | Mdx.Section s ->
          Fmt.pf ppf "%a%a" pp_text acc pp_section s;
          []
        | Text t        -> t::acc
        | Block b ->
          let b = Mdx.Block.eval b in
          Log.debug (fun l -> l "output: %a" Mdx.Block.dump b);
          Fmt.pf ppf "%a%a" pp_text acc pp_block b;
          []
    in
    Fmt.pf ppf "%a%!" pp_text (List.fold_left f [] t);
    close_out oc;
    let output = match output with None -> "-" | Some o -> o in
    Fmt.pr "Generating %s...\n%!" output;
    Fmt.kstrf
      Sys.command
      "pandoc \
      \  --section-divs \
      \  -f markdown-ascii_identifiers \
      \  --no-highlight\
      \  %s %s -o %s"
      out_args tmp output

open Cmdliner

let output =
  let doc = "Write output to $(b,FILE) instead of stdout.  If $(b,FILE) is -, \
             output will go to stdout," in
  Term.(app (const (fun x -> `Output x)))
    Arg.(value & opt (some string) None & info ["o";"output"] ~doc ~docv:"FILE")

let out_type =
  let doc = "Output type: html or latex. Default is html" in
  Arg.(value & opt (enum ["html", Html; "latex", Latex]) Html & info ["t";"type"] ~doc ~docv:"TYPE")

let cmd =
  let doc = "Pre-process markdown files to produce OCaml code." in
  let exits = Term.default_exits in
  Term.(pure run $ Cli.setup $ Cli.file $ output $ out_type),
  Term.info "output" ~doc ~exits
