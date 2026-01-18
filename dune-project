(lang dune 3.13)

(name llm_mcp)
(version 0.1.0)

(generate_opam_files true)

(source
 (github jeong-sik/llm-mcp))

(authors "Jeongsik Yun")

(maintainers "Jeongsik Yun")

(license MIT)

(documentation https://github.com/jeong-sik/llm-mcp)

(package
 (name llm_mcp)
 (synopsis "Multi-LLM MCP Server in OCaml")
 (description "MAGI Trinity: Unified MCP wrapper for Gemini, Claude, and Codex CLIs")
 (depends
  (ocaml (>= 5.1.0))
  (dune (>= 3.13))
  ; External dependencies (opam pin)
  (mcp_protocol (>= 0.1.0))
  (grpc-direct (>= 0.1.0))
  (compact-protocol (>= 0.1.0))
  ; Standard dependencies
  (yojson (>= 2.0))
  (cmdliner (>= 1.1))
  (lwt (>= 5.6))
  (lwt_ppx (>= 2.1))
  (cohttp-lwt-unix (>= 5.0))
  (ppx_deriving_yojson (>= 3.6))
  (caqti (>= 2.0))
  (caqti-lwt (>= 2.0))
  (caqti-driver-postgresql (>= 2.0))
  (re (>= 1.10))
  (base64 (>= 3.5))
  (msgpck (>= 1.7))
  (uri (>= 4.2))
  (ctypes (>= 0.20))
  (ctypes-foreign (>= 0.20))
  (integers (>= 0.7))
  (zstd (>= 0.4))
  (domainslib (>= 0.5))
  (alcotest (and :with-test (>= 1.6)))
  (alcotest-lwt (and :with-test (>= 1.6)))))
