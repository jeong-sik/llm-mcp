(lang dune 3.13)

(name llm_mcp)
(version 0.6.1)

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
 (description "Unified MCP wrapper for Gemini, Claude, Codex, and Ollama CLIs with Chain Engine")
 (depends
  (ocaml (>= 5.1.0))
  (dune (>= 3.13))
  dune-build-info
  ; External dependencies (opam pin)
  (mcp_protocol (>= 0.1.0))
  (grpc-direct (>= 0.1.0))
  ; Standard dependencies
  (yojson (>= 2.0))
  (cmdliner (>= 1.1))
  (ppx_deriving_yojson (>= 3.6))
  ; Eio runtime
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  ; HTTP (Eio-native)
  (cohttp (>= 6.0))
  (cohttp-eio (>= 6.0))
  (httpun-eio (>= 0.1))
  (gluten-eio (>= 0.4))
  ; Database (Eio-native)
  (caqti (>= 2.0))
  (caqti-eio (>= 2.0))
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
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (alcotest (and :with-test (>= 1.6)))
  (bisect_ppx (and :with-test (>= 2.8)))
  (odoc :with-doc)))
