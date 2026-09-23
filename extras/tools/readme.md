This folder contains debugging tools for jsonToCapr.R
They help with analyzing why the roundtrip test (json -> Capr -> json) fails
in some cases.

Shared logic (SQL normalization, concept-set reordering, etc.) lives in
`roundtrip_utils.R`; the other scripts source it automatically when run via
Rscript from this directory or from the repo root.
