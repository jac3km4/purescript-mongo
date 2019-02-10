
let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20181209/src/mkPackage.dhall sha256:8e1c6636f8a089f972b21cde0cef4b33fa36a2e503ad4c77928aabf92d2d4ec9

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20181209/src/packages.dhall sha256:c63285af67ae74feb2f6eb67521712441928d2726ea10e2040774849ca765027

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
