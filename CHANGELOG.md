# Changelog

## 0.2.6

Infrastructure refresh: dep bumps + docs migration.

### Changed

- Bumped `shackle` from git ref `0.6.19` to hex `0.7.1` -- fixes the
  OTP 27+ build break that buoy inherited transitively (old shackle
  versions pulled `granderl 0.1.5` from hex, broken on modern OTP).
- Bumped `foil` from git ref `0.1.2` to hex `0.1.4`.
- CI matrix bumped from OTP 21-26 to OTP 25-28.
- `cowboy` test dep bumped from git ref `2.10.0` to hex `2.12.0`.
  (Newer cowboy releases use a cowlib version constraint that the
  rebar3 bundled in the OTP 25 container image can't parse;
  pinning to 2.12.0 keeps the OTP 25 test row green.)
- `fprofx` test dep moved from `ransomr/fprofx` to `lpgauth/fprofx`
  (`otp_19` branch), matching the rest of the ecosystem.
- Documentation migrated from `edown` to `rebar3_ex_doc`. Generated
  `doc/` directory removed.

No source or API changes.
