# Changelog

## 1.1.27 -- 2019-02-16

- Make `jsonErrors` total. Thanks to @gelisam for this fix! See
  https://github.com/ocharles/digestive-functors-aeson/pull/12 for more
  information.

## 1.1.26 -- 2018-07-06

- Increase the upper bound of `lens`.

## 1.1.25

- Increase the upper bound of `aeson`.

## 1.1.24

- Increase upper bound of `aeson`.

## 1.1.23

- Increase upper bound of `lens`.

## 1.1.22

- Increase upper bound of `aeson`.

## 1.1.21

- Increase upper bounds on `base` and `lens`.

## 1.1.20

- Update upper bounds of `base` and `lens`.

## 1.1.19

- Builds with `aeson-0.11`.

## 1.1.18

- Update upper bounds of `aeson`.

## 1.1.17

- Update upper bounds of `lens`.

## 1.1.16

- Update upper bounds of `lens`.

## 1.1.15

- Update upper bounds of `aeson` and `lens`.

## 1.1.14

- Update upper bounds to base < 4.9, digestive-functors < 0.9, lens < 4.10

## 1.1.13

- Builds with lens < 4.8.

## 1.1.12.1

- 1.1.12 included old test code which fails tests. This release includes the correct
  code.

## 1.1.12

- Builds with lens < 4.7.

## 1.1.11

- Builds with 4.4 <= lens < 4.5, aeson < 0.9. This increases the lower-bound on
  the lens dependency.

## 1.1.10

- Builds with lens < 4.4.

## 1.1.9

- Builds with lens < 4.3.

## 1.1.8

- Builds with base 4.7

## 1.1.7

- Correctly parse 'true' and 'false'. Now parses 'true' to be the string "on",
  which means that the 'bool' form provided by `digestive-functors` works as
  expected.

## 1.1.6

- Build with `lens` 4.1.

## 1.1.5

- Upgraded to `lens` 4.0. Dropped `lens-aeson` dependency.

## 1.1.4

- Upgraded to `digestive-functors` 0.7.

## 1.1.3

- This version only includes distribution changes
- The cabal file now correctly specifies the correct version of
  the digestive-functors-aeson build dependency.
- Tests are now ran using tasty rather than test-framework.

## 1.1.2

- Support top level lists. This means you can now parse the JSON document
  `[ 0, 1, 2 ]` with the form `listOf stringRead`.

## 1.1

- Support lists

-----

## 1.0.2

- Added `jsonErrors`, which can transform a `ToJSON a => View a` into a aeson
  `Value`. This respects the validation hierarchy.
- Tests!

-----

## 1.0.1

- Added a dependency on the `Safe` package to build with GHC < 7.6.

-----

## 1.0.0

- Initial release
