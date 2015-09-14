## 0.4.1

- Added support for linear-1.20.

## 0.4.0.2

- Relaxed lower-bounds for aeson to support aeson-0.8.

## 0.4.0.1

- Fixed Bézier interpolation.

## 0.4

### Breaking changes

- Removed `splineSampler` from `Spline`. It has to be passed to `spline`
  explicitely now on.

### Non-breaking changes

- Added ToJSON instances for Spline.
- Added ToJSON instances for Key.
- Added the keyInterpolation function to get a Text version of the
  interpolation of a Key.
- `Eq`, `Functor` and `Show` instances for `Spline`.

## 0.3.3.3

- Support for vector-0.11.

## 0.3.3.2

- Fixed typo.

## 0.3.3.1

- Fixed documentation displaying, which was fucked up almost everywhere.

## 0.3.3

- Support for linear-1.19.

## 0.3.2

- FromJSON instances for Spline.
- FromJSON instances for Key.

## 0.3.1

- Fixed sampling implementation.
- Updated documentation.
- Added normalizeSampling.

## 0.3

- Added Bézier interpolation mode.
- Removed CP type.
- Removed Polynomial type.
- Added Key type. It replaces both CP and Polynomial.
- Enhanced user interface with the library.
- Internal files refactoring.

## 0.2.2

- Added cubicHermite.

## 0.2.1

- Added unspline.

## 0.2

- Function 'smooth' has a new name; 'sample'.
- Enhanced internal implementation.
- Fixed some documentation formatting issues.

## 0.1.3

- Support for GHC 7.10.

## 0.1.2

- Support for linear 1.18.*.

## 0.1.1

- Support for linear 1.17.*.

## 0.1.0.1

- Fixed cabal meta information.
- Fixed haddock documentation.

## 0.1

- Initial revision.
