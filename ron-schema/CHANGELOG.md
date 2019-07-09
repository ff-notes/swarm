# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)
and this project adheres to
[Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
### Added
- Schema language: `alias` declaration.

### Changed
- Renamed `Boole` -> `Bool` due to tradition.
- `ORSet` don't generate `ObjectORSet` wrapper since `ORSet` now manages
  objects too.
- Now `ObjectState` keeps a typed reference to an object with state frame
  attached,
  and `Object` is just a type UUID --
  a typed reference to an object in a state frame passed in
  `MonadObjectState` context.
  Object is now passed as an explicit argument.

### Removed
- Concept of view type. Maybe will return later.

## [0.6] - 2019-04-25
### Added
- Schema language: `RGA` type.

## [0.5] - 2019-02-04
### Added
- `RON.UUID.liftName` function to create name UUIDs in compile time.
- `RON.Util.ByteStringL` type.
- `RON.Error` module with unified pretty errors.
- Organize `Replicated`, `ReplicatedAsPayload`, and `ReplicatedAsObject` in
  class hierarchy.
- Add `ORSet.removeValue` and `removeRef` implementation.
- Op "patterns" and patterns.

### Removed
- Type alias `ObjectId` since objects are identified by UUID.

### Changed
- Extracted `ron-storage` package.
- Extracted `ron-schema` package.
- Extracted `ron-rdt` package.
- Switched from `Either String a` to `MonadError String m => m a` in failable
  procedures.
- `ORSet.addRef` now adds item's frame, too.
- `ORSet.addNewRef` now returns the reference to the freshly created object.
- Change `StateFrame` key to UUID since objects are identified by UUID.
- Renamed `RawOp` to `ClosedOp` according to the fresh spec.

### Fixed
- Error handling in Boole decoder.

## [0.4] - 2019-01-09
### Added
- Schema `enum` declaration.
- `docIdFromUuid`.
- `OnDocumentChanged` is called each time when any document is changed.

### Changed
- Made GHC 8.6 default.

### Removed
- Schema embedded DSL helpers: `atomInteger`, `atomString`, `boole`, `char`,
  `field`, `option`, `orSet`, `rgaString`, `structLww`, `versionVector`.

### Fixed
- `RGA.edit` bug with re-adding deleted items (#39).

## [0.3] - 2018-12-05
### Added
- Encode/decode EpochTime.
- EDN-based schema DSL.

### Removed
- `RON.Storage.createVersion` from public API.
- `NFData` instances.

## [0.2] - 2018-11-20
### Added
- Schema boole type.
- RON.Storage and submodules are moved from ff project.
- RON.Schema is now re-exported via RON.Schema.TH.

### Changed
- Renamed UUID field "schema" to "version", according to changes in the
  specification.
- RGA: sequential UUIDs on initialization.
- Optimized `Base64.isLetter`.
- Extend `UUID.mkName` to accept any monad.
- Renamed `MonadStorage` methods `list...` -> `get...`
- Renamed `RON.Storage.saveDocument` -> `createDocument`

### Removed
- `RON.Storage.uuidToFileName` as it has no sense as an abstraction
- `RON.Storage.IO.runStorageT` with `StorageT`

## [0.1] - 2018-11-08
### Added
- Package `ron`
  - RON-text format
  - RON-binary format
  - RON-RDT:
    - LWW
    - RGA
    - OR-Set
    - VersionVector
  - RON-Schema
  - RON-Schema TemplateHaskell code generator

[Unreleased]: https://github.com/ff-notes/ff/compare/ron-schema-0.6...HEAD
[0.6]: https://github.com/ff-notes/ff/compare/v0.5...ron-schema-0.6
[0.5]: https://github.com/ff-notes/ff/compare/v0.4...v0.5
[0.4]: https://github.com/ff-notes/ff/compare/v0.3...v0.4
[0.3]: https://github.com/ff-notes/ff/compare/v0.2...v0.3
[0.2]: https://github.com/ff-notes/ff/compare/v0.1...v0.2
[0.1]: https://github.com/ff-notes/ff/tree/v0.1
