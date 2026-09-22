# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A multi-project sbt build of Paper (Minecraft 1.21) server plugins written in Scala 3. `katLib` and `katLibDb` are shared libraries; `homeSweetHome`, `magicalWarps`, `rider`, `feelingsRelay` and `cmdCallback` are plugins built on them.

The repo is managed with jj (Jujutsu) as well as git. Keep changes small and focused.

## Commands

```sh
sbt compile                                   # everything
sbt magicalWarps/compile                      # one project
sbt test                                      # all tests
sbt katLibDb/test                             # one project's tests
sbt "magicalWarps/testOnly *PostgresWarpStorageTest"            # one suite
sbt "magicalWarps/testOnly *PostgresWarpStorageTest -- *name*"  # tests in a suite matching a munit glob
sbt magicalWarps/package                      # plugin jar, with katLib/katLibDb jars embedded
sbt magicalWarps/debugBuild                   # package and copy into ../Server/plugins/
scalafmt                                      # format with the scalafmt CLI (no sbt plugin; config in .scalafmt.conf, max 120 columns)
```

The Postgres tests use Testcontainers (`postgres:17-alpine`), so Docker must be running. Test suites run one at a time in a forked JVM, because MockBukkit's server is a global singleton.

## Build and plugin loading

- `project/PaperPlugin.scala` is an sbt AutoPlugin that every plugin project enables. Plugin dependencies are declared with `paperDep(...)`: they are `Provided` at build time and are not shaded. Instead, their coordinates are written into a generated Java `BuildInfo` class (rendered by `project/FallibleJavaStaticFieldsRenderer.scala`), and Paper downloads them at runtime. The loader only knows Maven Central and Sonatype snapshots, so a new runtime dependency must be available there.
- `katLib` and `katLibDb` are not published. They are embedded as jars inside each plugin jar through `jarInJarFiles`, and extracted to `<dataFolder>/libraries/` at load time. The overridden `packageBin` also strips `.tasty` files.
- Every plugin has a copy of `ScalaPluginLoader.java` (reads `BuildInfo` and builds the classpath) and `ScalaPluginBootstrap.java` (returns the Scala `object`'s `MODULE$` as the plugin instance if the main class is an object). They are wired up in `paper-plugin.yml`. The copies are identical apart from the package, so change them all together.
- Transitive dependencies Paper already has (snakeyaml, paper-api, and cats/circe for the library projects) are excluded through `libraryExclusions` in `build.sbt`.

## Architecture

- **`ScalaPlugin`** (`katLib`) is the base class of every plugin. It provides `given ScalaPlugin`, an slf4j `logger`, `serverThreadExecutionContext` for running on the main thread, and `addDisableAction`. Disable actions run newest first on disable, so resources are torn down before what they depend on. Plugins register their resources there instead of overriding `onDisable`.
- **`ScalaDbPlugin`** (`katLibDb`) mixes in a cats-effect `Dispatcher[IO]`, created in `runKatLibRepeatableSetup` and closed as a disable action. DB code is written in `IO` with skunk/dataprism, then exposed to plugin code as `Db[Future, Codec]` via `dispatcher.unsafeToFuture`.
- **Commands**: `katLib` has two frameworks. `command/` is the older combinator-based one, which can register through Bukkit, the command map or Brigadier (`CommandRegistrationType`). `brigcommands/` is a Brigadier-native one. Plugin commands live in each plugin's `cmd/` package.
- **Storage** (homeSweetHome, magicalWarps): a storage trait (`HomeStorage`, `WarpStorage`) with file-based (JSON) and Postgres implementations, chosen from config. The Postgres implementations cache data in memory with `PostgresCached`, which builds on `CachedRemoteData`. It refreshes periodically and applies changes from Postgres `LISTEN/NOTIFY` messages (JSON with an operation and a payload), so several servers can share one database. A notification it can't parse triggers a full refresh.
- **Database migrations**: `DbUpdates.updateIfNeeded(presentDbVersion, schema)` applies `src/main/resources/db-updates/<n>.sql` in order, in one transaction under an advisory lock, and tracks the version in a `version` table. To change a schema, add the next numbered file and bump `presentDbVersion` both where the plugin calls `updateIfNeeded` and in the plugin's test suite (`HomeSuite`, `WarpsSuite`). Statements in a file are split on `;`, and `;;` escapes a literal semicolon. `PostgresSchema` validates schema names and builds the connection parameters.
- **Cross-server teleports**: `Teleporter` (`katLib/util`) with `BungeeChannel` for moving players between servers through the proxy, and `CrossServerPostgresTeleporter` for handing over a pending teleport through Postgres, so it happens when the player arrives on the other server.
- **Config**: homeSweetHome and magicalWarps have a `version` field in `config.yml`. When a user's config is outdated, the plugin saves the current default as `config-v<N>.yml` next to it and asks the user to update. Keep `config-v<N>.yml` in resources in sync with `config.yml`. Configs are decoded with circe-yaml, often with `perspective-derivation` higher-kinded data (types ending in `K`, like `HomeK` and `WarpK`).
- Each plugin keeps its mutable state (config, storage or home handler, teleporter) in a `*Context` class (`HSHContext`, `WarpsContext`). `setup(ignoreOneTime = false)` runs on enable and also registers commands. A reload runs `onDisable()` and then `setup(ignoreOneTime = true)`, which replaces what the context holds. Commands must therefore read state through the context, and not capture storage or the teleporter directly.

## Gotchas

- Warp and home names are case-sensitive. Don't lowercase them for lookups.
- Postgres stores timestamps in microseconds. Truncate `Instant`s with `truncatedTo(ChronoUnit.MICROS)` before storing them, or the value in memory won't equal the value read back.
- A plugin that uses `BungeeChannel` must register the `"BungeeCord"` incoming and outgoing plugin channels.
- Plugin metadata and permissions are in `paper-plugin.yml`, not `plugin.yml`.

## Tests

munit, MockBukkit and Testcontainers. Test helpers are shared across projects through `test->test` dependencies:
- `katLib` `testing/`: `BukkitSuite` gives each test a fresh mocked server, plus plugin creation helpers, `LogCapture` and `RecordingPlayerMock`.
- `katLibDb` `testing/`: `TestPostgres` shares one container per JVM and gives each test its own database (`freshDatabase`). `DbSuite` adds `connect`, which fails the test if pool sessions are still in use when it ends, and `closeAfterTest`.
- Plugin test suites (`HomeSuite`, `WarpsSuite`) build on these. To test cross-server behaviour, start two plugin instances on the same database and wait with `eventually` for notifications to arrive.
- Paper only lets a plugin class loader construct a plugin, so tests create plugins through a `TestX extends XPlugin` subclass (loaded by `TestPluginClassLoader`) and use the instance only through its superclass type.
