ThisBuild / scalaVersion := "3.5.1"
ThisBuild / organization := "net.katsstuff.bukkit"
ThisBuild / crossPaths   := false

ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots")
ThisBuild / resolvers += "papermc" at "https://papermc.io/repo/repository/maven-public"
ThisBuild / resolvers += "DynMap" at "https://repo.mikeprimm.com/"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "utf-8",
    "-feature",
    "-unchecked"
    // "-Ycheck-init",
    // "-Xlint",
    // "-Wdead-code",
    // "-Wunused"
  )
)

lazy val libraryExclusions = Seq(
  ExclusionRule("org.yaml", "snakeyaml"),
  ExclusionRule("io.papermc.paper", "paper-api"),
  ExclusionRule("org.spigotmc", "spigot-api"),
  ExclusionRule("org.typelevel", "cats-core_3"),
  ExclusionRule("io.circe", "circe-core_3"),
  ExclusionRule("io.circe", "circe-parser_3")
)

lazy val katLibDeps = Seq(
  "org.typelevel" %% "cats-core"              % "2.12.0",
  "info.debatty"   % "java-string-similarity" % "2.0.0"
)

lazy val katLib = project
  .settings(
    commonSettings,
    version                                  := "4.0.0-SNAPSHOT",
    libraryDependencies += "io.papermc.paper" % "paper-api" % "1.21-R0.1-SNAPSHOT",
    libraryDependencies ++= katLibDeps
  )

lazy val katLibDbDeps = Seq(
  "net.katsstuff" %% "dataprism-skunk" % "0.1.0",
  "org.tpolecat"  %% "skunk-circe"     % "0.6.3",
  "io.circe"      %% "circe-core"      % "0.14.5",
  "io.circe"      %% "circe-parser"    % "0.14.5"
)

lazy val katLibDb = project
  .dependsOn(katLib)
  .settings(
    commonSettings,
    version := "4.0.0-SNAPSHOT",
    libraryDependencies ++= katLibDbDeps
  )

lazy val homeSweetHome = project
  .dependsOn(katLib % Provided, katLibDb % Provided)
  .enablePlugins(PaperPlugin)
  .settings(
    commonSettings,
    version := "3.3.0",
    jarInJarFiles := Map(
      "Katlib.jar"   -> (katLib / Compile / packageBin).value,
      "KatlibDb.jar" -> (katLibDb / Compile / packageBin).value
    ),
    resolvers += "jitpack.io" at "https://jitpack.io", // Vault
    libraryDependencies += "io.papermc.paper"    % "paper-api" % "1.21-R0.1-SNAPSHOT" % Provided,
    libraryDependencies += "com.github.MilkBowl" % "VaultAPI"  % "1.7"                % Provided,
    libraryDependencies += paperDep("io.circe" %% "circe-yaml" % "0.15.0-RC1" exclude ("org.yaml", "snakeyaml")),
    libraryDependencies += paperDep("net.katsstuff" %% "perspective-derivation" % "0.3.0"),
    libraryDependencies ++= (katLibDeps ++ katLibDbDeps).map(paperDep),
    buildInfoPackage := "net.katsstuff.bukkit.homesweethome"
  )

lazy val magicalWarps = project
  .dependsOn(katLib % Provided, katLibDb % Provided)
  .enablePlugins(PaperPlugin)
  .settings(
    commonSettings,
    version := "2.2.0",
    jarInJarFiles := Map(
      "Katlib.jar"   -> (katLib / Compile / packageBin).value,
      "KatlibDb.jar" -> (katLibDb / Compile / packageBin).value
    ),
    resolvers += "jitpack.io" at "https://jitpack.io", // Vault
    resolvers += "DynMap" at "https://repo.mikeprimm.com/",
    libraryDependencies += "io.papermc.paper"    % "paper-api"  % "1.21-R0.1-SNAPSHOT" % Provided,
    libraryDependencies += "com.github.MilkBowl" % "VaultAPI"   % "1.7"                % Provided,
    libraryDependencies += "us.dynmap"           % "dynmap-api" % "3.4-beta-3"         % Provided,
    libraryDependencies += paperDep("io.circe" %% "circe-yaml" % "0.15.0-RC1" exclude ("org.yaml", "snakeyaml")),
    libraryDependencies += paperDep("net.katsstuff" %% "perspective-derivation" % "0.3.0"),
    libraryDependencies ++= (katLibDeps ++ katLibDbDeps).map(paperDep),
    buildInfoPackage := "net.katsstuff.bukkit.magicalwarps"
  )

lazy val rider = project
  .dependsOn(katLib % Provided)
  .enablePlugins(PaperPlugin)
  .settings(
    commonSettings,
    version := "1.1.0",
    jarInJarFiles := Map(
      "Katlib.jar" -> (katLib / Compile / packageBin).value
    ),
    libraryDependencies += "io.papermc.paper" % "paper-api" % "1.21-R0.1-SNAPSHOT" % Provided,
    libraryDependencies ++= katLibDeps.map(paperDep),
    buildInfoPackage := "net.katsstuff.bukkit.rider"
  )

lazy val feelingsRelay = project
  .dependsOn(katLib % Provided)
  .enablePlugins(PaperPlugin)
  .settings(
    commonSettings,
    version := "1.1.3",
    jarInJarFiles := Map(
      "Katlib.jar" -> (katLib / Compile / packageBin).value
    ),
    resolvers += "Scarsz-Nexus" at "https://nexus.scarsz.me/content/groups/public",
    resolvers += "m2-dv8tion" at "https://m2.dv8tion.net/releases",
    libraryDependencies += "io.papermc.paper" % "paper-api"  % "1.21-R0.1-SNAPSHOT" % Provided,
    libraryDependencies += "com.discordsrv"   % "discordsrv" % "1.28.0"             % Provided,
    libraryDependencies += paperDep("io.circe" %% "circe-core" % "0.14.5"),
    libraryDependencies += paperDep("io.circe" %% "circe-parser" % "0.14.5"),
    libraryDependencies += paperDep("io.circe" %% "circe-yaml" % "0.15.0-RC1" exclude ("org.yaml", "snakeyaml")),
    libraryDependencies += paperDep("net.katsstuff" %% "perspective-derivation" % "0.3.0"),
    libraryDependencies ++= katLibDeps.map(paperDep),
    buildInfoPackage := "net.katsstuff.bukkit.feelingsrelay"
  )
