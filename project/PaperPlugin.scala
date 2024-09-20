import sbt.*
import sbt.Keys.*
import sbt.plugins.JvmPlugin
import sbtbuildinfo.BuildInfoPlugin

object PaperPlugin extends AutoPlugin {

  object autoImport {
    def paperDep(dep: ModuleID): ModuleID =
      dep % Provided withIsTransitive true withExtraAttributes Map("PaperProvided" -> "true")

    lazy val debugBuild =
      TaskKey[Unit]("debugBuild", "Copies a built assembly jar to the folder run/plugins for easy debugging")

    lazy val jarInJarFiles = TaskKey[Map[String, File]]("paperJarInJarFiles", "Jar files to include in this jar")
  }
  import BuildInfoPlugin.autoImport.*
  import autoImport.*

  override def requires: Plugins = JvmPlugin && BuildInfoPlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.map[Seq[ModuleID], scala.collection.immutable.Seq[Map[String, Any]]](libraryDependencies) {
        case (k, vs) =>
          import sbt.librarymanagement.*
          def handleCross(c: CrossVersion): Map[String, String] = c match {
            case Disabled => Map()
            case c: Binary =>
              Map(
                "type"   -> "binary",
                "prefix" -> c.prefix,
                "suffix" -> c.suffix
              )
            case c: Constant => Map("type" -> "constant", "value" -> c.value)
            case c: Patch    => Map("type" -> "patch")
            case c: Full =>
              Map(
                "type"   -> "full",
                "prefix" -> c.prefix,
                "suffix" -> c.suffix
              )

            case c: For3Use2_13 =>
              Map(
                "type"   -> "for3use2_13",
                "prefix" -> c.prefix,
                "suffix" -> c.suffix
              )
            case c: For2_13Use3 =>
              Map(
                "type"   -> "for2_13use3",
                "prefix" -> c.prefix,
                "suffix" -> c.suffix
              )
          }

          k -> vs
            .filter(v => v.name == "scala3-library" || v.extraAttributes.get("PaperProvided").contains("true"))
            .map { v =>
              Map(
                "organization"   -> v.organization,
                "name"           -> v.name,
                "revision"       -> v.revision,
                "crossVersion"   -> handleCross(v.crossVersion),
                "configurations" -> "",
                "exclusions" -> v.exclusions.map { v =>
                  Map(
                    "organization"   -> v.organization,
                    "name"           -> v.name,
                    "artifact"       -> v.artifact,
                    "configurations" -> v.configurations.map(_.name),
                    "crossVersion"   -> handleCross(v.crossVersion)
                  )
                }
              ) ++ v.extraAttributes.filter(_._1 != "PaperProvided")
            }
            .toList
      },
      scalaVersion,
      BuildInfoKey.map[Map[String, File], scala.collection.immutable.Seq[String]](jarInJarFiles) { case (_, map) =>
        ("jarInJarFiles", map.keys.toList)
      }
    ),
    jarInJarFiles := Map.empty,
    Compile / packageBin := {
      import java.nio.file.Files
      import java.util.jar.{JarEntry, JarInputStream, JarOutputStream}

      val packageFile = (Compile / packageBin).value
      val tempFile    = Files.createTempFile(packageFile.toPath.getParent, packageFile.name, "")

      val includedJarFiles = jarInJarFiles.value

      scala.util.Using.resources(
        new JarInputStream(Files.newInputStream(packageFile.toPath)),
        new JarOutputStream(Files.newOutputStream(tempFile))
      ) { (istream, ostream) =>
        Stream
          .continually(istream.getNextJarEntry)
          .takeWhile(_ != null)
          .filterNot(_.getName.endsWith(".tasty"))
          .foreach { iJarEntry =>
            ostream.putNextEntry(new JarEntry(iJarEntry))
            IO.transfer(istream, ostream)
          }

        includedJarFiles.foreach { case (name, file) =>
          val jarEntry = new JarEntry(name)
          jarEntry.setTime(file.lastModified())
          jarEntry.setSize(Files.size(file.toPath))
          ostream.putNextEntry(jarEntry)
          IO.transfer(file, ostream)
        }
      }

      IO.move(tempFile.toFile, packageFile)
      packageFile
    },
    buildInfoRenderFactory := { (a, b, c) => new FallibleJavaStaticFieldsRenderer(a, b, c) },
    debugBuild := {
      val fatJar = (Compile / packageBin).value
      IO.copyFile(fatJar, file("../Server/plugins/" + fatJar.getName))
    }
  )
}
