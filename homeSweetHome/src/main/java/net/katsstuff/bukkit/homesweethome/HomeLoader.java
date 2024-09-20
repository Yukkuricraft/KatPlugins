package net.katsstuff.bukkit.homesweethome;

import io.papermc.paper.plugin.loader.PluginClasspathBuilder;
import io.papermc.paper.plugin.loader.PluginLoader;
import io.papermc.paper.plugin.loader.library.impl.JarLibrary;
import io.papermc.paper.plugin.loader.library.impl.MavenLibraryResolver;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.Exclusion;
import org.eclipse.aether.repository.RemoteRepository;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;

@SuppressWarnings({"UnstableApiUsage", "unchecked", "ConstantValue"})
public class HomeLoader implements PluginLoader {

    private String parseName(String rawName, Map<String, String> map) {
        var scalaVersion = net.katsstuff.bukkit.homesweethome.BuildInfo.scalaVersion;
        var type = map.get("type");
        var prefix = map.get("prefix");
        if (prefix != null && !prefix.isEmpty()) {
            prefix = "_" + prefix;
        } else {
            prefix = "_";
        }
        var suffix = map.get("suffix");
        if (suffix != null && !suffix.isEmpty()) {
            suffix = "_" + suffix;
        } else {
            suffix = "";
        }

        String binaryVersion;
        if (scalaVersion.startsWith("3")) {
            binaryVersion = "3";
        } else {
            var parts = scalaVersion.split("\\.", 2);
            binaryVersion = parts[0] + "_" + parts[1];
        }

        return switch (type) {
            case null -> rawName;
            case "binary" -> rawName + prefix + binaryVersion + suffix;
            case "constant", "patch", "full", "for3use2_13", "for2_13use3" ->
                    throw new IllegalStateException("Unsupported CrossVersion");
            default -> throw new IllegalStateException("Unknown binary version");
        };
    }

    private Exclusion parseExclusion(Map<String, Object> map) {
        var organization = (String) map.get("organization");
        var name = (String) map.get("name");
        var artifact = (String) map.get("artifact");
        var configurations = (List<String>) map.get("configurations");
        var crossVersion = (Map<String, String>) map.get("crossVersion");

        if (!artifact.equals("*")) throw new IllegalStateException("Tried to exclude specific artifact. Not possible");
        if (configurations.size() > 1)
            throw new IllegalStateException("Tried to exclude artifact with multiple configurations. Not possible");

        var configuration = configurations.isEmpty() ? "" : configurations.getFirst();
        return new Exclusion(organization, parseName(name, crossVersion), configuration, "jar");
    }

    private Dependency parseDep(Map<String, Object> map) {
        var organization = (String) map.get("organization");
        var name = (String) map.get("name");
        var revision = (String) map.get("revision");
        var crossVersion = (Map<String, String>) map.get("crossVersion");
        var rawExclusions = (List<Map<String, Object>>) map.get("exclusions");
        var configurations = (String) map.get("configurations");

        var exclusions = rawExclusions.stream().map(this::parseExclusion).toList();

        var artifact = new DefaultArtifact(organization, parseName(name, crossVersion), configurations, "jar", revision);

        return new Dependency(artifact, null).setExclusions(exclusions);
    }

    @Override
    public void classloader(@NotNull PluginClasspathBuilder classpathBuilder) {
        var dataDir = classpathBuilder.getContext().getDataDirectory();
        var pluginSource = classpathBuilder.getContext().getPluginSource();
        var destination = dataDir.resolve("libraries").resolve("Katlib.jar");

        try(var filesystem = FileSystems.newFileSystem(pluginSource)) {
            Files.createDirectories(destination.getParent());
            Files.copy(filesystem.getPath("Katlib.jar"), destination, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        classpathBuilder.addLibrary(new JarLibrary(destination));

        var resolver = new MavenLibraryResolver();
        resolver.addRepository(
                new RemoteRepository.Builder(
                        "central",
                        "default",
                        "https://repo.maven.apache.org/maven2"
                ).build()
        );
        resolver.addRepository(
                new RemoteRepository.Builder(
                        "sonatype-snapshots",
                        "default",
                        "https://oss.sonatype.org/content/repositories/snapshots"
                ).build()
        );

        for (Map<String, Object> libraryDependency : net.katsstuff.bukkit.homesweethome.BuildInfo.libraryDependencies) {
            var dep = parseDep(libraryDependency);
            resolver.addDependency(dep);
        }
        resolver.addDependency(new Dependency(new DefaultArtifact("org.typelevel:cats-core_3:2.9.0"), null));
        resolver.addDependency(new Dependency(new DefaultArtifact("info.debatty:java-string-similarity:2.0.0"), null));
        
        classpathBuilder.addLibrary(resolver);
    }
}
