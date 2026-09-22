package net.katsstuff.bukkit.katlib.testing

import java.io.File
import java.nio.file.Files
import java.util.logging.Logger

import io.papermc.paper.plugin.configuration.PluginMeta
import io.papermc.paper.plugin.provider.classloader.{ConfiguredPluginClassLoader, PluginClassLoaderGroup}
import org.bukkit.Bukkit
import org.bukkit.plugin.PluginDescriptionFile
import org.bukkit.plugin.java.JavaPlugin

/**
  * Paper only lets a plugin be constructed by a plugin class loader. This
  * loader defines just the plugin class itself, and leaves everything else to
  * the normal class loader, so the plugin can be used as any of its
  * superclasses from tests.
  */
class TestPluginClassLoader(pluginClassName: String, name: String, parent: ClassLoader)
    extends ClassLoader(parent),
      ConfiguredPluginClassLoader:

  private val description = new PluginDescriptionFile(name, "1.0.0-TEST", pluginClassName)
  val dataFolder: File    = Files.createTempDirectory(s"$name-test").toFile

  private var plugin: JavaPlugin = null

  override def loadClass(className: String, resolve: Boolean): Class[?] =
    if className == pluginClassName then
      getClassLoadingLock(className).synchronized {
        val loaded = Option(findLoadedClass(className)).getOrElse {
          val path  = className.replace('.', '/') + ".class"
          val bytes = parent.getResourceAsStream(path).readAllBytes()
          defineClass(className, bytes, 0, bytes.length)
        }
        if resolve then resolveClass(loaded)
        loaded
      }
    else super.loadClass(className, resolve)

  override def loadClass(className: String, resolve: Boolean, checkGlobal: Boolean, checkLibraries: Boolean): Class[?] =
    loadClass(className, resolve)

  override def getConfiguration: PluginMeta = description

  override def init(plugin: JavaPlugin): Unit =
    this.plugin = plugin
    plugin.init(
      Bukkit.getServer,
      description,
      dataFolder,
      new File(dataFolder, s"$name.jar"),
      this,
      description,
      Logger.getLogger(name)
    )

  override def getPlugin: JavaPlugin = plugin

  override def getGroup: PluginClassLoaderGroup = null

  override def close(): Unit = ()

object TestPluginClassLoader:

  /**
    * Creates and enables a plugin. Bukkit must be mocked first.
    * @tparam P
    *   What the plugin is used as. This must be a superclass of pluginClass,
    *   as pluginClass is loaded again in its own class loader.
    * @param pluginClass
    *   A class with a no argument constructor. As it is loaded again, it
    *   should be a small class made for the test.
    */
  def createPlugin[P <: JavaPlugin](name: String, pluginClass: Class[?]): P =
    val loader = new TestPluginClassLoader(pluginClass.getName, name, pluginClass.getClassLoader)
    val plugin = loader.loadClass(pluginClass.getName).getDeclaredConstructor().newInstance().asInstanceOf[P]
    plugin.setEnabled(true)
    plugin
