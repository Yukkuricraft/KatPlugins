package net.katsstuff.bukkit.homesweethome;

import io.papermc.paper.plugin.bootstrap.BootstrapContext;
import io.papermc.paper.plugin.bootstrap.PluginBootstrap;
import io.papermc.paper.plugin.bootstrap.PluginProviderContext;
import io.papermc.paper.plugin.provider.util.ProviderUtil;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("UnstableApiUsage")
public class HomeBootstrap implements PluginBootstrap {
    @Override
    public void bootstrap(@NotNull BootstrapContext context) {
    }

    @Override
    public @NotNull JavaPlugin createPlugin(@NotNull PluginProviderContext context) {
        var mainClassStr = context.getConfiguration().getMainClass();
        try {
            var mainClass = Class.forName(mainClassStr + "$");
            return (JavaPlugin) mainClass.getField("MODULE$").get(null);
        } catch (ClassNotFoundException e) {
            return ProviderUtil.loadClass(context.getConfiguration().getMainClass(), JavaPlugin.class, this.getClass().getClassLoader());
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
}
