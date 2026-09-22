package net.katsstuff.bukkit.homesweethome

import net.katsstuff.bukkit.homesweethome.testing.HomeSuite
import net.katsstuff.bukkit.katlib.db.CrossServerPostgresTeleporterSuite
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase

class HSHTeleporterTest extends CrossServerPostgresTeleporterSuite, HomeSuite:
  override def channel: String                     = "homesweethome_delayed_teleport_change"
  override def migratedDatabase(): TestDatabase    = super[HomeSuite].migratedDatabase()
