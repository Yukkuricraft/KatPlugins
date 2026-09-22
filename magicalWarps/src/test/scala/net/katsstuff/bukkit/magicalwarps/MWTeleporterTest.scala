package net.katsstuff.bukkit.magicalwarps

import net.katsstuff.bukkit.katlib.db.CrossServerPostgresTeleporterSuite
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import net.katsstuff.bukkit.magicalwarps.testing.WarpsSuite

class MWTeleporterTest extends CrossServerPostgresTeleporterSuite, WarpsSuite:
  override def channel: String                  = "magicalwarps_delayed_teleport_change"
  override def migratedDatabase(): TestDatabase = super[WarpsSuite].migratedDatabase()
