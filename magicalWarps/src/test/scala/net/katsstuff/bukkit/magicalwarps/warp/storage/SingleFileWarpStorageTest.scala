package net.katsstuff.bukkit.magicalwarps.warp.storage

import java.nio.file.Files

import net.katsstuff.bukkit.magicalwarps.testing.WarpsSuite
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import net.katsstuff.bukkit.magicalwarps.{WarpsConfig, WarpsPlugin}

class SingleFileWarpStorageTest extends WarpsSuite:

  private val path = Files.createTempDirectory("warps").resolve("storage.json")

  private def storage(): SingleFileWarpStorage =
    given WarpsPlugin = createWarpsPlugin()
    given WarpsConfig = warpsConfig()
    val s             = new SingleFileWarpStorage(path)
    await(s.reloadWarps())
    s

  override def beforeEach(context: BeforeEach): Unit =
    super.beforeEach(context)
    Files.deleteIfExists(path)

  test("warps are saved and loaded again") {
    val s     = storage()
    val warps = Seq(fullWarp("spawn"), warp("Hub"))
    warps.foreach(w => result(s.setWarp(w)))
    assertEquals(storage().allWarps, warps.map(w => w.name -> w).toMap)
  }

  test("renaming a warp moves it, keeping everything else") {
    val s     = storage()
    val spawn = fullWarp("spawn")
    result(s.setWarp(spawn))
    result(s.setWarp(warp("other")))

    result(s.renameWarp(spawn, "hub"))
    val expected = Map("hub" -> (spawn.copy(name = "hub"): Warp), "other" -> warp("other"))
    assertEquals(s.allWarps.keySet, expected.keySet)
    assertEquals(s.allWarps("hub"), expected("hub"))
    assertEquals(storage().allWarps.keySet, expected.keySet)
  }

  test("warp names keep their case") {
    val s = storage()
    result(s.setWarp(warp("Spawn")))
    assertEquals(s.getWarp("Spawn").map(_.name), Some("Spawn"))
    assertEquals(s.getWarp("spawn"), None)
    assertEquals(storage().allWarps.keySet, Set("Spawn"))
  }

  test("warps can be removed") {
    val s = storage()
    result(s.setWarp(warp("spawn")))
    result(s.removeWarp("spawn"))
    assertEquals(storage().allWarps, Map.empty[String, Warp])
  }

  test("importing replaces all warps") {
    val s = storage()
    result(s.setWarp(warp("old")))
    result(s.importData(Seq(warp("new"))))
    assertEquals(storage().allWarps.keySet, Set("new"))
  }

  test("warps exported to a file can be imported again, replacing what was there") {
    val s     = storage()
    val warps = Seq(fullWarp("spawn"), warp("Hub"))
    warps.foreach(w => result(s.setWarp(w)))
    result(s.exportStorageData())

    result(s.removeWarp("spawn"))
    result(s.setWarp(warp("added later")))
    result(s.importStorageData())

    val expected = warps.map(w => w.name -> w).toMap
    assertEquals(s.allWarps, expected)
    assertEquals(storage().allWarps, expected)
  }

  test("importing a missing file fails, and changes nothing") {
    val s = storage()
    result(s.setWarp(warp("spawn")))
    Files.deleteIfExists(s.exportImportPath)

    intercept[Exception](result(s.importStorageData()))
    assertEquals(storage().allWarps.keySet, Set("spawn"))
  }

