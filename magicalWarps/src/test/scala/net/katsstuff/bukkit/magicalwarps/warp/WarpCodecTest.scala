package net.katsstuff.bukkit.magicalwarps.warp

import io.circe.syntax.*
import net.katsstuff.bukkit.magicalwarps.testing.WarpsSuite

class WarpCodecTest extends WarpsSuite:

  test("warps survive being encoded and decoded") {
    for w <- Seq(warp("plain"), fullWarp("full")) do assertEquals(w.asJson.as[Warp], Right(w))
  }

  test("warps are encoded with the column names") {
    val json = fullWarp("full").asJson
    for key <- Seq("name", "world_uuid", "display_name", "allowed_perm_groups", "allowed_users") do
      assert(json.hcursor.downField(key).succeeded, key)
  }
