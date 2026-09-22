package net.katsstuff.bukkit.homesweethome

import scala.collection.mutable

class NestedMapTest extends munit.FunSuite:

  private def maps: Seq[(String, () => NestedMap[String, String, Int])] = Seq(
    "mutable"    -> (() => NestedMap(mutable.HashMap.empty, () => mutable.HashMap.empty)),
    "concurrent" -> (() => NestedMap.concurrent)
  )

  for (kind, make) <- maps do
    test(s"$kind: put and get") {
      val m = make()
      m.put("a", "x", 1)
      m.put("a", "y", 2)
      m.put("b", "x", 3)
      assertEquals(m.get("a", "x"), Some(1))
      assertEquals(m.get("a", "z"), None)
      assertEquals(m.get("c", "x"), None)
      assertEquals(m("b", "x"), 3)
      assertEquals(m.getAll("a"), Map("x" -> 1, "y" -> 2))
      assertEquals(m.toNormalMap, Map("a" -> Map("x" -> 1, "y" -> 2), "b" -> Map("x" -> 3)))
      assertEquals(m.iterator.toSet, Set(("a", "x", 1), ("a", "y", 2), ("b", "x", 3)))
    }

    test(s"$kind: reading doesn't create inner maps") {
      val m = make()
      m.getOrElse("a", "x", 0)
      m.getAll("a")
      m.remove("a", "x")
      assert(!m.containsOuter("a"))
    }

    test(s"$kind: makeInnerIfNotExists and removeInner") {
      val m = make()
      m.makeInnerIfNotExists("a")
      assert(m.containsOuter("a"))
      assert(!m.contains("a", "x"))
      m.put("a", "x", 1)
      m.makeInnerIfNotExists("a")
      assertEquals(m.get("a", "x"), Some(1))
      m.removeInner("a")
      assert(!m.containsOuter("a"))
    }

    test(s"$kind: updateWith") {
      val m = make()
      m.updateWith("a", "x")(v => Some(v.getOrElse(0) + 1))
      m.updateWith("a", "x")(v => Some(v.getOrElse(0) + 1))
      assertEquals(m.get("a", "x"), Some(2))
      m.updateWith("a", "x")(_ => None)
      assertEquals(m.get("a", "x"), None)
    }

    test(s"$kind: getOrElseUpdate, remove, ++= and --=") {
      val m = make()
      assertEquals(m.getOrElseUpdate("a", "x", 1), 1)
      assertEquals(m.getOrElseUpdate("a", "x", 2), 1)
      m ++= Seq(("a", "y", 2), ("b", "z", 3))
      assertEquals(m.remove("a", "x"), Some(1))
      m --= Seq(("b", "z"))
      assertEquals(m.toNormalMap, Map("a" -> Map("y" -> 2), "b" -> Map.empty[String, Int]))
      m.clear()
      assertEquals(m.toNormalMap, Map.empty[String, Map[String, Int]])
    }

  test("concurrent: updates from many threads are not lost") {
    val m = NestedMap.concurrent[String, String, Set[Int]]
    val threads = (0 until 8).map { t =>
      new Thread(() => (0 until 200).foreach(i => m.updateWith(s"owner${i % 3}", "home")(s => Some(s.getOrElse(Set.empty) + (t * 200 + i)))))
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    assertEquals(m.iterator.map(_._3).flatten.toSet, (0 until 1600).toSet)
  }
