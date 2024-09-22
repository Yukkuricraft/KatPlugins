package net.katsstuff.bukkit.katlib.db

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import net.katsstuff.bukkit.katlib.ScalaPlugin

import scala.compiletime.uninitialized

trait ScalaDbPlugin extends ScalaPlugin:

  private var _dispatcher: Dispatcher[IO] = uninitialized
  def dispatcher: Dispatcher[IO] = _dispatcher

  override protected def runKatLibRepeatableSetup(): Unit = 
    super.runKatLibRepeatableSetup()
    val (dispatcher, closeDispatcher) = Dispatcher.parallel[IO].allocated.unsafeRunSync()(using IORuntime.global)
    this._dispatcher = dispatcher

    addDisableAction(closeDispatcher.unsafeRunSync()(IORuntime.global))
