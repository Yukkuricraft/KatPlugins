package net.katsstuff.bukkit.katlib.testing

import scala.jdk.CollectionConverters.*

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.core.read.ListAppender
import org.slf4j.LoggerFactory

/** Captures everything logged while the test runs. */
trait LogCapture extends munit.FunSuite:

  private var appender: ListAppender[ILoggingEvent] = null

  private def rootLogger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]

  override def beforeEach(context: BeforeEach): Unit =
    super.beforeEach(context)
    appender = new ListAppender[ILoggingEvent]
    appender.start()
    rootLogger.addAppender(appender)

  override def afterEach(context: AfterEach): Unit =
    try
      rootLogger.detachAppender(appender)
      appender.stop()
    finally super.afterEach(context)

  /** The messages logged at the given level so far. */
  def logged(level: Level): Seq[String] =
    appender.list.asScala.toList.filter(_.getLevel == level).map(_.getFormattedMessage)
