import sbtbuildinfo.{BuildInfoOption, BuildInfoResult, JavaStaticFieldsRenderer, TypeExpression}

class FallibleJavaStaticFieldsRenderer(options: Seq[BuildInfoOption], pkg: String, cl: String)
    extends JavaStaticFieldsRenderer(options: Seq[BuildInfoOption], pkg: String, cl: String) {

  override def renderKeys(buildInfoResults: Seq[BuildInfoResult]): Seq[String] =
    header ++
      buildInfoResults.flatMap(line) ++
      Seq(toStringLines(buildInfoResults)) ++
      toMapLines(buildInfoResults) ++
      Seq(buildUrlLines) ++
      buildJsonLines ++
      toJsonLines ++
      footer

  override protected def getJavaType(typeExpr: TypeExpression): Option[String] = {
    def tpeToReturnType(tpe: TypeExpression): Option[String] =
      tpe match {
        case TypeExpression("Any", Nil)               => Some("Object")
        case TypeExpression("Short", Nil)             => Some("Short")
        case TypeExpression("Int", Nil)               => Some("Integer")
        case TypeExpression("Long", Nil)              => Some("Long")
        case TypeExpression("Double", Nil)            => Some("Double")
        case TypeExpression("Float", Nil)             => Some("Float")
        case TypeExpression("Boolean", Nil)           => Some("Boolean")
        case TypeExpression("scala.Symbol", Nil)      => Some("String")
        case TypeExpression("java.lang.String", Nil)  => Some("String")
        case TypeExpression("java.net.URL", Nil)      => Some("java.net.URL")
        case TypeExpression("sbt.URL", Nil)           => Some("java.net.URL")
        case TypeExpression("java.io.File", Nil)      => Some("java.io.File")
        case TypeExpression("sbt.File", Nil)          => Some("java.io.File")
        case TypeExpression("scala.xml.NodeSeq", Nil) => None

        case TypeExpression("sbt.ModuleID", Nil) => Some("String")
        case TypeExpression("sbt.Resolver", Nil) => Some("String")

        case TypeExpression("sbt.librarymanagement.ModuleID", Nil) => Some("String")
        case TypeExpression("sbt.librarymanagement.Resolver", Nil) => Some("String")

        case TypeExpression("sbt.internal.util.Attributed", Seq(TypeExpression("java.io.File", Nil))) =>
          Some("java.io.File")

        case TypeExpression("scala.Option", Seq(arg)) =>
          tpeToReturnType(arg) map { x => s"java.util.Optional<$x>" }
        case TypeExpression("scala.collection.Seq" | "scala.collection.immutable.Seq", Seq(arg)) =>
          tpeToReturnType(arg) map { x => s"java.util.Collection<$x>" }
        case TypeExpression("scala.collection.immutable.Map", Seq(arg0, arg1)) =>
          for {
            x0 <- tpeToReturnType(arg0)
            x1 <- tpeToReturnType(arg1)
          } yield s"java.util.Map<$x0, $x1>"
        case TypeExpression("scala.Tuple2", Seq(arg0, arg1)) =>
          for {
            x0 <- tpeToReturnType(arg0)
            x1 <- tpeToReturnType(arg1)
          } yield s"java.util.Map.Entry<$x0, $x1>"

        case TypeExpression("java.time.LocalDate", Nil) => Some("java.time.LocalDate")
        case TypeExpression("java.time.Instant", Nil)   => Some("java.time.Instant")

        case _ => None
      }
    tpeToReturnType(typeExpr)
  }

  override protected def quote(v: Any): String = v match {
    case (k, _v)                => s"java.util.Map.entry(${quote(k)}, ${quote(_v)})"
    case mp: Map[_, _]          => s"java.util.Map.ofEntries(${mp.map(quote).mkString(", ")})"
    case seq: collection.Seq[_] => s"java.util.List.of(${seq.map(quote).mkString(", ")})"
    case _                      => super.quote(v)
  }
}
