package net.katsstuff.bukkit.feelingsrelay

import scala.compiletime.*
import scala.deriving.*

import io.circe.*
import perspective.*
import perspective.derivation.*

trait CapitalizeDefaultsDecoder[A] extends Decoder[A]
object CapitalizeDefaultsDecoder:

  given derived[A](
      using gen: HKDProductGeneric[A],
      decoders: gen.Gen[Decoder],
      defaults: GetDefault.Aux[A, gen.Gen]
  ): CapitalizeDefaultsDecoder[A] = new CapitalizeDefaultsDecoder[A]:
    override def apply(cursor: HCursor): Either[DecodingFailure, A] =
      import gen.given

      gen.names
        .tupledK(decoders)
        .map2K(defaults.defaults)(
          [Z] =>
            (nameAndDecoder: (gen.Names, Decoder[Z]), default: Option[Z]) => {
              val (name, decoder) = nameAndDecoder
              cursor
                .get[Option[Z]](name.capitalize)(using Decoder.decodeOption(using decoder)) match
                case Right(Some(a)) => Right(a)
                case Right(None) => default.toRight(cursor.get(name.capitalize)(using decoder)).left.flatMap(identity)
                case l @ Left(_) => l.asInstanceOf[Decoder.Result[Z]]
          }
        )
        .sequenceIdK
        .map(gen.from)

  trait GetDefault[A]:
    type Out[F[_]]
    def defaults: Out[Option]

  object GetDefault:
    type Aux[A, Out0[F[_]]] = GetDefault[A] {
      type Out[F[_]] = Out0[F]
    }

    inline given mkDefault[A](
        using m: Mirror.ProductOf[A],
        s: ValueOf[Tuple.Size[m.MirroredElemTypes]]
    ): GetDefault.Aux[A, [F[_]] =>> ProductK[F, m.MirroredElemTypes]] =
      val default = getDefaults[A](s.value).asInstanceOf[ProductK[Option, m.MirroredElemTypes]]
      new DerivedGetDefault(default)

    class DerivedGetDefault[A, ElemTypes <: Tuple](default: ProductK[Option, ElemTypes])
        extends GetDefault[A]:
      type Out[F[_]] = ProductK[F, ElemTypes]
      override def defaults: Out[Option] = default
    end DerivedGetDefault

    private inline def getDefaults[A](size: Int): Tuple = ${ getDefaultsImpl[A]('size) }

    import scala.quoted.*
    // https://stackoverflow.com/a/68422070
    private def getDefaultsImpl[A](size: Expr[Int])(using quotes: Quotes, aTpe: Type[A]): Expr[Tuple] =
      import quotes.reflect.*
      val sizeN = size.valueOrAbort

      val terms: List[Option[Term]] =
        (1 to sizeN).toList.map { i =>
          TypeRepr
            .of[A]
            .typeSymbol
            .companionClass
            .declaredMethod(s"$$lessinit$$greater$$default$$$i")
            .headOption
            .map(Select(Ref(TypeRepr.of[A].typeSymbol.companionModule), _))
        }

      def exprOfOption[T](oet: Option[Expr[T]])(using Type[T], Quotes): Expr[Option[T]] = oet match
        case None     => Expr(None)
        case Some(et) => '{ Some($et) }

      val exprs: List[Option[Expr[Any]]]  = terms.map(_.map(_.asExprOf[Any]))
      val exprs1: List[Expr[Option[Any]]] = exprs.map(exprOfOption)
      Expr.ofTupleFromSeq(exprs1)
