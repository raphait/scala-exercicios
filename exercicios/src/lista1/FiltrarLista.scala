package lista1

object FiltrarLista extends App {

	def filtra[T](lista: List[T], p: T => Boolean): List[T] = lista.filter(p)
	//Só consegui passar uma Partially Applied Function fazendo esse filtraCurry no estilo curry
	def filtraCurry[T](lista: Traversable[T])( p: T => Boolean) = lista.filter(p)
	
	//notaçãp orientada a objetos de funcção - Vou deixar assim para demosntrar
	//vou tentar escrever todas as formas de escrita de função
	def maioresQue5 = new Function[Int, Boolean] {
		def apply(x: Int): Boolean = x > 5
	}
	
	def todasAsLaranjas = (x: String) => x.contains("laranja");
	
	case class Fruta(val nome: String, val valor: Double, val madura: Boolean){ override def toString(): String = "{"+nome+", "+valor+", "+madura+"}"}

	//Tentativa de refactoring
	class Predicate[T](p: T => Boolean) extends (T => Boolean) {
		def apply(arg0: T) = p(arg0)
	}
	
	//Não sei bem para que o "case" e "object" servem.
	case object Maduras extends Predicate[Fruta](p= _.madura)
	case object Baratas extends Predicate[Fruta](p= _.valor <= 5.0)
	
	
	println( filtra(List(100, 2 ,3, 4, 6, 8, 9, 10), maioresQue5) )
	println( filtra("laranja" :: "laranja-lima" :: "laranja-pêra" :: "pera" :: "uva" :: "maçã" :: Nil, todasAsLaranjas) )
	println( filtraCurry(Range(0, 100)) (_ % 2 == 0)  )

	//Gostaria de passar duas funções, por ex.: Maduras && Baratas
	println( filtra(
			List(
					Fruta("laranja", 10.50, false),
					Fruta("morango", 5.0, true),
					Fruta("pêssego", 3.99, true),
					Fruta("banana", 2.99, false)
			), Maduras)
	)
	
}