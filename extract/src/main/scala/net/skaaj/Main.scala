package net.skaaj

import io.circe._
import io.circe.parser._
import net.skaaj.CirceUtils._

object Main extends App {
  val jsonStr = """
    {
      "store": {
        "book": [
          { "category": "reference",
            "author": "Nigel Rees",
            "title": "Sayings of the Century",
            "price": 8.95
          },
          { "category": "fiction",
            "author": "Evelyn Waugh",
            "title": "Sword of Honour",
            "price": 12.99
          },
          { "category": "fiction",
            "author": "Herman Melville",
            "title": "Moby Dick",
            "isbn": "0-553-21311-3",
            "price": 8.99
          },
          { "category": "fiction",
            "author": "J. R. R. Tolkien",
            "title": "The Lord of the Rings",
            "isbn": "0-395-19395-8",
            "price": 22.99
          }
        ],
        "bicycle": {
          "color": "red",
          "price": 19.95
        }
      }
    }
  """

  val doc = parse(jsonStr).getOrElse(Json.Null)
  val result = for {
    price <- doc.fetchRequired[Double]("$.store.bicycle.price")
    color <- doc.fetch[String]("$.store.bicycle.color")
    authors <- doc.fetchAll[String]("$.store.book[*].author")
  } yield (price, color, authors)

  println(s"> $result")
  // > Right((19.95,Some(red),List(Nigel Rees, Evelyn Waugh, Herman Melville, J. R. R. Tolkien)))
}
