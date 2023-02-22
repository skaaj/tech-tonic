package net.skaaj

import io.circe._
import io.circe.parser._
import io.circe.optics.JsonPath._

object CirceExample extends App {
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
  // $.store.book[*].author
  println(root.store.book.each.author.json.getAll(doc))
  // $..author
  println(doc.findAllByKey("author"))
  // $.store.*
  println(root.store.each.json.getAll(doc))
  // $.store..price
  println(root.store.json.getOption(doc).fold(List.empty[Json])(_.findAllByKey("price")))
  // $..book[2]
  println(doc.findAllByKey("book").flatMap { book =>
    val cursor = book.hcursor
    cursor.downN(2).as[Json].toOption
  })
}
