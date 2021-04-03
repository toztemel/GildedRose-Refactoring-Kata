package com.gildedrose

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseTest extends AnyFlatSpec with Matchers with BeforeAndAfter {

  val normal = new Item("normal", 10, 10)
  val agedBrie = new Item("Aged Brie", 10, 10)
  val legendary = new Item("Sulfuras, Hand of Ragnaros", 10, 80)
  val backstage = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10)
  val conjured = new Item("Conjured", 10, 10)

  val items: Array[Item] = Array(normal, agedBrie, legendary, backstage, conjured)

  val app = new GildedRose(items)

  before {
    items.foreach(_.sellIn = 10)
    items.collect {
      case i: Item if i.name == "Sulfuras, Hand of Ragnaros" => i.quality = 80
      case i: Item => i.quality = 10
    }
  }

  private def resetSellIn(items: Array[Item]) =
    items.foreach(_.sellIn = 0)

  behavior of "Degrading of sellIn and quality of normal item"

  it should "not update the name" in {
    app.updateQuality()
    normal.name should equal("normal")
  }

  it should "degrade normal item sellIn by 1" in {
    app.updateQuality()
    normal.sellIn should equal(9)
    app.updateQuality()
    normal.sellIn should equal(8)
  }

  it should "degrade normal item quality by 1" in {
    app.updateQuality()
    normal.quality should equal(9)
    app.updateQuality()
    normal.quality should equal(8)
  }

  it should "degrade normal item quality until 0" in {
    normal.quality = 0
    app.updateQuality()
    app.items(0).quality should equal(0)
  }

  behavior of "When sell date is passed"

  it should "degrade quality twice fast " in {
    resetSellIn(items)

    app.updateQuality()

    normal.quality should equal(8)
    //    conjured.quality should equal(6)
  }

  behavior of "Aged Brie"

  it should "increase quality by 1" in {

    app.updateQuality()

    agedBrie.quality should equal(11)
  }

  it should "quality is never more than 50" in {
    agedBrie.quality = 50

    app.updateQuality()
    agedBrie.quality should equal(50)
  }

  behavior of "Sulfuras"

  it should "never to be sold or decrease in Quality " in {
    app.updateQuality()

    legendary.sellIn should equal(10)
    legendary.quality should equal(80)
  }

  behavior of "Backstage passes"

  it should "increase quality by 1 when there are than 11 days or more" in {
    backstage.sellIn = 11
    app.updateQuality()

    backstage.quality should equal(11)
  }

  it should "increase quality by 2 when there are 10 days or less" in {
    app.updateQuality()

    backstage.quality should equal(12)
  }

  it should "increase quality by 3 when there are 5 days or less" in {
    val item = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10)
    val item2 = new Item("Backstage passes to a TAFKAL80ETC concert", 1, 10)
    val app = new GildedRose(Array[Item](item, item2))
    app.updateQuality()
    app.items(0).sellIn should equal(4)
    app.items(1).sellIn should equal(0)
    app.items(0).quality should equal(13)
    app.items(1).quality should equal(13)
  }

  it should "quality never exceeds 50" in {
    backstage.sellIn = 11
    backstage.quality = 50
    app.updateQuality()

    backstage.quality should equal(50)

    backstage.sellIn = 10
    backstage.quality = 49
    app.updateQuality()

    backstage.quality should equal(50)

    backstage.sellIn = 5
    backstage.quality = 48
    app.updateQuality()

    backstage.quality should equal(50)
  }

  it should "drop quality to 0 after the concert" in {
    backstage.sellIn = 1
    app.updateQuality()

    backstage.quality should equal(13)

    app.updateQuality()
    backstage.quality should equal(0)
  }

  behavior of "Conjured"

  it should "degrade in quality by 2" in {
    app.updateQuality()

    conjured.quality should equal(8)
  }


}