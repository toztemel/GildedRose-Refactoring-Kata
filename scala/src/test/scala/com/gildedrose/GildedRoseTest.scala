package com.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseTest extends AnyFlatSpec with Matchers {

  behavior of "Degrading of sellIn and quality"

  it should "not update the name" in {
    val items = Array[Item](new Item("foo", 0, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal("foo")
  }

  it should "degrade normal item by 1" in {
    val item = new Item("foo", 1, 1)
    val app = new GildedRose(Array[Item](item))
    app.updateQuality()
    app.items(0).sellIn should equal(0)
    app.items(0).quality should equal(0)
  }

  it should "never drop quality below 0" in {
    val item = new Item("foo", 1, 1)
    val expiredItem = new Item("foo", 0, 1)
    val app = new GildedRose(Array[Item](item, expiredItem))
    app.updateQuality()
    app.updateQuality()
    app.items(0).sellIn should equal(-1)
    app.items(0).quality should equal(0)
    app.items(1).sellIn should equal(-2)
    app.items(0).quality should equal(0)
  }

  behavior of "When sell date is passed"

  it should "degrade quality twice fast " in {
    val item = new Item("foo", 1, 11)
    val agedBrie = new Item("Aged Brie", 1, 9)
    val sulfuras = new Item("Sulfuras, Hand of Ragnaros", 1, 80)
    val backstage = new Item("Backstage passes to a TAFKAL80ETC concert", 1, 7)
    val conjured = new Item("Conjured", 0, 10)

    val app = new GildedRose(Array[Item](item, agedBrie, sulfuras, backstage))
    app.updateQuality()

    app.items(0).sellIn should equal(0)
    app.items(1).sellIn should equal(0)
    app.items(2).sellIn should equal(1)
    app.items(3).sellIn should equal(0)

    app.items(0).quality should equal(10)
    app.items(1).quality should equal(10)
    app.items(2).quality should equal(80)
    app.items(3).quality should equal(10)

    app.updateQuality()

    app.items(0).sellIn should equal(-1)
    app.items(1).sellIn should equal(-1)
    app.items(2).sellIn should equal(1)
    app.items(3).sellIn should equal(-1)

    app.items(0).quality should equal(8)
    app.items(1).quality should equal(12)
    app.items(2).quality should equal(80)
    app.items(3).quality should equal(0)

    app.updateQuality()

    app.items(0).sellIn should equal(-2)
    app.items(1).sellIn should equal(-2)
    app.items(2).sellIn should equal(1)
    app.items(3).sellIn should equal(-2)

    app.items(0).quality should equal(6)
    app.items(1).quality should equal(14)
    app.items(2).quality should equal(80)
    app.items(3).quality should equal(0)
  }

  behavior of "Aged Brie"

  it should "increase quality by 1" in {
    val item = new Item("Aged Brie", 1, 1)
    val app = new GildedRose(Array[Item](item))
    app.updateQuality()
    app.items(0).sellIn should equal(0)
    app.items(0).quality should equal(2)
  }

  it should "quality is never more than 50" in {
    val item = new Item("Aged Brie", 1, 49)
    val app = new GildedRose(Array[Item](item))

    app.updateQuality()
    app.items(0).sellIn should equal(0)
    app.items(0).quality should equal(50)

    app.updateQuality()
    app.items(0).sellIn should equal(-1)
    app.items(0).quality should equal(50)
  }

  behavior of "Sulfuras"

  it should "never to be sold or decrease in Quality " in {
    val item = new Item("Sulfuras, Hand of Ragnaros", 1, 49)
    val app = new GildedRose(Array[Item](item))
    app.updateQuality()
    app.items(0).sellIn should equal(1)
    app.items(0).quality should equal(49)

    app.updateQuality()
    app.items(0).sellIn should equal(1)
    app.items(0).quality should equal(49)
  }

  it should "always keep quality as 80" in {
    val item = new Item("Sulfuras, Hand of Ragnaros", 1, 80)
    val app = new GildedRose(Array[Item](item))
    app.updateQuality()
    app.items(0).sellIn should equal(1)
    app.items(0).quality should equal(80)

    app.updateQuality()
    app.items(0).sellIn should equal(1)
    app.items(0).quality should equal(80)
  }

  behavior of "Backstage passes"

  it should "increase quality by 1 when there are than 11 days or more" in {
    val item = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 10)
    val app = new GildedRose(Array[Item](item))
    app.updateQuality()
    app.items(0).sellIn should equal(10)
    app.items(0).quality should equal(11)
  }

  it should "quality never exceeds 50" in {
    val item = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 50)
    val item2 = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)
    val item3 = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 48)
    val app = new GildedRose(Array[Item](item, item2, item3))
    app.updateQuality()
    app.items(0).sellIn should equal(10)
    app.items(1).sellIn should equal(9)
    app.items(2).sellIn should equal(4)
    app.items(0).quality should equal(50)
    app.items(1).quality should equal(50)
    app.items(2).quality should equal(50)
  }

  it should "increase quality by 2 when there are 10 days or less" in {
    val item = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10)
    val item2 = new Item("Backstage passes to a TAFKAL80ETC concert", 6, 10)
    val app = new GildedRose(Array[Item](item, item2))
    app.updateQuality()
    app.items(0).sellIn should equal(9)
    app.items(1).sellIn should equal(5)
    app.items(0).quality should equal(12)
    app.items(1).quality should equal(12)
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

  it should "drop quality to 0 after the concert" in {
    val item = new Item("Backstage passes to a TAFKAL80ETC concert", 1, 10)
    val app = new GildedRose(Array[Item](item))
    app.updateQuality()
    app.items(0).sellIn should equal(0)
    app.items(0).quality should equal(13)

    app.updateQuality()
    app.items(0).sellIn should equal(-1)
    app.items(0).quality should equal(0)

  }

  behavior of "Conjured"

  ignore should "degrade in quality by 2" in {
    val item = new Item("Conjured", 1, 49)
    val app = new GildedRose(Array[Item](item))

    app.updateQuality()
    app.items(0).sellIn should equal(0)
    app.items(0).quality should equal(47)

  }


}