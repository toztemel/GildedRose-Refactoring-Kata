package com.gildedrose

class GildedRose(val items: Array[Item]) {

  private val itemToUpdate: Map[String, Update] = Map(
    "Sulfuras, Hand of Ragnaros" -> Legendary,
    "Aged Brie" -> AgedBrie,
    "Backstage passes to a TAFKAL80ETC concert" -> Backstage,
    "Conjured" -> Conjured
  ).withDefaultValue(Normal)

  def updateQuality(): Unit =
    items.
      foreach(item => itemToUpdate(item.name)(item))

  implicit class ItemOps(item: Item) {
    val MaxQuality = 50
    val MinQuality = 0

    def incQuality(): Unit =
      item.quality = Math.min(MaxQuality, item.quality + 1)

    def decQuality(amount: Int = 1): Unit =
      item.quality = Math.max(MinQuality, item.quality - amount)

    def decSellIn(): Unit =
      item.sellIn = item.sellIn - 1

    def applyIfExpired(f: Item => Unit): Unit =
      if (item.sellIn < 0) f(item)

  }

  trait Update {

    protected def updateQuality(item: Item): Unit

    protected def onExpiry(item: Item): Unit

    def apply(item: Item): Unit = {
      updateQuality(item)
      updateSellIn(item)
      item.applyIfExpired(item => onExpiry(item))
    }

    private def updateSellIn(item: Item): Unit = {
      item.decSellIn()
    }
  }

  case object Normal extends Update {
    override def updateQuality(item: Item): Unit = item.decQuality()

    override def onExpiry(item: Item): Unit = item.decQuality()
  }

  case object Conjured extends Update {
    override def updateQuality(item: Item): Unit = item.decQuality(2)

    override def onExpiry(item: Item): Unit = item.decQuality(2)
  }

  case object AgedBrie extends Update {
    override def updateQuality(item: Item): Unit = item.incQuality()

    override def onExpiry(item: Item): Unit = item.incQuality()
  }

  case object Legendary extends Update {
    override def updateQuality(item: Item): Unit = ()

    override def onExpiry(item: Item): Unit = ()

    override def apply(item: Item): Unit = ()
  }

  case object Backstage extends Update {
    override def updateQuality(item: Item): Unit = {
      item.incQuality()
      if (item.sellIn < 11) {
        item.incQuality()
      }
      if (item.sellIn < 6) {
        item.incQuality()
      }
    }

    override def onExpiry(item: Item): Unit =
      item.decQuality(item.quality)

  }

}