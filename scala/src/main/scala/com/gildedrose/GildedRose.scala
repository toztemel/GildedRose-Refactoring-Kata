package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def updateQuality() {
    items.
      collect {
        case item: Item if item.name.equals("Sulfuras, Hand of Ragnaros") =>
          ()

        case item: Item if (item.name.equals("Aged Brie")) =>
          item.incQuality
          item.decSellIn
          item.applyIfExpired(_.incQuality)

        case item: Item if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) =>
          item.incQuality
          if (item.sellIn < 11) {
            item.incQuality
          }
          if (item.sellIn < 6) {
            item.incQuality
          }
          item.decSellIn
          item.applyIfExpired(_.resetQuality)

        case item: Item if (item.name.equals("Conjured")) =>
          item.decQuality
          item.decQuality
          item.decSellIn
          item.applyIfExpired(_.decQuality)
          item.applyIfExpired(_.decQuality)

        case item: Item =>
          item.decQuality
          item.decSellIn
          item.applyIfExpired(_.decQuality)
      }
  }

  implicit class ItemOps(item: Item) {
    val MaxQuality = 50
    val MinQuality = 0

    def incQuality(): Unit =
      item.quality = Math.min(MaxQuality, item.quality + 1)

    def decQuality(): Unit =
      item.quality = Math.max(MinQuality, item.quality - 1)

    def resetQuality(): Unit =
      item.quality = MinQuality

    def decSellIn(): Unit =
      item.sellIn = item.sellIn - 1

    def applyIfExpired(f: Item => Unit): Unit =
      if (item.sellIn < 0) f(item)

  }

}