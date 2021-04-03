package com.gildedrose

class GildedRose(val items: Array[Item]) {

  private def incQuality(item: Item): Unit =
    item.quality = Math.min(50, item.quality + 1)

  private def decQuality(item: Item): Unit =
    item.quality = Math.max(0, item.quality - 1)

  private def resetQuality(item: Item): Unit =
    item.quality = 0

  private def decSellIn(item: Item): Unit =
    item.sellIn = item.sellIn - 1

  private def applyIfExpired(item: Item)(f: Item => Unit): Unit =
    if (item.sellIn < 0) f(item)

  def updateQuality() {
    items.
      collect {
        case item: Item if item.name.equals("Sulfuras, Hand of Ragnaros") =>
          ()

        case item: Item if (item.name.equals("Aged Brie")) =>
          incQuality(item)
          decSellIn(item)
          applyIfExpired(item)(incQuality)

        case item: Item if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) =>
          incQuality(item)
          if (item.sellIn < 11) {
            incQuality(item)
          }
          if (item.sellIn < 6) {
            incQuality(item)
          }
          decSellIn(item)
          applyIfExpired(item)(resetQuality)

        case item: Item =>
          decQuality(item)
          decSellIn(item)
          applyIfExpired(item)(decQuality)
      }
  }

}