package me.ukl

object QTables {
  
  val baseLuminance = List(
      16,  11,  10,  16,  24,  40,  51,  61,
      12,  12,  14,  19,  26,  58,  60,  55,
      14,  13,  16,  24,  40,  57,  69,  56,
      14,  17,  22,  29,  51,  87,  80,  62,
      18,  22,  37,  56,  68, 109, 103,  77,
      24,  35,  55,  64,  81, 104, 113,  92,
      49,  64,  78,  87, 103, 121, 120, 101,
      72,  92,  95,  98, 112, 100, 103,  99
  )
  
  val baseChrominance = List(
        17,  18,  24,  47,  99,  99,  99,  99,
        18,  21,  26,  66,  99,  99,  99,  99,
        24,  26,  56,  99,  99,  99,  99,  99,
        47,  66,  99,  99,  99,  99,  99,  99,
        99,  99,  99,  99,  99,  99,  99,  99,
        99,  99,  99,  99,  99,  99,  99,  99,
        99,  99,  99,  99,  99,  99,  99,  99,
        99,  99,  99,  99,  99,  99,  99,  99
  )
  
  /**
   * Scales a base table to the specified quality
   * @param quality specifies a quality from 0-100 where 50 returns the base table
   */
  def getTable(baseTable: List[Int], quality: Int) = {
    val clampedQ = Math.max(Math.min(100, quality), 1);
    
    val scaleFactor =
      if (clampedQ < 50)
        5000 / clampedQ
      else
        200 - clampedQ * 2
    
    (baseTable.map { x => Math.max(Math.min(255, (x * scaleFactor + 50) / 100), 1) }).toArray
  }
  
  /**
   * Scales a base table to the specified quality
   * @param quality specifies a quality from 0.01 - 1.0 where 0.5 returns the base table
   */
  def getTable(baseTable: List[Int], quality: Double) = {
    val clampedQ = Math.max(Math.min(1.0, quality), 0.005);
    
    val scaleFactor =
      if (clampedQ < 0.5)
        0.5 / clampedQ
      else
        2.0 - clampedQ * 2.0
    
    (baseTable.map { x => Math.max(Math.min(255, Math.round(x * scaleFactor).toInt), 1) }).toArray
  }
  
  def getLum(quality: Double) = getTable(baseLuminance, quality)
  
  def getChrom(quality: Double) = getTable(baseChrominance, quality)
  
}