
package org.sboles.brew.brewlib

import java.text.DecimalFormat

/**
 * Provides hop calculators
 *
 * @author sboles
 */
object Hops {

  /**
   * Decimal format that makes sense for hop calculations: #.##
   */
  val decimalFormat = new DecimalFormat("#.##")

  /**
   * Format a double value
   * @param v Value to format
   * @return Formatted string
   */
  def decimalFormat(v: Double): String = {
    decimalFormat.format(v)
  }

  /**
   * Converts a hop measurement in AAU to an equivalent measurement in
   * ounces.
   * @param aau Hop AAU measurement
   * @param aa Hop AA measurement
   * @return Ounces of hops
   */
  def aauToOuncesAA(aau: Double, aa: Double): Double = {
    if ( aa > 0 ) aau / aa else 0.0
  }

  /**
   * Converts a hop measurement in AAU to an equivalent measurement in
   * ounces.
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param aau Hop AAU measurement
   * @param aa Hop AA measurement
   * @return Ounces of hops
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def aauToOuncesAA(aau: String, aa: String): Double = {
    val dAAU: Double = try {
      aau.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat AAU value as a double")
    }
    val dAA: Double = try {
      aa.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat AA value as a double")
    }
    aauToOuncesAA(dAAU, dAA)
  }
}
