
package org.sboles.brew.brewlib

import java.text.DecimalFormat

/**
 * Provides temperature calculations
 *
 * @author sboles
 */
object Temperature {

  /**
   * Decimal format for temperature values: ###.#
   */
  val decimalFormat = new DecimalFormat("###.#")

  /**
   * Convert degrees Celsius to degrees Fahrenheit
   * @param dC Degrees Celsius
   * @return Degrees Fahrenheit
   */
  def celsiusToFahrenheit(dC: Double): Double = {
    1.8 * dC + 32.0
  }

  /**
   * Convert degrees Celsius to degrees Fahrenheit
   * @param dC Degrees Celsius
   * @return Degrees Fahrenheit
   */
  def celsiusToFahrenheit(dC: Int): Double = {
    celsiusToFahrenheit(dC.toDouble)
  }

  /**
   * Convert degrees Celsius to degrees Fahrenheit
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param dC Degrees Celsius
   * @return Degrees Fahrenheit
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def celsiusToFahrenheit(dC: String): Double = {
    try {
      celsiusToFahrenheit(dC.toDouble)
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat Celsius temperature as a double")
    }
  }

  /**
   * Convert degrees Fahrenheit to degrees Celsius
   * @param dF Degrees Fahrenheit
   * @return Degrees Celsius
   */
  def fahrenheitToCelsius(dF: Double): Double = {
    (5.0 / 9.0) * (dF - 32.0)
  }

  /**
   * Convert degrees Fahrenheit to degrees Celsius
   * @param dF Degrees Fahrenheit
   * @return Degrees Celsius
   */
  def fahrenheitToCelsius(dF: Int): Double = {
    fahrenheitToCelsius(dF.toDouble)
  }

  /**
   * Convert degrees Fahrenheit to degrees Celsius
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param dF Degrees Fahrenheit
   * @return Degrees Celsius
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def fahrenheitToCelsius(dF: String): Double = {
    try {
      fahrenheitToCelsius(dF.toDouble)
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat Fahrenheit temperature as a double")
    }
  }
}
