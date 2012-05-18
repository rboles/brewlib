
package org.sboles.brew.brewlib

import java.text.DecimalFormat

/**
 * Provides gravity calculators
 *
 * @author sboles
 */
object Gravity {

  /**
   * Decimal format for gravity values: #.###
   */
  val decimalFormat = new DecimalFormat("#.###")


  /**
   * Decimal format for ABV values: ##.##
   */
  val abvDecimalFormat = new DecimalFormat("##.##")

  /**
   * Uses an equation commonly attributed to Ray Daniels to calculate
   * alcohol by volume.
   * @param og Original gravity
   * @param fg Final gravity
   * @return ABV
   */
  def abvDaniels(og: Double, fg: Double): Double = {
    (76.08 * (og - fg) / (1.775 - og)) * (fg / 0.794)
  }

  /**
   * Uses an equation commonly attributed to Ray Daniels to calculate
   * alcohol by volume. Adjusts the original and final gravities to
   * 60 degrees Fahrenheit and then calculates ABV
   * @param og Original gravity
   * @param fg Final gravity
   * @param ot Fahrenheit temperature of original gravity measurement
   * @param ft Fahrenheit temperature of final gravity measurement
   * @return ABV
   */
  def abvDaniels(og: Double, fg: Double, ot: Double, ft: Double): Double = {
    abvDaniels(tempAdjustFahrenheit(og, ot),
               tempAdjustFahrenheit(fg, ft))
  }

  /**
   * Uses an equation commonly attributed to Ray Daniels to calculate
   * alcohol by volume. Adjusts the original and final gravities to
   * 60 degrees Fahrenheit and then calculates ABV.
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param og Original gravity
   * @param fg Final gravity
   * @param ot Fahrenheit temperature of original gravity measurement
   * @param ft Fahrenheit temperature of final gravity measurement
   * @return ABV
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def abvDaniels(og: String, fg: String, ot: String, ft: String): Double = {
    val _og = try {
      og.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat original gravity value as a double")
    }
    val _fg = try {
      fg.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat final gravity value as a double")
    }
    val _ot = try {
      ot.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat temperature of initial gravity sample as a double")
    }
    val _ft = try {
      ft.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat temperature of final gravity sample as a double")
    }

    abvDaniels(tempAdjustFahrenheit(_og, _ot),
               tempAdjustFahrenheit(_fg, _ft))
  }

  /**
   * Uses an equation commonly attributed to Charlie Papazian to calculate
   * alcohol by volume.
   * @param og Original gravity
   * @param fg Final gravity
   * @return ABV
   */
  def abvPapazian(og: Double, fg: Double): Double = {
    (og - fg) * 131.25
  }

  /**
   * Uses an equation commonly attributed to Charlie Papazian to calculate
   * alcohol by volume. Adjusts the original and final gravities to 60
   * degrees Fahrenheit and then calculates ABV
   * @param og Original gravity
   * @param fg Final gravity
   * @param ot Fahrenheit temperature of original gravity measurement
   * @param ft Fahrenheit temperature of final gravity measurement
   * @return ABV
   */
  def abvPapazian(og: Double, fg: Double, ot: Double, ft: Double): Double = {
    abvPapazian(tempAdjustFahrenheit(og, ot),
                tempAdjustFahrenheit(fg, ft))
  }

  /**
   * Uses an equation commonly attributed to Charlie Papazian to calculate
   * alcohol by volume.
   * @param og Original gravity
   * @param fg Final gravity
   * @param constant A value in the neighborhood of 131
   * @return ABV
   */
  def abvPapazian(og: Double, fg: Double, constant: Double): Double = {
    (og - fg) * constant
  }

  /**
   * Uses an equation commonly attributed to Charlie Papazian to calculate
   * alcohol by volume. Adjusts the original and final gravities to
   * 60 degrees Fahrenheit and then calculates ABV.
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param og Original gravity
   * @param fg Final gravity
   * @param ot Fahrenheit temperature of original gravity measurement
   * @param ft Fahrenheit temperature of final gravity measurement
   * @return ABV
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def abvPapazian(og: String, fg: String, ot: String, ft: String): Double = {
    val _og = try {
      og.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat original gravity value as a double")
    }
    val _fg = try {
      fg.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat final gravity value as a double")
    }
    val _ot = try {
      ot.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat temperature of initial gravity sample as a double")
    }
    val _ft = try {
      ft.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat temperature of final gravity sample as a double")
    }

    abvPapazian(tempAdjustFahrenheit(_og, _ot),
               tempAdjustFahrenheit(_fg, _ft))
  }

  /**
   * Adjust gravity measured at a temperature to 60 degrees Fahrenheit
   * @param gravity Gravity
   * @param temp Temperature of gravity reading
   * @return Adjusted gravity
   */
  def tempAdjustFahrenheit(gravity: Double, temp: Double): Double = {
    gravity + ((temp - 60) / 10 * 0.003)
  }

  /**
   * Adjust gravity measured at a temperature to 60 degrees Fahrenheit
   * @param gravity Gravity
   * @param temp Temperature of gravity reading
   * @param constant A value in the neighborhood of 0.003
   * @return Adjusted gravity
   */
  def tempAdjustFahrenheit(gravity: Double, temp: Double, constant: Double): Double = {
    gravity + ((temp - 60) / 10 * constant)
  }

  /**
   * Adjust gravity measured at a temperature to 60 degrees Fahrenheit
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param g Gravity
   * @param t Temperature of gravity reading
   * @return Adjusted gravity
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def tempAdjustFahrenheit(g: String, t: String): Double = {
    val gravity = try {
      g.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat gravity as a double")
    }
    val temp = try {
      t.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat temperature as a double")
    }
    tempAdjustFahrenheit(gravity, temp)
  }

  /**
   * Adjust gravity measured at a temperature (degrees Celsius) to 60
   * degrees Fahrenheit
   * @param gravity Gravity
   * @param temp Temperature of gravity reading in Celsius
   * @return Adjusted gravity
   */
  def tempAdjustCelsius(gravity: Double, temp: Double): Double = {
    tempAdjustFahrenheit(gravity, Temperature.celsiusToFahrenheit(temp))
  }

  /**
   * Adjust gravity measured at a temperature to 60 degrees Celsius
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param g Gravity
   * @param t Temperature of gravity reading in degrees Celsius
   * @return Adjusted gravity
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def tempAdjustCelsius(g: String, t: String): Double = {
    val gravity = try {
      g.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat gravity as a double")
    }
    val temp = try {
      t.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat temperature as a double")
    }
    tempAdjustCelsius(gravity, temp)
  }

  /**
   * Convert specific gravity to degrees plato
   * @param specific Specific gravity
   * @return Degrees Plato
   */
  def specificToPlato(specific: Double): Double = {
    if ( specific > 0 ) {
      (specific - 1) * 1000 / 4
    } else {
      0.0
    }
  }

  /**
   * Convert specific gravity to degrees plato
   *
   * Throws IllegalArgumentException on failure to convert argument to
   * Double. The exception message specifies the problem argument.
   * @param specific Specific gravity
   * @return Degrees Plato
   * @throws IllegalArgumentException On failure to convert argument
   * to Double
   */
  @throws(classOf[IllegalArgumentException])
  def specificToPlato(specific: String): Double = {
    val dSpecific: Double = try {
      specific.toDouble
    } catch {
      case _ => throw new IllegalArgumentException(
        "Failed to treat specific gravity value as a double")
    }
    specificToPlato(dSpecific)
  }
}
