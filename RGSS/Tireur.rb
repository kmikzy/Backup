# Raho, Kmikzy, S4suk3 et Fabien : Le tireur
#==============================================================================
# ** Tireur
#------------------------------------------------------------------------------
#  Librairie de fonctionnalités statiques
#==============================================================================

module Tireur
  extend self
  #--------------------------------------------------------------------------
  # * Retourne un angle selon 2 coordonnées
  #--------------------------------------------------------------------------
  def get_angle(sx, sy, cx, cy, prec = false)
    ref = (prec) ? 57.3 : 180.0/Math::PI
    return (Math.atan2(cy-sy, cx-sx)*ref) + 90.0
  end
  #--------------------------------------------------------------------------
  # * Retourne une fonction pour tracer une distance
  #--------------------------------------------------------------------------
  def line_equation(sx, sy, cx, cy)
    a = (sy-cy)/(sx-cx).to_f
    b = (sy - sx*a).to_f
    fun = lambda{|ap, bp, x|ap*x + bp}
    return fun.curry.(a, b)
  end
  #--------------------------------------------------------------------------
  # * Retourne une fonction de parabole (Merci a Fabien !)
  #  [x,y]
  #--------------------------------------------------------------------------
  def parabole(sx, sy, cx, cy, force)
    distance = Math.hypot(sx-cx, sy-cy)
    vitesse_verticale = (-(distance**0.5)/(0.5+force))/2
    temps = distance/10.0 #Approximation
    coeff_a = -2.0 * vitesse_verticale / temps
    vx = (cx-sx)/temps
    vy = (cy-sy)/temps
    fun = lambda do |vx, vy, sx, sy, coeff_a, t|
      [vx*t+sx, (coeff_a*t**2)/2+vy+t+vitesse_verticale*t+cy]
    end
    return fun.curry.(vx, vy, sx, sy, coeff_a)
  end
end
