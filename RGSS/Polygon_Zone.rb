class Polygon_Zone
  #--------------------------------------------------------------------------
  # * WIN32API
  #--------------------------------------------------------------------------
  @@CreatePolygonRgn = Win32API.new('gdi32','CreatePolygonRgn','pll','l')
  @@PtInRegion = Win32API.new('gdi32','PtInRegion','lll','l')
  @@DeleteObject = Win32API.new('gdi32','DeleteObject','l','l')
  #--------------------------------------------------------------------------
  # * Constantes
  #--------------------------------------------------------------------------
  WINDING = 1
  #--------------------------------------------------------------------------
  # * Constructeur
  #--------------------------------------------------------------------------
  def initialize(points)
    modify_coord(points)
  end
  #--------------------------------------------------------------------------
  # * Destructeur
  #--------------------------------------------------------------------------
  def dispose
    @@DeleteObject.(@region) if @region
    @region = nil
  end
  #--------------------------------------------------------------------------
  # * Modifie les coordonnées
  #--------------------------------------------------------------------------
  def modify_coord(points)
    dispose
    @point_list = points.collect{|point|point.pack('l2')}.join
    @region = @@CreatePolygonRgn.(@point_list, points.size, WINDING)
  end
  #--------------------------------------------------------------------------
  # * Vérifie si un point est dans la zone
  #--------------------------------------------------------------------------
  def in_zone?(x, y)
    x += $game_map.display_x * 32
    y += $game_map.display_y * 32
    @@PtInRegion.(@region, x, y) != 0
  end
end
