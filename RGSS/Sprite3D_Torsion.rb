class Sprite3D_Torsion
  def initialize(bitmap, x = 320, y = 208, vp = nil)
    setup_viewport(vp)
    setup_bitmap(bitmap)
    setup_coords(x, y)
    setup_tables
    explode_bitmap
    create_sprites_table
  end
  def setup_viewport(vp)
    @viewport = vp
  end
  def setup_bitmap(bitmap)
    @bitmap = bitmap if bitmap.is_a?(Bitmap)
    @bitmap ||= Bitmap.new(bitmap)
  end
  def setup_coords(x, y)
    @width = @bitmap.width
    @height = @bitmap.height
    @x, @y = x, y
  end
  def setup_tables
    @sprites = Array.new
    @bitmaps = Array.new
  end
  def create_sprites_table
    @bitmaps.each.with_index do |value, key|
      temp = Sprite.new(@viewport)
      temp.ox = @width/2
      temp.y = (-@height/2) + @y + key
      temp.x = @x
      temp.bitmap = value
      @sprites << temp
    end
  end
  def explode_bitmap
    (0...@height).each do |iterator|
      rect = create_rect(iterator)
      temp_bmp = create_inline_bitmap(rect)
      @bitmaps << temp_bmp
    end
  end
  def create_rect(iterator)
    Rect.new(0, iterator, @width, 1)
  end
  def create_inline_bitmap(rect)
    temp = Bitmap.new(@width, 1)
    temp.blt(0, 0, @bitmap, rect)
    temp
  end
end
