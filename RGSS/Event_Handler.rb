#==============================================================================
# ** Event_Handler
#------------------------------------------------------------------------------
#  Custom Event Handler
#==============================================================================

module Event_Handler
  #--------------------------------------------------------------------------
  # * Singleton
  #--------------------------------------------------------------------------
  class << self
    #--------------------------------------------------------------------------
    # * Public instance variable
    #--------------------------------------------------------------------------
    attr_accessor :std_triggers
    Event_Handler.std_triggers = [
      :hover,
      :click,
      :trigger,
      :release,
      :repeat
    ] 
  end
  #--------------------------------------------------------------------------
  # * Event behaviour
  #--------------------------------------------------------------------------
  module Behaviour
    #--------------------------------------------------------------------------
    # * Setup Event Handler
    #--------------------------------------------------------------------------
    def setup_eHandler
      @__std_triggers = {
        hover:    nil,
        click:    nil,
        trigger:  nil,
        release:  nil, 
        repeat:   nil
      }
      @__cst_triggers = {}
      @table_triggers = {
        hover:    method(:hover?),
        click:    method(:click?),
        trigger:  method(:trigger?),
        release:  method(:release?),
        repeat:   method(:repeat?)
      }
    end
    #--------------------------------------------------------------------------
    # * Unbinding process
    #--------------------------------------------------------------------------
    def unbind(key = nil)
      unless key
        setup_eHandler
        return
      end
      if(Event_Handler.std_triggers.include?(key))
        @__std_triggers[key.to_sym] = nil
        return
      end
      @__cst_triggers[key.to_sym] = nil if @__cst_triggers[key.to_sym]
    end
    #--------------------------------------------------------------------------
    # * Binding event
    #--------------------------------------------------------------------------
    def bind(key, *args, &block)
      if(Event_Handler.std_triggers.include?(key))
        ntriggers = args[0] || -1
        @__std_triggers[key.to_sym] = {
          fun:        block,
          ntriggers:  ntriggers
        }
        return
      end
      trigger = args[0]
      ntriggers = args[1] || -1
      @__cst_triggers[key.to_sym] = {
        trigger:    trigger,
        fun:        block,
        ntriggers:  ntriggers
      }
    end
    #--------------------------------------------------------------------------
    # * Update events
    #--------------------------------------------------------------------------
    def update_eHandler
      @__std_triggers.each do |key, event|
        if event && event[:ntriggers] != 0
          if @table_triggers[key] && @table_triggers[key].()
            event[:fun].(self)
            event[:ntriggers] -= 1 if event[:ntriggers] != -1
          end
        end
      end
      @__cst_triggers.each do |key, event|
        if event
          if event[:ntriggers] != 0
            event[:fun].(self) if event[:trigger].(self)
            event[:ntriggers] -= 1 if event[:ntriggers] != -1
          else
            event = nil
          end
        end
      end
    end
    #--------------------------------------------------------------------------
    # * Hover
    #--------------------------------------------------------------------------
    def hover?
      @rect.hover?
    end
    #--------------------------------------------------------------------------
    # * Click
    #--------------------------------------------------------------------------
    def click?(key = :mouse_left)
      @rect.click?(key)
    end
    #--------------------------------------------------------------------------
    # * Trigger
    #--------------------------------------------------------------------------
    def trigger?(key = :mouse_left)
      @rect.trigger?(key)
    end
    #--------------------------------------------------------------------------
    # * Repeat
    #--------------------------------------------------------------------------
    def repeat?(key = :mouse_left)
      @rect.repeat?(key)
    end
    #--------------------------------------------------------------------------
    # * Release
    #--------------------------------------------------------------------------
    def release?(key = :mouse_left)
      @rect.release?(key)
    end
  end
  #==============================================================================
  # ** API
  #------------------------------------------------------------------------------
  #  Command handling
  #==============================================================================
  module API
    #--------------------------------------------------------------------------
    # * Binding
    #--------------------------------------------------------------------------
    def bind(e, *args, &block)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.each{|ev|ev.bind(*args, &block)}
    end
    #--------------------------------------------------------------------------
    # * UnBinding
    #--------------------------------------------------------------------------
    def unbind(e, k=nil)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.each{|ev|ev.unbind(k)}
    end
    #--------------------------------------------------------------------------
    # * Mouse Hover Event
    #--------------------------------------------------------------------------
    def mouse_hover_event?(e)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.any?{|ev| ev.hover?}
    end
    #--------------------------------------------------------------------------
    # * clicked event
    #--------------------------------------------------------------------------
    def mouse_click_event?(e, k=:mouse_left)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.any?{|ev| ev.click?(k)}
    end
    #--------------------------------------------------------------------------
    # * Triggered event
    #--------------------------------------------------------------------------
    def mouse_trigger_event?(e, k=:mouse_left)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.any?{|ev| ev.trigger?(k)}
    end
    #--------------------------------------------------------------------------
    # * Repeated event
    #--------------------------------------------------------------------------
    def mouse_repeat_event?(e, k=:mouse_left)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.any?{|ev| ev.repeat?(k)}
    end
    #--------------------------------------------------------------------------
    # * Released event
    #--------------------------------------------------------------------------
    def mouse_release_event?(e, k=:mouse_left)
      e = Kernel.events(e) if e.is_a?(Fixnum)
      e.any?{|ev| ev.release?(k)}
    end
    #--------------------------------------------------------------------------
    # * Load Commands
    #--------------------------------------------------------------------------
    append_commands
  end
end


#==============================================================================
# ** Kernel
#------------------------------------------------------------------------------
#  Object class methods are defined in this module. 
#  This ensures compatibility with top-level method redefinition.
#==============================================================================

module Kernel
  #--------------------------------------------------------------------------
  # * Constants
  #--------------------------------------------------------------------------
  RGSSHWND      = Win32API::FindWindow.('RGSS Player', 0)
  USERNAME      = ENV['USERNAME'].dup.to_utf8
  Clipboard     = UI::Clipboard
  EVENT_FORMAT  = Clipboard.get_format("VX Ace EVENT_COMMAND")
  Keyboard      = UI::Keyboard.new
  Mouse         = UI::Mouse.new
  #--------------------------------------------------------------------------
  # * RPG Datas Structure
  #--------------------------------------------------------------------------
  RPGDatas = [
    "Actors", 
    "Classes",
    "Skills",
    "Items",
    "Weapons",
    "Armors",
    "Enemies",
    "States",
    "Animations",
    "Tilesets",
    "CommonEvents",
    "MapInfos"
  ]
  #--------------------------------------------------------------------------
  # * API for command handling
  #--------------------------------------------------------------------------
  def command(name, *args)
    method_name = name.to_sym
    Command.send(method_name, *args)
  end
  #--------------------------------------------------------------------------
  # * Alias
  #--------------------------------------------------------------------------
  alias :cmd  :command
  alias :c    :command
  #--------------------------------------------------------------------------
  # * All selector
  #--------------------------------------------------------------------------
  def all_events
    events(:all_events)
  end
  #--------------------------------------------------------------------------
  # * Selectors
  #--------------------------------------------------------------------------
  def events(*ids, &block)
    return [] unless SceneManager.scene.is_a?(Scene_Map)
    if ids.length == 1 && ids[0] == :all_events
      return $game_map.each_events.values.dup
    end
    result = []
    ids.each{|id| result << $game_map.each_events[id] if $game_map.each_events[id]}
    result += $game_map.each_events.values.select(&block) if block_given?
    result
  end
  #--------------------------------------------------------------------------
  # * Trigger
  #--------------------------------------------------------------------------
  def trigger(&block)
    block
  end
  #--------------------------------------------------------------------------
  # * Alias
  #--------------------------------------------------------------------------
  alias :e        :events
  alias :listener :trigger
  #--------------------------------------------------------------------------
  # * Switch Activation/Desactivation
  #--------------------------------------------------------------------------
  def on;   true;   end
  def off;  false;  end
end

