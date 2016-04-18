# Utilisation avancÃ©e des objets par Avygeil, Molok, Kmikzy et s4suk3
# http://funkywork.blogspot.com

#==============================================================================
# ** String
#------------------------------------------------------------------------------
#  Ajout d'API pour les conversion d'instructions
#==============================================================================

class String
	#--------------------------------------------------------------------------
	# * Extrait les nombres d'une chaine
	# * Par Avygeil
	#--------------------------------------------------------------------------
	def extract_numbers
		return [] if not self =~ /(-)*\d+/
		return [$&.to_i] + ($').extract_numbers
	end

	#--------------------------------------------------------------------------
	# * Convertit une chaine en instruction
	#--------------------------------------------------------------------------
	def to_instruction
		self.strip!
		self.gsub!(/ /, "")
		instruction_data = self.split("=")
		name = instruction_data[0].to_sym
		pattern = RPG::Instruction.get(name)
		if instruction_data[1] =~ pattern
			integer_list = instruction_data[1].extract_numbers
			integer_list.collect!{|int|int.to_i}
			return RPG::Instruction.new(name, integer_list) 
		end
		return 
	end
	#--------------------------------------------------------------------------
	# * Convertit une chaine en jeu d'instruction
	#--------------------------------------------------------------------------
	def to_instruction_game
		instructions = Array.new
		self.split("\n").each do |line|
			instructions << line.to_instruction
		end
		return instructions.compact
	end
end
#==============================================================================
# ** RPG
#------------------------------------------------------------------------------
#  Ajout de la structure Instruction
#==============================================================================

module RPG

	#==============================================================================
	# ** RPG::Instruction
	#------------------------------------------------------------------------------
	#  Structure d'une instruction
	#==============================================================================

	class Instruction
		#--------------------------------------------------------------------------
    	# * Liste des instructions (extensible)
    	#--------------------------------------------------------------------------
		@@list ||= Array.new
		@@list << [:usable, /\d+/]
		@@list << [:consumable, /\d+/]
		@@list << [:learn, /\d+/]
		@@list << [:learn_always, /\d+/]
		@@list << [:forget, /\d+/]
		@@list << [:craftable, /(((w|a|i)([[:blank:]]*)\d+)+)[[:blank:]]*->[[:blank:]]*((w|a|i)([[:blank:]]*)\d+)/]

		#--------------------------------------------------------------------------
    	# * API d'utilisation des variables de classes
    	#--------------------------------------------------------------------------
    	class << self
	    	#--------------------------------------------------------------------------
	      	# * Retourne les instructions
	      	#--------------------------------------------------------------------------
	      	def instructions
	      		return @@list
	      	end
	      	#--------------------------------------------------------------------------
	      	# * Retourne une instruction en fonction de son nom
	      	#--------------------------------------------------------------------------
	      	def get(name)
		        name = name.to_sym unless name.is_a? Symbol
		        instruction_cell = @@list.find{|instr| instr[0] == name }
		        return instruction_cell[1]
	      	end

      	end
      	#--------------------------------------------------------------------------
    	# * Variables d'instances
    	#--------------------------------------------------------------------------
    	attr_accessor :name, :value
    	#--------------------------------------------------------------------------
    	# * Instanciation d'objet
    	#--------------------------------------------------------------------------
    	def initialize(name, value)
    		@name, @value = name, value
    	end
	end

	#==============================================================================
	# ** RPG::BaseItem
	#------------------------------------------------------------------------------
	#  Rajoute la gestion des instructions
	#==============================================================================

	class BaseItem
		#--------------------------------------------------------------------------
    	# * Variables d'instances
    	#--------------------------------------------------------------------------
		attr_accessor :instructions

		#--------------------------------------------------------------------------
    	# * Convertit les commentaires en instructions potentielles
    	#--------------------------------------------------------------------------
		def set_instructions
			@instructions = @note.to_instruction_game
		end
	end
end

#==============================================================================
# ** DataManager
#------------------------------------------------------------------------------
#  Rajoute la gestion des instructions aux objets
#==============================================================================

module DataManager
	class << self
		#--------------------------------------------------------------------------
	    # * Alias
	    #--------------------------------------------------------------------------
	    alias item_load_normal_database load_normal_database
	    alias item_load_battle_test_database load_battle_test_database

	    #--------------------------------------------------------------------------
	    # * Mappe les instructions
	    #--------------------------------------------------------------------------
	    def map_instructions
	    	$data_items.each{|items| items.set_instructions if items}
	    	$data_weapons.each{|items| items.set_instructions if items}
	    	$data_armors.each{|items| items.set_instructions if items}
	    end

	    #--------------------------------------------------------------------------
	    # * Ajoute la gestion des instructions
	    #--------------------------------------------------------------------------
	    def load_normal_database
	    	item_load_normal_database
	    	map_instructions
	    end

	    #--------------------------------------------------------------------------
	    # * Ajoute la gestion des instructions
	    #--------------------------------------------------------------------------
	    def load_battle_test_database
	    	item_load_battle_test_database
	    	map_instructions
	    end
   	end
end

#==============================================================================
# ** Game_Actor
#------------------------------------------------------------------------------
#  Rajoute la gestion des instructions aux objets
#==============================================================================

class Game_Actor
	#--------------------------------------------------------------------------
    # * alias
    #--------------------------------------------------------------------------
    alias item_usable? usable?

    #--------------------------------------------------------------------------
    # * Permission d'utilisation
    #--------------------------------------------------------------------------
    def consumable?(user, item)
    	if user && user.actor?
    		instr = item.instructions.find{|elt|elt.name == :consumable} if item.instructions
    		return true unless instr
    		return true if instr.value.include?(user.id)
    		return false
    	end
    	return true
    end

    #--------------------------------------------------------------------------
    # * Objet utilisable?
    #--------------------------------------------------------------------------
    def usable?(item, actor = false)
    	return item_usable?(item) && consumable?(actor, item)
    end
end

#==============================================================================
# ** Scene_ItemBase
#------------------------------------------------------------------------------
#  Rajoute la gestion des instructions aux objets
#==============================================================================

class Scene_ItemBase
	#--------------------------------------------------------------------------
    # * Un objet est utilisable?
    #--------------------------------------------------------------------------
    def item_usable?
    	flag = (item.for_friend?) ?  $game_party.members[@actor_window.index] : false
		return user.usable?(item, flag) && item_effects_valid?
	end
end

#==============================================================================
# ** Scene_Battler
#------------------------------------------------------------------------------
#  Rajoute la gestion des instructions aux objets
#==============================================================================

class Scene_Battler
	#--------------------------------------------------------------------------
    # * Variables d'instances
    #--------------------------------------------------------------------------
    attr_accessor :subject

end
