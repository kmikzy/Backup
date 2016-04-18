module DB
  class << self
    attr_accessor :tables
    DB.tables = Hash.new
    def method_missing(n, *args)
      DB.tables[n] || (raise(NoMethodError, "#{n} doesn't exists"))
    end
  end
  class Table 
    class << self
      attr_accessor :fields
      def field(name, default_value = 0)
        instance_variable_set("@#{name}".to_sym, default_value)
        send(:attr_accessor, name.to_sym)
        @fields ||= Array.new
        @fields << name.to_sym
      end
      def insert(*args)
        hash = Hash[@fields.zip(args)]
        self.new(hash)
      end
    end
    def initialize(hash)
      hash.each do |key, value|
        self.instance_variable_set("@#{key}".to_sym, value)
      end
      DB.tables["#{self.class}".to_sym] ||= Array.new
      DB.tables["#{self.class}".to_sym] << self
    end
  end
end

class Truc < DB::Table
  field :truc
  field :trac
  field :trac2
  insert "lol", "lal", "lil"
end

class Truc2 < DB::Table
  field :truc
  field :trac
end

Truc.new(truc: ":trac", trac: ":truc", trac2: "lol")
Truc2.new(truc: ":trac", trac: ":truc", trac2: "lol")

p DB.Truc
