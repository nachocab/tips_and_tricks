# sample from an array random
[:foo, :bar].sample # :bar
(1..10).to_a.sample 2 # [3, 10]

# debugger, don't use ruby-debug
require 'debugger'
debugger

# sort files by descending modification time
Dir['*.html'].sort_by{ |f| File.mtime(f) }.reverse -

# get the delete key working on irb: press ctrl+d in terminal preferences|keyboard|forward delete

# get duplicate elements of an array
questions.select{|e| questions.count(e) > 1 }

# allow utf8 in script
# encoding: utf-8

# better irb
pry

# get user input
gets
answer = gets

# capture control-c
begin
  ...
rescue Interrupt
  ...
end

# check if file exists
File.exist?(path)

# $: - Array of paths used for looking up external files: [/Users/nacho/.rvm/rubies/ruby-1.9.3-p194/lib/ruby/site_ruby/1.9.1, /Users/nacho/.rvm/rubies/ruby-1.9.3-p194/lib/ruby/site_ruby/1.9.1/x86_64-darwin11.3.0, /Users/nacho/.rvm/rubies/ruby-1.9.3-p194/lib/ruby/site_ruby, ...]
$:.unshift File.join(File.dirname(__FILE__), *%w[.. lib]) # prepends "./../lib" to the $: array of paths
# now you can do
require 'ruhoh' # instead of require './../lib/ruhoh', now all the files directly below this folder are accessible, and here you set up all the requirements

# require - don't need to add the .rb extension
require 'ruhoh/client/client' # instead of 'ruhoh/client/client.rb'
Ruhoh::Client.new # because it's a subfolder
# and then in ruhoh/client/client.rb
class Ruhoh
  class Client # could be a module, if you don't need to instantiate
    ...
  end
end

# check if a string matches a regex: returns position of first match or nil
"My string" =~ /My/ # 0
"My string" =~ /string/ # 3
"My string" =~ /paco/ # nil

# check if a string matches a regex: returns a MatchData object
m = "My string".match /xxx/ # nil
m = "My string".match /My/ # m[0]=>"My" m.string=>"My string" m.regexp => /My/ m.begin 0 => 0
m = "My string".match /string/ # m[0]=>"My" m.string=>"My string" m.regexp => /My/ m.begin 0 => 3
m = "My string".match /(\w+)\s(\w+)/ # m[0]=>"My string" m[1]=>"My" m[2]=>"string" m.captures=>["My", "string"] zero-based, you can also use $1 and $2 for the captured elements, m.begin 2 => 3
m = "My string".match /\s(\w+)/ # $1 => "string" m.pre_match => "My" m.post_match => ""


# open file write
File.open(local_filename, 'w') {|f| f.write(doc) }

# read entire file into array
arr = IO.readlines(path)

# read file line by line
File.open(path) do |file|
    file.each do |line|
        ...
    end
end


# format time now
Time.now.ctime # "Sun Jul 22 12:00:43 2012"
Time.now.strftime("%H:%M:%S") # "12:01:52"

# remove array element
[1,2,3] - [2]

# remove a substring from a string, sanitize
"paco.fastq".sub(/.fastq$/,'')
str.sub(/[\/]/,"_") # remove forward slash

# invert hash
class Hash
  def inverse # unix gods
    i = Hash.new
    self.each_pair{ |k,v|
      if (v.class == Array)
        v.each{ |x|
          if i.has_key?(x)
            i[x] = [k,i[x]].flatten
          else
            i[x] = k
          end
        }
      else
        if i.has_key?(v)
       i[v] = [k,i[v]].flatten
        else
          i[v] = k
        end
      end
    }
    return i
  end
end

# count frequency elements array
arr.inject(Hash.new(0)) {|h,i| h[i] += 1; h }

# sort hash by value
metrics.sort_by {|key, value| value} # ascending
metrics.sort_by {|key, value| -value} # descending

# initialize a hash with an array
h = Hash.new {|h,k| h[k] = [] }
h["a"].push(3)
h["a"].push(5)
h["b"].push(6)
# {"a"=>[3, 5], "b"=>[6]}

# puts to stderr
$stderr.puts

# exit abort error
abort "MacOS too old, see: https://gist.github.com/1144389" if macos_version < 10.5

# arguments
ARGV
ARGV[0] # first command

# current script
$0



# execute command
results = %x[ls] # same as Kernel#` (backticks)

# get all files of a given type from a directory
Dir.glob(File.join(GENE_SETS_PATH,"*.txt"))
Dir['*.html']

# get directory of current file
File.dirname(__FILE__)
File.join(File.dirname(__FILE__), '..', '/') # move up one folder

# Get filename from path
File.basename($0)

# remove file extension
File.basename(my_file,File.extname(my_file))

# prepend to an array
array.unshift 0 #=> [0, 1, 2, 3, 4]

# check if element in my array
[1,2,3].include?(2)

# ruby string interpolate
%{"paco #{pepe}"}

# map
["a","b"].map(&:to_sym)

# class level instance variables
class Polygon
    @sides = 8
    class << self
        attr_accessor :sides
        def prueba
            puts @options
        end

        def run
            @options = 3
        end
    end
end
puts Polygon.sides # => 8
Polygon.run #=> 3

require 'test_helper'

class RubyTest < ActiveSupport::TestCase
  # Glossary
  #
  # first-class entities - They can be stored in variables, they are objects, they can be
  # created with .new()
  #
  # Mix a module into a class - Make the module's instance methods available to all
  # instances of the class
  #
  # receiver - Receiver.method(argument) => Receiver is what receives the method message.
  context "Ruby" do
    should "Parallel assignment" do
      # Any assignment expression that has more than one l-value or r-value.

      a = 3
      b = [3]
      c = [3,4]
      assert_equal [3], [*a]
      assert_equal [3], [*b]
      assert_equal [3,4], [*c]
      # One l-value, multiple r-values
      x = 1,2,3
      assert_equal x, [1,2,3]

      x, = 1,2,3;
      assert_equal x, 1
      #      assert x == 1

      # Multiple l-values, one array r-value
      x,y,z = [1,2,3] # same as x,y,z = 1,2,3
      assert_equal 1, x
      assert_equal 2, y
      assert_equal 3, z

      x = [1,2]; assert_equal [1,2], x
      x, = [1,2]; assert_equal 1, x

      # different number of l- and r-values
      x,y,z = 1,2
      assert_equal 1,x
      assert_equal 2,y
      assert_equal nil,z

      x,y = 1,2,3 # 3 is not assigned
      assert_equal 1,x
      assert_equal 2,y

      # Parentheses in parallel assignment. The lefthand side can use
      #   parentheses for subassignment. If two or more l-values are enclosed in
      #   parentheses, they are initially treated as a single l-value. When the
      #   corresponding r-value has been determined, the rules of parallel
      #   assignment are applied recursively.
      x,(y,z) = 3,[4,5]
      assert_equal 3, x
      assert_equal 4, y
      assert_equal 5, z

      x,y,z = 3,[4,5]
      assert_equal 3, x
      assert_equal [4,5], y
      assert_equal nil, z

      # Parentheses with hashes
      x,(y,z) = 3,{4 => 5}
      assert_equal 3,x
      assert_equal(({4 => 5}), y)
      assert_equal nil, z

      x,(y,z) = 3, *{4 => 5}
      assert_equal 3, x
      assert_equal 4, y
      assert_equal 5, z




      # Return more than one value
      def return_two_values
        a = 2; b = 3
        [a,b]
      end
      x,y = return_two_values
      assert_equal x,2
      assert_equal y,3


      # See the splat operator for more assignments

    end

    should "unless::" do
      # Unless means: execute the LEFT unless the RIGHT evaluates to TRUE
      #
      # falsy values => The right is true, the left executes
      a = "not changed"; b = "changed"

      a = b unless false; assert_equal a,"changed"
      a = b unless [];    assert_equal a,"changed"
      a = b unless "";    assert_equal a,"changed"
      a = b unless nil;   assert_equal a,"changed"
      a = b unless {};    assert_equal a,"changed"

      # truthy values => anything else, the left doesn't execute
      a = "not changed"; b = "changed"

      a = b unless true;  assert_equal a,"not changed"
      a = b unless [3];   assert_equal a,"not changed"
    end

    should "super:: Chaining" do
      # It calls the method in the superclass with the same name, sending it the current
      # arguments. (The superclass doesn't have to define it, it could inherit it from an
      # ancestor.)
      #   super      => All arguments get passed when calling the method in the superclass
      #   super()    => No arguments get passed when calling the method in the superclass
      #   super(x,y) => x and y get passed when calling the method in the superclass

      class Point
        def initialize(x,y)
          @x = x
          @y = y
        end
      end

      class Point3D < Point
        def initialize(x,y,z)
          super(x,y) # Point3D has access to instance variables @x and @y, so @x, @y = super(x,y) is unnecessary.
          @z = z
        end
        def show
          "" << "#{@x} " << "#{@y} " << "#{@z}"
        end
      end

      p3d = Point3D.new(2,3,4)
      assert_equal "2 3 4", p3d.show

      ############
      class Parent
        def age
          30
        end
      end
      class Child < Parent
        def age
          super - 20
        end
      end
      c = Child.new
      assert_equal 10, c.age
    end

    should "ensure::" do
      # Ensure that housekeeping details like closing files, disconnecting DB connections
      #   and committing transactions get done. Use it whenever you allocate a resource
      #   (file, DB connection) to ensure proper cleanup occurs. Even if there is an
      #   exception, the ensure clause gets called.
      #
      # The ensure clause can replace the running exception with another one, or switch
      # the current return value with another one.
      def ensure_returns
        begin
          return 1
        ensure
          return 2
        end
      end
      assert_equal 2, ensure_returns
    end

    should "and::" do
      # Using this pattern wee can guarantee that the result we return will be valid, even
      #   if an unexpected error is raised
      def increase_number(number)
        number.is_a?(Fixnum) ? number += 1 : (raise ArgumentError)
      end

      def increment(number)
        begin
          return_value = increase_number(number) and return return_value  #successful
          # If the below executes, something went wrong.
        rescue ArgumentError => e
          return e.class
        end
      end

      assert_equal 3, increment(2)
      assert_equal(ArgumentError, increment("error"), "It wasn't a number")
    end

    should "for...in..." do
      @mestres = ""
      for mestre in ["bimba", "pastinha", "camisa"]
        @mestres << mestre << " "
      end
      assert_equal "bimba pastinha camisa ", @mestres
    end

    should "alias:: " do
      def existing; 3; end
      alias old_existing existing
      assert_equal 3, old_existing
    end

    should "configure ruby" do
      # gem sources -a http://gems.github.com # add sources
      # set HOME=.
      # gem install sqlite3-ruby --version 1.2.3
    end

    should "case::" do
      a = case ['an_array']
      when is_a?(Array)
        'WRONG WAY OF DOING IT'
      when Array
        'RIGHT'
      when b = ['an option hash']
        b # better than repeating ['an option hash']
      else
        'other'
      end
      assert_equal 'RIGHT', a

      b = case 'a'
      when 'a','b'
        'yes'
      end
      assert_equal 'yes', b, "You can assign the result of a case structure directly"

      empty = nil
      res = case empty
      when nil
        3
      when blank?
        # WRONG: "empty" will never equal "blank?"
      else
        4
      end
      assert_equal 3, res

      arg = "H"
      case arg
      when Array
      when String then arg.downcase!
      when :start
      when :end then -1
      when /[a-z]/
      end
      assert_equal "h", arg, "Using case with then"

      case
      when 1 == 1
        a = 3
      end
      assert_equal a, 3, "case with no argument"
    end
  end

  context "Kernel" do
    should "method_missing:: obj.method_missing(symbol [, *args] ) => result " do
      # symbol is the symbol for the method called, and args are any arguments that were
      #   passed to it
      class Roman
        def roman_to_int(str)
          case str.to_s
          when 'i' then 1
          when 'ii' then 2
          when 'iii' then 3
          end
        end
        def method_missing(method)
          roman_to_int(method)
        end
      end

      r = Roman.new
      assert_equal 3, r.iii
    end
  end

  context "Operators" do
    should "||=::" do
      def set_results(results = nil)
        results ||= []
        # same thing as: results = results || [] same thing as: results = [] if
        #   results.nil?
      end
      assert_equal [], set_results
      assert_equal [], set_results(false)
      assert_equal [1,2,3], set_results([1,2,3])
    end

    should "*:: The splat operator" do
      # Uses Array Expansion Syntax. It lets us group together all remaining parameters
      #   into a single array variable

      # 1 - ASSIGNMENT: * expands r-value arrays and hashes
      pet1, pet2, pet3 = 'duck', *['dog','cat']
      assert_equal 'duck', pet1
      assert_equal 'dog', pet2
      assert_equal 'cat', pet3

      pet1, pet2, pet3 = 'duck', *{'dog' => 'cat'}
      assert_equal 'duck', pet1
      assert_equal ['dog','cat'], pet2
      assert_equal nil, pet3


      # 2 - ASSIGNMENT: * on l-values contracts values into arrays
      pet1, *other_pets = 'duck','dog','cat'
      assert_equal 'duck', pet1
      assert_equal ['dog','cat'], other_pets


      # 3 - convert a non-nested array to hash
      vehicles = [ :planes, 21, :cars, 36 ]
      vehicles_hash = Hash[*vehicles]
      assert_equal({ :planes => 21, :cars => 36 }, vehicles_hash)
      # convert a nested array into a hash
      vehicles_nested = [[:planes, 21], [:cars, 36]] # same as vehicles_hash.to_a
      vehicles_hash_unnested = Hash[*vehicles_nested.flatten]
      assert_equal({ :planes => 21, :cars => 36 }, vehicles_hash_unnested)

      assert_equal [:planes, 21, :cars, 36], [*vehicles_nested.flatten]
      assert_equal [:planes, 21, :cars, 36], vehicles_nested.flatten #WRONG - produces an error when converting to hash

      # 4 - Convert Hash to array by exploding hash. Not a good idea. Better use to_a
      vehicles_array = *{ :planes => 21, :cars => 36 }
      assert_same_elements [ [:planes, 21], [:cars, 36] ], vehicles_array

      # 4.1 - special case, with only one element. Convert hash to array.
      h = {'paco' => 343}
      a = *h
      b = h.to_a
      assert_equal ['paco',343], a
      assert_equal [['paco',343]], b

      h = {'paco' => 343, 'luis' => 454}
      a = *h
      b = h.to_a
      assert_equal [['paco',343],['luis',454]], a
      assert_equal a, b


      # 5 - Use in a case statement - explode array
      WILD = %w( lion ñu )
      TAME = %w( cat cow )
      def determine_continent(animal)
        case animal
        when *WILD then "Africa"
        when *TAME then "Europe"
        end
      end
      assert_equal "Africa", determine_continent("ñu")

      # 6 - Array#* operates as Array#join if supplied a string. If give a number, it does
      #   repetition
      array = %w( join this sentence )
      assert_equal "join—this—sentence", ( array * '—')
      assert_equal [0,0,0,0,0], [0] * 5

      # 7 - Define methods that take an unlimited number of arguments
      def method_with_unlimited_arguments(*args)
        true
      end
      def receives_arguments_and_hash(*args)
        options = args.extract_options!
      end
      def receives_only_hash(*opts)
        opts = opts.first.merge(opts.last) # e.g.
      end
      some_ints = [1,2,3]
      assert method_with_unlimited_arguments(2)
      assert method_with_unlimited_arguments(some_ints)
      assert method_with_unlimited_arguments([1,2,3])

      # * repeats a string if followed by an integer.
      repeated = 'hola' * 2
      assert_equal "holahola", repeated
    end

    should "!!::Double negation" do
      # Used to force an object into an explicit true/false. Usually not necessary
      def is_this_true?
        @fishy_variable
      end
      @fishy_variable = nil
      assert_false @fishy_variable, "The variable is evaluated as false"
      assert_not_equal false, !@fishy_variable, "Even thought it isn't actually <false>"
      assert_equal false, !!@fishy_variable, "The double negation coerces it into <false>"

    end

    should "String#<<:: Append altering the left operand (also Array#<< and Set#<<)" do
      greeting = "A bananeira"
      greeting << " caiu"
      assert_equal "A bananeira caiu", greeting
    end

    should "%::formatting interpolation" do
      assert_equal "9.50", ("%.2f" % 9.5)

      assert_equal "<p>hello</p>", ( "<%s>%s</%s>" % %w{p hello p} )
    end

    should "<=>::" do
      # Fixnum
      assert_equal -1, 2 <=> 3
      assert_equal  0, 3 <=> 3
      assert_equal  1, 3 <=> 2

      # String
      assert_equal -1, "abc" <=> "abcd"
      assert_equal  0, "abc" <=> "abc"
      assert_equal  1, "abcd" <=> "abc"

      # Array
      assert_equal -1, [1] <=> [1,2]
      assert_equal  0, [1] <=> [1]
      assert_equal  1, [1,2] <=> [1]

    end
  end

  context "Methods & Classes" do
    setup do
      class Point
      end

    end
    should "Singleton Method::" do
      # Singleton Method - Method that is defined for only a single object rather than a
      # class of objects.
      def Point.sum
        # sum is a singleton method on an object Point
      end

      # The singleton methods of an object are instance methods of its singleton class.
      class Iuna
        def self.don ; "don"; end
        def self.yin ; "yin"; end

        def self.singleton_class
          class << self
            self
          end
        end
      end
      iuna = Iuna.new
      assert_contains iuna.class.singleton_class.instance_methods(false), "don"
      assert_contains iuna.class.singleton_class.instance_methods(false), "yin"
    end

    should "Class methods::" do
      # Class methods are singleton methods of an object of Class type
      class Iuna
        def self.don ; "don"; end
        def self.yin ; "yin"; end
      end
      assert_same_elements ["don","yin"], Iuna.singleton_methods(false)
    end

    should "define_method(meth_name, meth_body):: Defines an instance method in the receiver" do
      # It expects a Symbol as meth_name and it creates a method with that name, using the
      # associated "block" as the method body. Instead of a block, it can also be a Proc
      # or a Method object. The block is evaluated using instance_eval.
      class A
        def create_method(name, &block)
          self.class.send(:define_method, name, &block)
        end
        define_method(:paco) { 3 }
      end

      a = A.new
      assert_equal 3, a.paco

      a.create_method(:camaron) { 4 }
      assert_equal 4, a.camaron

      # you can do the same thing with class_eval
      A.class_eval { define_method(:camaron2){ 3 }}
      assert_equal 3, a.camaron2
    end

    should "Eigenclass:: or singleton class" do

      class << Point # opening the singleton class
        def class_method1
          # instance method of the eigenclass, and also class method of Point.
        end
        def class_method2
        end
      end

      # Alternative Syntax: If you open the eigenclass of a class object within the
      # definition of a class itself, you can use self instead of the name of the class
      class Point
        # instance methods go here
        class << self # opening the singleton class
          # class methods go here as instance methods of the eigenclass
          def class_method3

          end
        end
      end
    end

    should "method visibility public:: private:: protected::" do
      # methods are normally public (except the initialize method, which is private).

      # private methods are implicitely invoked on self, and may not be explicitly invoked
      # on an object.

      # public, private and protected only apply to methods. Instance & class variables
      # are encapsulated and thus, behave as private (the only way to make an instance
      # variable accessible from outside a class is defining an accessor method).
      # Constants behave as public (there is no way of making them unaccessible to outside
      # use).


      # A protected method is like a private method (it can only be invoked from within
      # the implementation of a class or its subclasses) but it may be explicitely invoked
      # on any instance of the class.
      #
      class Esquiva
        def patada(esq) esq.esquivar end

        protected
        def esquivar() "esquivando" end

      end
      esq1 = Esquiva.new
      esq2 = Esquiva.new

      assert_equal "esquivando", esq1.patada(esq2), "Calling protected method"
      assert_raise NoMethodError do
        esq1.esquivar # esquivar is protected
      end
      #      def safe_send(receiver, method, message) # regular send bypasses visibility rules
      #        eval "receiver.#{method}"  # receiver.method
      #      rescue => e
      #        puts "#{message}: #{e}"    # This is my message: NoMethodError
      #      end
      #
      #      visibility = ARGV.shift || "public"
      #
      #
    end

    should "Subclassing <:: or inheritance" do
      # Used to extend a class with the methods in another class. When we define a class,
      # we may specify that it extends—or inherits from—another class, known as the
      # superclass.
      #
      # class Gem < Ruby     #=> Gem is a subclass of Ruby. Ruby is the superclass of Gem
      # If you do not specify a superclass, the class implicitly extends Object.

      # < is a boolean operator
      assert String < Comparable, "String inherits from Comparable"

      assert_false String < Integer, "String doesn't inherit from Integer"
    end


  end

  context "Module" do
    # Modules are classes that can't be instantiated or subclassed. They can be used as
    # namespaces (to group related methods or constants) and as mixins (to include
    # instance methods in classes).
    setup do
      module InstanceMethods
        def an_instance_method; self.class ;end
      end
      module ClassMethods
        def a_class_method; self ;end
      end
    end
    should "include:: and extend::  can only be called on A MODULE " do
      # include: Adds module methods as INSTANCE METHODS.
      #
      # extend: Adds all module methods ONLY TO THE INSTANCE it's called on.
      #
      # If you want to access a method defined inside a class


      class MyClass
        include InstanceMethods
        extend ClassMethods # ClassMethods == SingletonMethods
        # If extend is called inside the class, it adds module methods as CLASS METHODS
        # (because adding instance methods to its eigenclass makes them class methods).
      end

      my_class_instance = MyClass.new
      assert_equal MyClass, my_class_instance.an_instance_method, "Instance methods are now available"
      assert_equal MyClass, MyClass.a_class_method, "Class methods are now available"

      # Using extend outside the class definition, adds the methods as singleton methods
      # (meaning, only to that instance of the class).
      obj_instance = Object.new
      obj_instance.extend InstanceMethods # Other object instances won't have access to "an_instance_method"
      assert_equal Object, obj_instance.an_instance_method, "Instance methods are now available"
    end

    should "include:: and extend:: simplified" do
      module InstanceMethodsWithExtend
        # Called everytime a module is included. Same thing with extended. base is also
        # known as klass
        def self.included(base)
          base.extend(ClassMethods)
        end
      end
      class MyClass
        include InstanceMethodsWithExtend
      end

      my_class_instance = MyClass.new
      assert_equal MyClass, my_class_instance.an_instance_method, "Instance methods are now available"
      assert_equal MyClass, MyClass.a_class_method, "Class methods are now available"
    end

    should "require::" do
      # Loads an external file. Ruby finds these files by searching in the LOAD_PATH. You
      # may need to include after requiring (if they contain modules).

      #   If a file is in the lib directory, you require it directly by name. require
      #   'easter' # lib/easter.rb
      #
      # If it's in a subdirectory of lib, you need to specify it.
      #  require 'shipping/airmail' # lib/shipping/airmail.rb
    end

    should "const_get:: mod.const_get(sym) => obj" do
      #   Returns the value of the named constant in mod.
      assert_equal 3, Math.const_get(:PI).round

      #   Leave the lefthandside empty to look for a global constant
      assert ::ARGV, "global constant"
    end

    should "class << self::" do
      class Capoeira
        def an_instance_method;end

        class << self
          def a_class_method;end
        end

        def self.another_class_method;end
      end
      assert_same_elements %w( a_class_method another_class_method), Capoeira.public_methods - Capoeira.superclass.public_methods

    end

  end

  context "Literals" do
    should "String literals" do
      #   There are four matching delimiters: [], {}, (), <>. Any other delimiter used has
      #   to be repeated: !!,||, "", '', //,++,**,:: etc.

      #   No interpolation: '' or %q
      assert_equal "paco", 'paco'
      assert_equal "paco", %q[paco]
      assert_equal "paco", %q{paco}
      assert_equal "paco", %q(paco)
      assert_equal "pa<c>o", %q<pa<c>o>, "Because they are paired, matching delimiters can be nested"
      assert_equal "pa{c}o", %q/pa{c}o/
      assert_equal "pa_co", %q_pa\_co_

      #   Interpolation "", %Q or just %
      assert_equal "4", "#{2*2}"
      assert_equal "4", %Q+#{2*2}+
      assert_equal "4", %|#{2*2}|

      #   Here documents
      multiline_no_dash = <<termina_con_esto
        Hola
        Paco
termina_con_esto
      assert_equal "        Hola\n        Paco\n", multiline_no_dash

      multiline_with_dash = <<-termina_con_esto
        Hola
        Paco
      termina_con_esto
      assert_equal "        Hola\n        Paco\n", multiline_with_dash
    end

    should "Array literals (arrays of strings)" do
      #   No interpolation %w
      assert_equal ['a', 'b', 'c'], %w[a b c]
      assert_equal ['(', '[', '{', '<'], %w| ( [ { < |
      assert_equal ['\s'], %w(\s)

      #   Interpolation %W
      assert_equal [' '], %W(\s)
      assert_equal ["\s"], %W(\s)
      a = 3
      assert_not_equal 3, %W( a ), %q(it doesn't work, you have to use #{a})
    end

    should "Symbol literals" do
      assert_equal :"hola hermano", %s(hola hermano)
    end
  end

  context "Blocks, Procs, Lambdas and Methods" do
    #   Blocks are syntactic structures; they are NOT objects, and cannot be manipulated
    #   as objects. It's possible to create an object that represents a block: a proc or a
    #   lambda.
    should "not assign a block to a variable" do
      #   my_block = { |x| x*x } #=> SyntaxError use this instead: my_block = lambda{ |x|
      #   x*x } my_block.call
    end

    should "Blocks: yield::" do
      #   IF YOU SEE A "yield", it means that the function that has it will recieve a
      #   block at runtime.
      def pass_number_as_block_argument(number)
        yield number # "number" becomes the argument of the block. number is yielded to the block
      end

      assert        pass_number_as_block_argument(4) { |x| x > 3 }
      assert_false  pass_number_as_block_argument(2) { |x| x > 3 }

      class IsTwo
        def is_two?(num)
          num == 2
        end
      end

      def foo
        yield IsTwo.new
      end

      assert_false foo { |bar| bar.is_two? 3 }
      assert       foo { |bar| bar.is_two? 2 }

      #[1,2,3].select
      #      def select
      #        ary = []
      #        each do |item|
      #          if yield(item)
      #            ary << item
      #          end
      #        end
      #        ary
      #      end

      Array.class_eval do
        def select_words1
          if block_given?
            self.select { |word, frequency| yield(word, frequency) }.collect { |word, frequency| word }
          end
        end
        def select_words2(&block)
          if block
            self.select(&block).collect { |word, frequency| word }
          end
        end
      end
      assert_equal ["a"], [["a",2],["b",1]].select_words1 { |word, freq| freq == 2 }
      assert_equal ["a"], [["a",2],["b",1]].select_words2 { |word, freq| freq == 2 }
    end

    should "a block refers to outer variables, not inner" do
      def execute_the_block_three_times_with_local_variable
        my_var = 100 #var in the context from which it is called
        yield
        yield
        yield
      end
      my_var = 5 #var in the same context as the block
      execute_the_block_three_times_with_local_variable { my_var += 1 }
      assert_equal 8, my_var
    end

    should "&:: creates procs implicitely. It converts Blocks into Procs and viceversa" do
      def convert_block_to_proc(&block)# The unary ampersand operator
        block #now it's a proc
      end

      external_proc = convert_block_to_proc { |x|  x > 3 }

      assert external_proc.call(4), "the block returns true"
      assert_false external_proc.call(1), "the block returns false"

      #   any? expects a block and we're sending it a proc
      assert_raise ArgumentError do
        [1,4,7].any? external_proc
      end

      assert [1,4,7].any?(&external_proc), "We turn the proc back into a block"
    end

    should "Proc:: use call(). proc { |...| block } => a_proc. Also, call proc with Proc#[]::" do
      #   Procs have block-like behavior, they are objects. Greatest feature => Procs can
      #   be saved in variables.
      def convert_block_to_proc_using_ampersand(&block)
        block
      end
      def convert_block_to_proc_using_proc_new(&block)
        Proc.new(&block)
      end
      proc_with_ampersand = convert_block_to_proc_using_ampersand { "&" }
      proc_with_proc_new = convert_block_to_proc_using_proc_new { "Proc.new()" }
      assert_equal "&", proc_with_ampersand.call
      assert_equal "Proc.new()", proc_with_proc_new.call

      #   Alternative way to call Proc#[]
      my_proc = lambda{ |x| x * 2 }
      assert_equal 6, my_proc.call(3)
      assert_equal 6, my_proc[3]
    end

    should "not passsing variables to blocks" do
      assert_equal [:a,:b], {:a => 3, :b => 5}.collect { |letter, _| letter }
    end

    should "Proc:: Pass a Proc as an argument" do
      #   Calling a proc is like yielding to a block
      Array.class_eval do
        def iterate!(code) # note, no &. A Proc is just a regular parameter
          self.each_with_index do |n,i|
            self[i] = code.call(n)
          end
        end
      end
      square = Proc.new do |n|
        n**2
      end
      assert_equal [1,4,9,16], [1,2,3,4].iterate!(square)
    end

    should "defined?:: checks if a variable is defined" do
      assert_false defined?(d), "d is not defined"
    end

    should "block_given?:: checks to see if a block has been sent as an argument" do
      def did_you_pass_me_a_block?()
        block_given? ? yield : "No block"
      end

      assert_equal "No block", did_you_pass_me_a_block?
      assert_equal "Yes, I did", did_you_pass_me_a_block? { 'Yes, I did' }
      proc = Proc.new { 'Kind of. I passed you a proc' }
      assert_equal "Kind of. I passed you a proc", did_you_pass_me_a_block?(&proc)
    end

    should "to_proc::" do
      #   If you send something that isn't a Proc to the & unary op. It will try to
      #   convert it into a proc by using its to_proc method, and then convert it into a
      #   block

    end

    should "Lambda:: lambda { |...| block } => a_proc | proc { ... } is a synonym" do
      #   Lambdas have method-like behavior, but they are instances of class Proc. Calling
      #   a lambda is like invoking a method.

      saved_proc_with_error = Proc.new { return 3 }

      #   Can't call a proc that returns a value
      assert_raise LocalJumpError do
        saved_proc_with_error.call
      end

      # either remove the return, or use a lambda
      saved_proc = Proc.new { 3 } # remove the return
      assert_equal 3, saved_proc.call

      saved_lambda = lambda { return 3 } # Using a lambda
      assert_equal 3, saved_lambda.call
    end

    should "Lambda and Proc differences" do
      #   1) Lambdas don't check the number of parameters passed when called. Procs do.
      checks_params = lambda { |one, two, three| "#{one} #{two} #{three.class}" }
      assert_raise ArgumentError do
        checks_params.call(1,2)
      end

      does_not_check_params = Proc.new { |one, two, three| "#{one} #{two} #{three.class}" }
      assert_equal "1 2 NilClass", does_not_check_params.call(1,2)

      #   2) Lambdas have diminutive returns. A Proc will stop a method and return the
      #   value provided, lambdas will return their value to the method and let the method
      #   continue on.
      def proc_return
        Proc.new { return "Proc.new"}.call #stop here
        return "proc_return method finished"
      end
      def lambda_return
        lambda { return "lambda"}.call
        return "lambda_return method finished" #stop here
      end

      assert_equal "Proc.new", proc_return # Procs are code snippets, not methods.
      assert_equal "lambda_return method finished", lambda_return
      #   lambdas are methods: they check the number of arguments and don't override the
      #   calling method's return
    end

    should "block-arguments::" do
      #   #block-arguments are like regular arguments. They get assigned when the lambda
      #   is called
      methods = %w( upcase! chop! )
      var = "hola"
      block = lambda { |responder| methods.each { |method| responder.send(method) } }
      block.call(var) # var becomes "responder" block-argument.
      assert_equal "HOL", var
    end

    should "Method::" do
      #   Useful if you want to pass a method as a block. It's like a lambda but without
      #   being anonymous
      Array.class_eval do
        def iterate!(code) # note, no &. A Proc is just a regular parameter
          self.each_with_index do |n,i|
            self[i] = code.call(n)
          end
        end
      end

      def square(n)
        n**2
      end
      assert_equal [1,4,9,16], [1,2,3,4].iterate!(method(:square))

    end

  end

  context "Iterators" do
    should "Array.map:: also called Enum.collect::" do
      #   Wraps the elements generated inside the block into an Array.

      assert_equal [1,4,9], [1,2,3].collect { |x| x*x }, "Array"
      assert_equal [2,4,6], (1..3).collect { |x| x*2 }, "Range"
      assert_equal [2,3,4], [1,2,3].collect(&:succ) # calls .method(:succ)

      assert_equal [[2],[2],[2]], [1,2,3].collect { |x| [2] }, "Array"
      #   DO NOT CONFUSE WITH each:: "each" returns the original array, "collect" returns
      #   the resulting array
      assert_equal [1,4,9], [1,2,3].collect { |x| x*x }, "Array"
      assert_equal [1,2,3], [1,2,3].each { |x| x*x }, "Array"

      #   Convert an array into a hash
      hash = {}
      ['a','b','c','a','b'].collect {|letter| hash[letter] ? hash[letter] += 1 : hash[letter] = 1 }
      hash #you can assign the hash directly to collect because in this case it will return 1's and 2's
      assert_same_elements( {'a' => 2, 'b' => 2, 'c' => 1}, hash )

      nested_array = [[1,:a],[2, :b],[3, :c]]
      ret = nested_array.collect { |number, letter|
        true
      }
      assert ret

      #repeated numbers
      assert_equal [1,2,1,1,2], [1,2,1,1,2].select{ |x| x }.collect{ |x| x }

    end

    should "inject::" do
      #   Invokes the associated block with two arguments. The first argument is an
      #   accumulator convert previous iterations. The second is the next element. The
      #   return value becomes the first block-argument for the next iteration (except for
      #   the last iteration). The initial value of the accumulator is either the arg to
      #   inject, or the first element of the enumerable object.
      data = [2,5,3,4]
      sum = data.inject { |sum, current_value| sum + current_value}
      assert_equal 2 + 5 + 3 + 4, sum

      multiply = data.inject(1) { |accum, current_value| accum * current_value }
      assert_equal 1 * 2 * 5 * 3 * 4, multiply

      maximum = data.inject { |max, current_value| max > current_value ? max : current_value}
      assert_equal 5, maximum

      #   injecting an empty hash converts a nested array into a hash. It's better to use
      #   hash_with_splat, but if it's nested with two or more leves it might give errors,
      #   then you might want to use flatten(1).
      nested_array = [ [1,2],[3,4],[5,6] ]
      new_hash = nested_array.inject({}) do |hash, (key, value)| #   hash,(key,value) = {},[1,2]
        hash[key] = value
        hash
      end
      hash_with_splat = Hash[*nested_array.flatten]
      assert_same_elements(({5 => 6, 1 => 2, 3 => 4}),new_hash)
      assert_same_elements(({5 => 6, 1 => 2, 3 => 4}), hash_with_splat )

      #   inject an array
      nested_hash ={%w( à ä á) => 'a', %w( é è ê ë) => 'e'}
      convert, to = nested_hash.inject(['','']) do |array,(key,value)| # array,(key,value) = ['',''], *{['à','ä','á'] => 'a'}
        array[0] << key * ''
        array[1] << value * key.size
        array
      end
      assert_equal 'àäáéèêë',convert
      assert_equal 'aaaeeee',to

    end

    should "each::" do
      hash = { :hola => 'oi', :adios => 'adeus'}
      joined_hash = ""
      hash.each { |key,value| joined_hash += (key.to_s + value)} # key, value = [key, value] parallel assignment
      assert_equal "holaoiadiosadeus".length, joined_hash.length

      #   When using Hash.each with two block-parameters, it's the same as using
      #   Hash.each_pair.
    end

    should "next::" do
      # While inside a loop, you can always skip an element
      add = 0
      [1,nil,"djd",3,4].each do |number|
        next unless number.kind_of?(Fixnum) #Skip if not a number
        add += number
      end
      assert_equal 8, add
    end


  end

  context "String:" do
    should "concatenate: <<::" do
      # #It's better to append than to build new ones
      str = ''
      a = 'Nava'
      b = 'laga'
      c = 'mella'
      assert_equal 'Navalagamella', str << a << b << c, "Better, they already existed"
      str = ''
      assert_equal 'Navalagamella', str << "#{a}" << "#{b}" << "#{c}", "Less efficient, creates new strings"
      str = ''
      assert_equal 'Navalagamella', str.concat(a + b + c)
    end

    should "match:: String#[]" do
      # Alternative to using mach. It returns the portion of the string that matches the
      # regex.
      assert_equal "ac", "paco"[/ac/]
      assert_equal "f", "asdf"[/d(.)/, 1]
    end

    should "scan::" do
      # collects all of the regular expression's matches against the string into an array.
      # If the pattern has groups, each element of the array is itself an array of
      # captured text.
      assert_equal ["a", "d"], "asdf".scan(/[a-e]/)

      assert_equal [["ruby"], ["regex"]], "hello ruby; hello regex".scan(/hello (\w+)/)
      assert_equal ["ruby", "regex"], [["ruby"], ["regex"]].flatten
    end

    should "split:: I want to break up a string everytime a delimiter appears" do
      # str.split(pattern=$;, [limit]) => array $; is the delimiter
      str = "Hola Paco. Cuanto tiempo. Ole."

      # Split at every period
      assert_equal ["Hola Paco", " Cuanto tiempo", " Ole"], str.split(".")
      assert_equal ["Hola Paco", " Cuanto tiempo", " Ole"], str.split(/\./)

      # Split at every period, removing the whitespace
      assert_equal ["Hola Paco", "Cuanto tiempo", "Ole"], str.split(/\.\s*/)

      # Split at every period, removing the whitespace and returning the delimiter
      assert_equal ["Hola Paco", ".", "Cuanto tiempo", ".", "Ole", "."], str.split(/(\.)\s*/)


      # Split at every period, removing the whitespace and returning one of the delimiters
      str = "¡Hola Paco! ¿Como andas? Ole…"
      assert_equal ["¡Hola Paco", "!", "¿Como andas", "?", "Ole", "…"], str.split(/(\.|\?|\!|\…)\s*/)


    end

    should "strip::" do
      # #remove leading and trailing whitespace
      assert_equal "hola cuñao", "   hola cuñao   ".strip
    end
  end

  context "Enumerable" do
    # Enumerable is a mixin that allows traversal, searching and sorting methods. The
    # requirements are that the class that mixes it in must have a method "each()", and in
    # some cases also the operator <=> (for max, min, sort)
    #
    # Classes that implement each: Array, Hash, Range, String
    should "each::" do
      #      (5..10).each
    end

    should "any?:: enum.any? [{|obj| block } ] => true or false; ^::" do
      # Passes each element of the collection to the given block and it returns TRUE if at
      # least ONE of the collection members is not false or nil. It expects a block. If no
      # block is passed any? returns TRUE if at least one element is not nil
      assert %w( ant bear cat ).any? {|word| word.length >= 3}

      def received_arguments?(*args)
        args.any?
      end
      assert received_arguments?(1,2,"hola")
      assert_false received_arguments?

      assert %w( ant bear cat ).any? ^ nil # ^ is xor:  TRUE  ^ FALSE => TRUE
      assert [].any? ^  "block" #                       FALSE ^ TRUE  => TRUE
      assert_false %w( ant bear cat ).any? ^ "block" #  TRUE  ^ TRUE  => FALSE
      assert_false false ^ false                     #  FALSE ^ FALSE => FALSE

    end

    should "collect:: or map::" do
      assert_equal [2,3,4], [1,2,3].collect { |x| x.succ }, "Array"
      assert_equal [2,3,4], (1..3).collect { |x| x.succ }, "Range"
      assert_equal ["ABC DEF"], "abc def".collect { |x| x.upcase }, "String"
      # Not useful with Hash. Use inject instead
    end

    should "detect::" do
      # Returns the first in enum for which block is not false. If no object matches,
      # calls ifnone and returns its result when it is specified, or returns nil

      is_nil = (1..10).detect  {|i| i % 5 == 0 and i % 7 == 0 }
      is_35 = (1..100).detect {|i| i % 5 == 0 and i % 7 == 0 }
      assert_equal nil, is_nil
      assert_equal 35, is_35
    end

    should "sort_by::" do
      #  Sorts enum using a set of keys generated by mapping the values in enum through the
      #  given block. Not very efficient.
      #
      #  Ascending order by default
      assert_equal %w( fig pear apple ), %w{ apple pear fig }.sort_by {|word| word.length}

      # Descending order
      assert_equal %w( apple pear fig ), %w{ apple pear fig }.sort_by {|word| -word.length}

    end

    should "enum_for:: AKA to_enum::" do
      # Create a new enumerator instance. You can choose between each_slice, each_cons and
      # each_with_index.
      # Useful when you want to chain an enumerator with .collect or .map
      assert_equal %w( x0 y1 z2 ),
        %w( x y z).enum_for(:each_with_index).map {|letter, index| letter + index.to_s }
      assert_raise LocalJumpError do
        %w( x y z).each_with_index.map {|letter, index| letter + index.to_s }
      end

    end

    # process array elements in groups of 4
    (1..100).to_a.each_slice(4) {|e| p e}

    should "each_slice::" do
      # Processes the array by pairs, triplets, etc
      assert_equal [2,5], [1,1,2,3].enum_for(:each_slice,2).map {|first, second| first + second}
    end

    should "each_cons::" do
      # Processes the array by pairs, triplets, etc OF CONSECUTIVE NUMBERS.
      assert_equal [2,3,5], [1,1,2,3].enum_for(:each_cons,2).map {|first, second| first + second}
    end

  end

  context "Range" do
    should "include?:: Check if an item belongs to a range or an array" do
      assert((1..100).include?(79))
      assert([1,50,79,100].include?(79))
    end
  end

  context "Set" do
    # A set is a collection of values without duplicates. The elements have no order. A
    # hash is a set of k/v pairs.

    should "to_set:: Creating Any Enumerable can be converted to a Set" do
      assert_equal Set[5,1,2,3,4], (1..5).to_set
      assert_equal Set[1,2,5], [1,2,5].to_set
      assert_equal Set[1,2,5], Set.new([1,2,5])
      assert_equal Set[2,3,4], Set.new([1,2,3]) { |x| x+1 }
    end

    should "&:: ^:: |:: -:: <<:: Set Operations" do
      primes = Set[2,3,5,7]
      odds = Set[1,3,5,7,9]

      assert_equal Set[5,7,3], primes & odds, "Intersection"
      assert_equal Set[5,7,3,1,2,9], primes | odds, "Union"
      assert_equal Set[2], primes - odds, "Difference"
      assert_equal Set[1,2,9], primes ^ odds, "Mutual Exclusion: (a|b) - (a&b) "

      assert_equal Set[2,3,5,7,11], primes << 11, "Add item"
    end
  end

  context "Object" do

    should "returning::" do
      def boo
        a = []
        a << 1
        a << 2
        a
      end
      def ole
        returning a = [] do
          a << 1
          a << 2
        end
      end
      def ole2
        returning [] do |a|
          a << 1
          a << 2
        end
      end
      def ole3
        returning [] do |a|
          [1,2].each { |i| a << i }
        end
      end

      assert_equal [1,2], boo
      assert_equal [1,2], ole
      assert_equal [1,2], ole2
      assert_equal [1,2], ole3
    end

    should "clone::" do
      # This duplication doesn't happen with numbers
      # (I think it's only classes and methods)

      arr1 = [1,2,3]
      arr2 = arr1
      arr2.pop
      assert_equal [1,2], arr1
      assert_equal [1,2], arr2

      arr1 = [1,2,3]
      arr2 = arr1.clone
      arr2.pop
      assert_equal [1,2,3], arr1
      assert_equal [1,2], arr2
    end

    should "blank?::" do
      # object is blank if it’s false, empty, or a whitespace string. This simplifies:
      # (a.nil? || a.empty?) to: if a.blank?
      assert ''.blank?
      assert ' '.blank?
      assert "".blank?
      assert " ".blank?
      assert [].blank?
      assert nil.blank?
      assert(({}).blank?)
      assert Hash[].blank?
    end

    should "==::, equal?:: and eql?::" do
      class A; end
      a = A.new; b = A.new

      # == same object (usually overriden in descendant classes)
      assert 1 == 1.0
      assert [] == []
      assert "" == ""
      assert_false a == b

      # eql? same value
      assert_false 1.eql?  1.0
      assert [].eql? []
      assert "".eql? ""
      assert_false a.eql? b # == is synonymous with eql? for Object instances.

      # equal? same object (should never be overriden in descendant classes)
      assert_false 1.equal? 1.0
      assert_false [].equal? []
      assert_false "".equal? ""
      assert_false a.equal? b

      assert a.equal? a
    end

    should "respond_to?:: obj.respond_to?(symbol, include_private=false) => true or false" do
      # Returns true if obj responds to the given method.
      assert_false 3.respond_to?(:upcase)
      assert "hola".respond_to?(:upcase)

      class A; def a; "a"; end ;end
      class B; def b; "b"; end ;end
      as_and_bs = [A.new , A.new, B.new, A.new, B.new ]
      result = as_and_bs.collect { |elem| elem.a if elem.respond_to?(:a) }.compact
      assert_equal ["a","a","a"], result

      result_without_compact = as_and_bs.collect { |elem| elem.a if elem.respond_to?(:a) }
      assert_equal ["a","a",nil,"a",nil], result_without_compact
    end

    should "send:: obj.send(symbol [, args...]) => obj" do
      # Invokes the method identified by symbol, passing it any arguments specified.
      assert_equal 4, 3.send(:succ)

      #  Send bypasses method visibility constraints, it can invoke private and protected methods.
      class A
        def private_abada; 3; end
        private :private_abada
      end
      a = A.new
      assert_raise NoMethodError do
        a.private_abada
      end
      assert_equal 3, a.send(:private_abada), "Bypassing visibility rules"
    end

    should "with_options::" do
      # provides a way to factor out redundant options on multiple method calls.
      #
      # #same as: map.default "", :action => 'index', :controller => 'post'
      ActionController::Routing::Routes.draw do |map|
        map.with_options(:controller => 'post') do |post|
          post.default "", :action => 'index'
        end
      end

      assert_equal 'index', ActionController::Routing::Routes.recognize_path("/")[:action]
    end
  end

  context "rake actions" do
    should "generate a controller" do
      # ruby script/generate haml_controller Posts open debit credit close

      "" # ruby script/generate haml_controller 'admin/credit_cards' suspend late_fee
    end
    should "generate a migration" do
      # ruby script/generate migration CreatePost ruby script/generate migration
      # AddTitleBodyToPost title:string body:text published:boolean
    end
    should "generate a model" do
      # ruby script/generate model Account ruby script/generate model post title:string
      # body:text published:boolean
    end
    should "generate a scaffold" do
      # ruby script/generate scaffold post title:string body:text published:boolean
    end
    should "destroy something generated" do
      # ruby script/destroy controller Posts ruby script/destroy migration CreatePost
      #
      # ruby script/destroy model Account
    end
  end

  context "Symbols" do
    # Symbols are inmutable strings, which means they can't be change, they must be
    # overwritten.
    should "convert symbols to strings and viceversa" do
      assert_equal "hola", :hola.to_s
      assert_equal :hola, "hola".intern
      assert_equal :hola, "hola".to_sym
      assert_equal :"hola guey", %s[hola guey]
    end

    should "Symbols should not change" do
      assert_equal "berimbau e atabaque", "berimbau" << " e atabaque"
      assert_raise NoMethodError do
        :berimbau << :" e atabaque" # Can't append a symbol
      end
    end

    should "Symbols are more memory efficient than strings" do
      assert_not_equal "hola".object_id, "hola".object_id, "Two objects in memory"
      assert_equal :hola.object_id, :hola.object_id
    end

    should "Always use simbols if you can" do
      assert 5.respond_to?("to_f"), "Ruby is casting it to a symbol, why waste memory?"
      assert 5.respond_to?(:to_f), "Better"
    end
  end

  context "Numeric" do
    #          "Numeric"
    #     "Integer"      "Float"
    # "Fixnum"   "Bignum"
    #
    should "You can separate thousands with underscores" do
      assert_equal 1_000_000, 1000000
    end

    should "Float#round::" do
      assert_equal 3, Math::PI.round
      assert_equal 3.1, Math::PI.round(1)
      assert_equal 3.14, Math::PI.round(2)
      assert_equal 3.142, Math::PI.round(3)
    end

    should "step::" do
      # num.step(limit, step ) {|i| block } => num
      # creates a seq of numbers starting at num, incremented by step until limit
      # on each call to the block
      a = ""
      1.step(10,2) { |i| a << i.to_s }
      assert_equal "13579", a
    end
  end

  context "Reflections & Bindings" do
    setup do
      class Demo
        def initialize(n)
          @secret = n
        end
        def get_binding
          return binding()
        end
      end
    end
    should "eval::" do
      # eval evaluates a ruby string
      assert_equal 7, eval("3 + 4")

      eval "def multiply(x,y); x*y; end"
      assert_equal 28, multiply(7,4)
    end
    should "eval can receive a binding to set the context" do
      # Bindings store the execution context at some particular place in the code.
      demo = Demo.new(99)
      assert_equal 99, eval("@secret", demo.get_binding)
    end
    should "eval can receive a proc to set the context" do
      #      greeter = Proc.new { |greetings| greetings.map { |greeting| "#{greeting} #{name}"}}
      #      eval( "name = 'pastinha'", greeter)
      #      assert_equal ['oi pastinha', 'aloha pastinha'], greeter.call(['oi', 'aloha'])
    end

    should "instance_eval:: " do
      #   Evaluates a string or block within the context of the receiver (obj). This is
      #   done by setting the variable self within the string or block to obj, giving the
      #   code access to obj's instance variables.
      #
      # In the version of instance_eval that takes a String, the optional second and third
      # parameters supply a filename and starting line number for compilation errors.

      class Klass
        def initialize
          @secret = 99
        end
      end
      k = Klass.new
      assert_equal 99, k.instance_eval { @secret }
      assert_equal 99, k.instance_eval("@secret")
      assert_raise NoMethodError do
        k.secret #there is no accessor for that variable
      end
    end

    should "instance_eval:: use it to define singleton methods on instances and class methods on Classes" do
      a = String.new
      a.instance_eval {
        def my_name(name)
          "My name is #{name}"
        end
      }
      assert_equal "My name is Nacho", a.my_name('Nacho') # my_name is a singleton method
      assert_raise NoMethodError do
        "paco".my_name('Nacho')
      end

      Fixnum.instance_eval "def zero; 0; end"
      assert_equal 0, Fixnum.zero # zero is a class method
    end

    should "class_eval:: AKA module_eval:: use it to define instance methods" do
      # include?::
      #   mod.class_eval(string [, filename [, lineno]]) => obj filename and lineno set
      #   the text for error messages Evaluates a string or block in the context of the
      #   receiver
      assert_equal 1, Demo.class_eval{ @@x = 1 }
      assert_equal 1, Demo.class_eval{ @@x }

      Fixnum.class_eval { def number; self; end }
      assert_equal 5, 5.number # number is an instance method of class Fixnum

      # You can dinamically use private methods
      module M; end
      assert_raise NoMethodError do
        String.include M # include is a private method
      end
      assert_nothing_raised do
        String.class_eval { include M }
      end
      assert String.include?(M)
    end
  end

  context "Introspection" do
    should "is_a?:: kind_of?:: ===:: know its type" do
      assert 5.is_a?(Integer)
      assert 5.kind_of?(Integer)
      assert Integer === 5

      assert_false 5 === Integer
    end

    should "check whether a module includes another" do
      module M1 ; end
      module M2 ; include M1 ; end
      assert M2.include?(M1)
    end

    should "An object or class should know its public and private methods" do
      assert 5.respond_to?(:"+"), "Public"
      assert Fixnum.respond_to?(:include, true), "Private"

      assert_false Fixnum.respond_to?(:include), "Private"
    end

    should "A class should know its ancestors::" do
      assert_contains Fixnum.ancestors, Integer
    end

    should "A class should know what modules has included" do
      assert_contains Fixnum.included_modules, Kernel
    end

    should "An object should find all its methods" do
      assert_contains 5.methods, "+"
    end

    should "A class should know its public, private and singleton methods" do
      assert_contains String.public_instance_methods, "upcase!"
      assert String.public_method_defined?(:upcase!)

      assert_contains String.private_instance_methods, "initialize"
      assert_contains String.singleton_methods, "method_added"
    end

    should "An object should know its instance variables, public, private methods" do
      d = Date.new
      assert_contains d.instance_variables, "@of"
      assert d.instance_variable_defined?(:@of)

      assert_equal 0, d.instance_variable_get(:@of) #remember to send text, not the variable

      assert_contains d.public_methods, "between?"
      assert_does_not_contain d.public_methods(false), "between?", "Exclude inherited methods"
      assert_contains d.private_methods, "lambda"
      assert_equal [], d.singleton_methods
    end

  end

end



class ViewsTest < ActionView::TestCase
  context "Helpers" do
    should "content_for::" do
      # Calling content_for stores a block of markup in an identifier for later use. You
      # can make subsequent calls to the stored content in other templates or the layout
      # by passing the identifier as an argument to yield.
      content_for :name do
        3
      end
      assert_equal "3", @content_for_name
    end

    should "javascript_include_tag(*sources)::" do
      # Returns an HTML script tag for each of the sources provided. You can pass 1-the
      # filename (w or w/o ext), 2-the full path (relative to your document root). You can
      # modify the html attrs of the SCRIPT tag by passing a hash as the last argument.

      assert_dom_equal('<script type="text/javascript" src="/javascripts/xmlhr.js"></script>',
        javascript_include_tag("xmlhr"), "No extension")
    end

    should "content_tag::content_tag(name, content_or_options_with_block = nil, options = nil, escape = true, &block)" do
      # Returns an HTML block tag of type name surrounding the content. Instead of passing
      # the content as an argument, you can also use a block in which case, you pass your
      # options as the second parameter
      html = content_tag(:div, content_tag(:p, "Hello world!"), :class => "strong")
      assert_equal '<div class="strong"><p>Hello world!</p></div>', html


      active_item = true
      html = content_tag(:div, "Hello World", :class => ("active" if active_item ))
      assert_equal '<div class="active">Hello World</div>', html


      # Using a block - notice collect:: is not the same as each:: - Cannot convert String into Array
      # Use the content tag as a block
      html = content_tag :div do
        ['a','b','c'].collect { |letter| content_tag(:scan, letter) }
      end
      assert_equal '<div><scan>a</scan><scan>b</scan><scan>c</scan></div>', html

      # Concatenate two content_tag blocks - use parentheses
      html = content_tag(:div)  +
        (content_tag :div do
          ['a','b','c'].collect { |letter| content_tag(:scan, letter) }
        end)
      assert_equal '<div></div><div><scan>a</scan><scan>b</scan><scan>c</scan></div>', html
    end

    should "capture::(&block)" do
      # When a Passes a variable to be evaluated as the argument for the block defined in
      # the template.
      #      def render_join(collection, join_string, &block)
      #        output = collection.collect do |item|
      #          capture(item, &block)
      #        end.join(join_string)
      #        concat(output)
      #      end
      #   <%render_join(@items,'<br />') do |item|%>
      #    <p>Item title: <%= item.title %></p>
      #   <% end %>
    end

    should "concat::" do
      # Outputs text in the view, concat "hello" is the equivalent of <%= "hello" %>. The
      # block binding is deprecated. Whatever you pass to output_buffer will get rendered.

      self.output_buffer = ""
      assert_equal "hola", concat('hola')
      assert_equal "hola", output_buffer

      def rounded_block(&block)
        concat(
          content_tag(:div) do
            content_tag(:p) + content_tag(:scan) { capture(&block) } + content_tag(:em)
          end
        )
      end
    end
  end
end


#
#\action_controller
#  |-base.rb
#      module ActionController
#        class Base
#          def url_for(options = {})
#  \routing
#     |-builder.rb
#        module ActionController
#           module Routing --> used as a namespace to define a bunch of classes. Never: 'include Routing'
#             class Routebuilder
#               def segment_for(string)
#
#module AC
#  module Routing
#    module Optimisation
#      def generate_optimisation_block(route,kind)
#      class Optimiser
#      class PositionalArguments < Optimiser
#      class PositionalArgumentsWithAdditionalParams < PositionalArguments
#      OPTIMISERS = [PositionalArguments, PositionalArgumentsWithAdditionalParams]
