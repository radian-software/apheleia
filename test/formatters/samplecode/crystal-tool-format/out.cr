module SomeController
  macro included
    def hello
      puts "hello"
    end
  end # <= this indentation is wrong.
end
